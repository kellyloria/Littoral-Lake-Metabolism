#' Estimate littoral metabolism
#' 
#' @description stan model o2_model_inhibition.stan
#' @param lake in file name of .cvs of "\LittoralMetabModeling\FinalInputs\"
#' 
#' @return Returns .r file for running metabolism model 
#' @export 
#' 

##==============================================================================
## script modified from https://github.com/nrlottig/nrlmetab
## additional details https://github.com/GLEON/LakeMetabolizer/tree/main
## Created  01/31/2024 by KAL
## * NEED to fix issue with only 1 years worth of dat *
#===============================================================================


rm(list=ls())
getwd()

## KAL's temporary path reminders: 
## setwd("/Users/kellyloria/Documents/LittoralMetabModeling")
## PC: setwd("R:/Users/kloria/Documents/LittoralMetabModeling")

# load packages
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
source("./Littoral-Lake-Metabolism/stan_utility.R")

lake <- "GBNS1" # check to site
year <- c(2021,2022,2023)

# stan settings
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# read data
data <- read_rdump(paste("./ModelInputs/F/",lake,"_",min(year),"_",max(year),"_sonde_list.R",sep=""))

# set reference temperature
data$temp_ref <- c(mean(data$temp))

# call the stan based model
model <- "o2_model_inhibition_KL.stan" #Steele 2 param inhibition
model_path <- paste0("./Littoral-Lake-Metabolism/stan/",model)

# set sampler dependencies 
chains <- 6 # was 6
iter <-5000  #  test run 
warmup <-2500
adapt_delta <- 0.85
max_treedepth <- 15
thin <- 1
data$sig_b0 <- 0.01 #pmax smoothing parameter
data$sig_r <- 0.01  #respiration smoothing parameter
data$sig_i0 <- 0.2  #light saturation smoothing parameter


##==================================
## Compile and run the model
##==================================
sm <- stan_model(file = model_path)
stanfit <- sampling(sm, data = data, chains = chains, cores = chains, iter = iter, warmup = min(iter*0.5,warmup),
                    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth), 
                    seed=194838,thin = thin,save_warmup=FALSE)



##==================================
## Assess model fit
##==================================
# specific summaries
#check_n_eff(stanfit)
#check_rhat(stanfit)
check_div(stanfit)
check_treedepth(stanfit,max_treedepth)
check_energy(stanfit)


fit_summary <- summary(stanfit, probs=c(0.025,0.5,0.975))$summary %>% 
  {as_tibble(.) %>%
      mutate(var = rownames(summary(stanfit)$summary))}

##==================================
## Export model fit
##==================================
# export path
output_path <- paste0("./ModelOutput/F/")
# save model full output
saveRDS(stanfit, paste0(output_path,"/",lake,"_nusm_fit.rds"))

fit_clean <- fit_summary %>%
  rename(lower = '2.5%', middle = `50%`,upper = '97.5%')  %>%
  mutate(name = strsplit(var, "\\[|\\]|,") %>% map_chr(~.x[1]),
         index = strsplit(var, "\\[|\\]|,") %>% map_int(~as.integer(.x[2])),
         day = ifelse(name %in% c("GPP","ER","NEP","AIR","Flux","GPP_m2","ER_m2","NEP_m2","b0","r","i0","b"), 
                      index, 
                      data$map_days[index])) %>%
  select(name, index, day, middle,lower,upper)


##==================================
## Rejoin output with meta dat
##==================================
# * CHECK THIS FILE NAME 

sonde_data <- read_csv(paste("./ModelInputMeta/F/","sonde_dat_",lake,"_",min(year),"_",max(year),".csv",sep=""))
 

out <- fit_clean %>%
  filter(name %in% c("GPP","ER","NEP")) %>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP","ER","NEP"))) %>% 
  mutate(middle = ifelse(name=="ER",-middle,middle),
         lower = ifelse(name=="ER",-lower,lower),
         upper = ifelse(name=="ER",-upper,upper),
         name = factor(name, levels=c("GPP","NEP","ER")))

out2 <- fit_clean %>%
  filter(name %in% c("GPP_m2","ER_m2","NEP_m2"))%>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP_m2","ER_m2","NEP_m2"))) %>% 
  mutate(middle = ifelse(name=="ER_m2",-middle,middle),
         lower = ifelse(name=="ER_m2",-lower,lower),
         upper = ifelse(name=="ER_m2",-upper,upper),
         name = factor(name, levels=c("GPP_m2","NEP_m2","ER_m2")))

out3 <- rbind(out,out2)

##==================================
## Export model results
##==================================

write_csv(out3, paste0(output_path,"/",lake,"_","_nusm_daily_full.csv"))
write_csv(fit_clean, paste0(output_path,"/",lake,"_","_nusm_summary_clean.csv"))

##==================================
## Plot model results
##==================================
#plot primary parameters

figure_path <- paste0("./Figures/F/")

p1 <- fit_clean %>%  
  filter(name=="b0" | name == "r" | name == "i0" ) %>% 
  rename(unique_day = index) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>%
  ggplot(aes(x=yday,y=middle,color=factor(year))) + 
  geom_point(size=0.5) +
  geom_line(alpha=0.5) +
  facet_wrap(vars(name, year),ncol=3,scales="free_y") +
  theme_bw() +
  labs(y="Mean Estimated Value",color="year",x="Day of Year")
p1
# ggsave(plot = p1,filename = paste0(figure_path,"/",lake,"_","_nusm_parameter_fit.jpeg"),width=9,height=6,dpi=300)


#plot time series of estimates
p2 <- ggplot(data = out %>% drop_na(year),aes(yday, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point()+
  #geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("green","black","dodgerblue","firebrick")) +
  scale_fill_manual(values = c("dodgerblue","firebrick","black")) +
  theme_bw() +
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) +
  facet_wrap(vars(year), ncol=3)
p2
# ggsave(plot = p2,filename = paste0(figure_path,"/",lake,"_","_nusm_daily_metab.jpeg"),width=9,height=3,dpi=300)

# Evaluate gas exchange:
p3 <- fit_clean %>%  
  filter(name=="x_pred") %>% 
  rename(unique_day = index) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>%
  drop_na() %>%
  ggplot(aes(x = yday, y = middle, color = factor(year))) + 
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.3, color = "gray50") +
  geom_line(alpha = 0.5) +
  facet_wrap(vars(name, year), ncol = 3, scales = "free_y") +
  theme_bw() +
  labs(y = "Mean Estimated Value", color = "Year", x = "Day of Year", 
       title = "Predicted Oxygen Concentration (mg m^-3)")  # Title added here
p3
# ggsave(plot = p3,filename = paste0(figure_path,"/",lake,"_","_nusm_xpred_fit.jpeg"),width=9,height=3,dpi=300)



# Evaluate gas exchange:
p4 <- fit_clean %>%  
  filter(name=="nu_generated") %>% 
  rename(unique_day = index) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>%
  drop_na() %>%
  ggplot(aes(x = yday, y = middle, color = factor(year))) + 
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.3, color = "gray50") +
  geom_line(alpha = 0.5) +
  facet_wrap(vars(name, year), ncol = 3, scales = "free_y") +
  theme_bw() +
  labs(y = "Mean Estimated Value", color = "Year", x = "Day of Year", 
       title = "Scaled gas exchange as generated nu")  # Title added here

p4
# ggsave(plot = p4, filename = paste0(figure_path,"/",lake,"_","_daily_nusm_gen_pred.jpeg"),width=9,height=3,dpi=300)


##
## optional 
##

# DOUBLE check gas transfer 
# lets just do this by hand to see if its working...
# create equation terms:
mu = mean(data$o2_obs)
tau = sd(data$o2_obs)
eta = mean(data$light)
max_ik = c(700/eta) #set upper limit of ik to 700 based on 14C incubation data
min_ik = c(20/eta) # set lower limit of ik to 50 based on 14C incubation data

# Initialize vectors to store results
x_obs <- numeric(data$n_obs)
x_eq <- numeric(data$n_obs)
lambda <- numeric(data$n_obs)
nu <- numeric(data$n_obs)

# Calculate scaled variables using the loop
for (n in 1:data$n_obs) {
  x_obs[n] <- (data$o2_obs[n] - mu) / tau  # Standardization of the O2 obs value
  x_eq[n] <- (data$o2_eq[n] - mu) / tau    # Standardization of the O2 eq
  lambda[n] <- data$light[n] / eta         # Standardization of the light value
  nu[n] <- data$k[n] * (x_eq[n] - x_obs[n]) # Atmospheric exchange equation
}

# Now x_obs, x_eq, lambda, and nu contain the results

hist(nu)
summary(nu)

nu <- data$k* (x_eq - x_obs) # Atmospheric exchange equation

hist(data$k)
summary(data$k)


