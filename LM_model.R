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

set.seed(2021)
rundate <- format(Sys.Date(), "%y%m%d")

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

lake <- "SSNS2" # check to site
year <- c(2022,2023)

# stan settings
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# read data
data <- read_rdump(paste("./ModelInputs/F/",lake,"_",min(year),"_",max(year),"_5ms_fourthlake_Offset_sonde_list.R",sep=""))

# set reference temperature
data$temp_ref <- c(mean(data$temp))
str(data)

# call the stan based model
model <- "o2_model_inhibition.stan" #Steele 2 param inhibition
model_path <- paste0("./Littoral-Lake-Metabolism/stan/",model)

# set sampler dependencies 
## intercal - -0.30 from GBNS1 sensor 
chains <- 4 # was 6
iter <-20000  #  test run 
warmup <-10000
adapt_delta <- 0.95
max_treedepth <- 15
thin <- 10
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

fit_summary <- summary(stanfit, probs=c(0.025,0.5,0.975))$summary %>% 
  {as_tibble(.) %>%
      mutate(var = rownames(summary(stanfit)$summary))}

# specific summaries
check_n_eff(stanfit)
check_rhat(stanfit)
check_div(stanfit)
check_treedepth(stanfit,max_treedepth)
check_energy(stanfit)

##==================================
## Export model fit
##==================================
# export path
output_path <- paste0("./ModelOutput/")
output_path_sum <- paste0("./ModelOutput/summary/")
output_path_fit <- paste0("./ModelOutput/fits/")


# save model full output
saveRDS(stanfit, paste0(output_path_fit,"/",lake,"_5ms_fourthlake_Offset_fit_",rundate,"_.rds"))

fit_clean <- fit_summary %>%
 dplyr:: rename(lower = '2.5%', middle = `50%`,upper = '97.5%')  %>%
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

sonde_data <- read_csv(paste("./ModelInputMeta/F/","sonde_dat_5ms_fourthlake_Offset_",lake,"_",min(year),"_",max(year),".csv",sep=""))


out <- fit_clean %>%
  filter(name %in% c("GPP","ER","NEP")) %>%
  dplyr::rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP","ER","NEP"))) %>% 
  mutate(middle = ifelse(name=="ER",-middle,middle),
         lower = ifelse(name=="ER",-lower,lower),
         upper = ifelse(name=="ER",-upper,upper),
         name = factor(name, levels=c("GPP","NEP","ER")))

out2 <- fit_clean %>%
  filter(name %in% c("GPP_m2","ER_m2","NEP_m2"))%>%
  dplyr::rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP_m2","ER_m2","NEP_m2"))) %>% 
  mutate(middle = ifelse(name=="ER_m2",-middle,middle),
         lower = ifelse(name=="ER_m2",-lower,lower),
         upper = ifelse(name=="ER_m2",-upper,upper),
         name = factor(name, levels=c("GPP_m2","NEP_m2","ER_m2")))

out3 <- rbind(out,out2)

out4 <- out3 %>%
  filter(year==2023)

##==================================
## Export model results
##==================================


write_csv(out4, paste0(output_path,"/",lake,"_","_daily_5ms_fourthlake_Offset_",rundate,".csv"))
write_csv(fit_clean, paste0(output_path_sum,"/",lake,"_","_summary_5ms_fourthlake_Offset_",rundate,".csv"))


##==================================
## Plot model results
##==================================
#plot primary parameters

figure_path <- paste0("./ModelOutput/figures/")

p1 <- fit_clean %>%  
  filter(name=="b0" | name == "r" | name == "i0") %>% 
  dplyr::rename(unique_day = index) %>% 
  left_join(sonde_data %>% filter(year==2023)%>%select(unique_day,yday,year) %>% distinct()) %>%  drop_na(year)%>%
  ggplot(aes(x=yday,y=middle,color=factor(year))) + 
  geom_point(size=0.5) +
  geom_line(alpha=0.5) +
  facet_wrap(vars(name, year),ncol=3,scales="free_y") +
  theme_bw() +
  labs(y="Mean Estimated Value",color="year",x="Day of Year") +
  coord_cartesian(ylim = c(0, 0.6)) 
  
p1
# 
# ggsave(plot = p1,filename = paste0(figure_path,"/",lake,"_","_parameter_fit_5ms_fourthlake_Offset_",rundate,".jpeg"),width=9,height=6,dpi=300)


#plot time series of estimates
p2 <- ggplot(data = out4 %>% filter(name=="GPP" | name == "NEP" | name == "ER") %>% drop_na(year),aes(yday, middle, color = name)) +
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

# ggsave(plot = p2,filename = paste0(figure_path,"/",lake,"_","_daily_metab_5ms_fourthlake_Offset_",rundate,".jpeg"),width=9,height=3,dpi=300)


###
###
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
# ggsave(plot = p3, filename = paste0(figure_path,"/",lake,"_","_daily_o2_pred.jpeg"),width=9,height=3,dpi=300)

