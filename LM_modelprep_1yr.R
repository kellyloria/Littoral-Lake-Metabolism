#' Preps data for littoral metabolism using stan  - for just one year of data
#
#' @description preps data using functions from LakeMetabolizer().See k.vachon.base() and k600.2.kGAS.base
#' @param lake_id in file name of .cvs of "\LittoralMetabModeling\FinalInputs\"
#' @param lake same as lake_id for now
#' @param max_d max lake depth in m
#' @param lake.area max lake area in m^2
#' @param out.time.period minutes 
#' @param tz here  "US/Pacific"
#'
#'
#' @return Returns .r file for running metabolism model 
#' @export 

##==============================================================================
## script modified from https://github.com/nrlottig/nrlmetab
## additional details https://github.com/GLEON/LakeMetabolizer/tree/main
## Created  01/31/2024 by KAL
#===============================================================================
rm(list=ls())
getwd()

## KAL's temporary path reminders: 
## setwd("/Users/kellyloria/Documents/LittoralMetabModeling")
## PC: setwd("R:/Users/kloria/Documents/LittoralMetabModeling")


## load packages
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(rstan)
library(patchwork)
library(plotly)
source("./Littoral-Lake-Metabolism/saved_fxns/helper_functions.r")

##==================================
## Get and process clean data
##==================================
lake <- "SHNS1"
lake_id <- "SHNS1"
max_d <-  501 
lake.area <- 496200000
out.time.period <- "60 min"
tz <-  "US/Pacific"

# read in clean data:
sonde <- list.files(paste("./FinalInputs/NonFiltered", sep = ""), full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

# Filter data for the year 2023 only
data <- sonde %>% filter(year == 2023)

summary(data)

# set conditions for mixing depth "z"
data <- data %>% 
  mutate(z = ifelse(z <= 0.5, 0.5, z)) %>% # can't have zero depth zmix
  mutate(z = ifelse(z >= 4, 4, z))  # in littoral zone depth zmix can not be deeper than the littoral depth

# determine data frequency obs/day
freq <- calc.freq(data$datetime) # needs to be 24

# set conditions for mixing depth "z"
data <- data %>% 
  group_by(year,yday) %>%
  mutate(obs = sum(!is.na(do))) %>%       #identify and filter records that have < 23 hrs of data 
  ungroup() %>%
  mutate(z = ifelse(z<=0.5,.5,z))%>% # can't have zero depth zmix
  mutate(z = ifelse(z>=4,4,z))  #in littoral zone depth zmix can not be deeper than the littoral depth


# determine data frequency obs/day
freq <- calc.freq(data$datetime) # needs to be 24

# use lake metabolizer fxn to calculate gas exchange:
data <- data %>% filter(obs>=(freq-(freq/24*2))) %>% #allow for 2 hours
  mutate(k600 = k.vachon.base(wnd = wspeed,lake.area = lake.area)) %>% #estimate K in m/day
  mutate(kgas = k600.2.kGAS.base(k600 = k600,temperature = wtemp,gas = "O2")) %>%  #m/d
  mutate(k = (kgas/freq)/z) %>% #convert gas to T^-1
  select(-kgas,-k600,-obs)



# Quick plot to check:
ggplot(data = data, aes(x = yday, y = do)) + 
  geom_line() + 
  facet_wrap(vars(year), scales = "free_x") +
  geom_point(aes(x = yday, y = z), col = "blue", size = 0.2)
ggplotly()


##==================================
## Prepare for data analysis
##==================================

# prepare data
sonde_prep <- data %>%
  arrange(year, yday, hour) %>%
  # for each year, create identifier for uninterrupted stretches of observations
  mutate(i = ifelse(is.na(do), 1, 0), 
         j = c(1, abs(diff(i)))) %>% 
  filter(!is.na(do)) %>%
  mutate(series = cumsum(j)) %>% 
  ungroup() %>%
  # create unique index for each series
  # remove series with fewer than 24 observations
  mutate(unique_series = year + series/length(unique(series))) %>%
  group_by(unique_series) %>%
  filter(n() >= 24) %>%  # Filter out series with fewer than 24 observations
  ungroup() %>%
  # recreate series index and make unique index for days
  # create index for observations (for joining later)
  # replace 0 par_int with smallest non-zero value
  mutate(unique_series = as.factor(unique_series) %>% as.numeric(),
         unique_day = paste(year, yday) %>% as.factor() %>% as.numeric(),
         index = 1:length(do),
         par_int = ifelse(par_int == 0, 0.00001, par_int)) %>%
  select(-i, -j) 

# return missing observations for check
sonde_check <- data %>% 
  expand(year, yday, hour) %>%
  full_join(sonde_prep) %>%
  arrange(year, yday)

ggplot(sonde_check, aes(x = datetime, y = do)) + 
  geom_point(size = 0.2) + 
  geom_line() + 
  facet_wrap(vars(year), scales = "free_x")

# Export prepared data
sonde_check %>%
  write_csv(paste("./ModelInputMeta/NF/sonde_dat_", lake, "_2023.csv", sep = ""))


##==================================
## Package data for modeling
##==================================

# define variables in environment 
o2_freq <- freq;
o2_obs <- 1000 * sonde_prep$do  # convert to mg m^-3
o2_eq <- 1000 * sonde_prep$do_eq  # convert to mg m^-3
light <- sonde_prep$par_int
temp <- sonde_prep$wtemp
wspeed <- sonde_prep$wspeed
map_days <- sonde_prep$unique_day
k <- sonde_prep$k
days_per_year <- length(unique(sonde_prep$unique_day))
obs_per_series <- length(unique(sonde_prep$unique_series)) 
obs_per_day <- length(unique(sonde_prep$unique_day)) 
z <- sonde_prep$z
n_obs <- length(o2_obs)
n_series <- length(unique(sonde_prep$unique_series)) 
n_days <- days_per_year
n_years <- 1

# export as .R
stan_rdump(c("o2_freq", "o2_obs", "o2_eq", "light", "temp", "wspeed", "map_days", "obs_per_series", "days_per_year",
             "obs_per_day", "z", "k", "n_obs", "n_series", "n_days", "n_years"),
           file = "./ModelInputs/NF/SHNS1_2023_sonde_list.R")
