#' Preps data for littoral metabolism using stan 
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

## Prepping 1 site at a time for added caution
##==================================
## Get and process clean data
##==================================
lake <- "GBNS1"
lake_id <- "GBNS1"
max_d <-  160  #/3
lake.area <- 165400000 # /3
out.time.period <- "60 min"
tz <-  "US/Pacific"

# read in clean data:
sonde = list.files(paste("./FinalInputs/Filtered/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()
if(lake == "lake_id") sonde <- sonde %>% drop_na(datetime)
unique(sonde$year)

# Adjust to just site 
sonde <- sonde %>% filter(Site %in% lake)

# Adjust data frame to relevant years
years = c(2021,2022,2023)
data <- sonde %>% filter(year %in% years)
summary(data)

## Filter out wind speeds greater than 5 
data <- data %>% filter(wspeed<=5)

# set conditions for mixing depth "z"
data <- data %>% 
  group_by(year,yday) %>%
  mutate(obs = sum(!is.na(do))) %>%       #identify and filter records that have < 23 hrs of data 
  ungroup() #%>% mutate(z = ifelse(z<=0.5,.5,z))%>% # can't have zero depth zmix
 # mutate(z = ifelse(z>=4.5,4.5,z))  #in littoral zone depth zmix can not be deeper than the littoral depth

# determine data frequency obs/day
freq <- calc.freq(data$datetime) # needs to be 24

# use lake metabolizer fxn to calculate gas exchange:
data <- data %>% filter(obs>=(freq-(freq/24*2))) %>% #allow for 2 hours
  mutate(k600 = k.vachon.base(wnd = wspeed,lake.area = lake.area)) %>% #estimate K in m/day
  mutate(kgas = k600.2.kGAS.base(k600 = k600,temperature = wtemp,gas = "O2")) %>%  #m/d
  mutate(k = (kgas/freq)/z) %>% #convert gas to T^-1
  select(-kgas,-k600,-obs)

# quick plot to check: 
ggplot(data=data,aes(x=yday,y=do)) + geom_line() + facet_wrap(vars(year),scales="free_x") +
  geom_point(aes(x=yday,y=z),col="blue",size=0.2)


# quick check of DO and gas exchange K:
ggplot(data=data) + facet_wrap(vars(year),scales="free_x") +
  geom_point(aes(x=do,y=k),col="blue",size=0.2)
# ggplotly() # optional call for zoom in 


##==================================
## Prepare for data analysis
##==================================

# prepare data
sonde_prep = data %>%
  arrange(year, yday, hour) %>%
  # for each year, create identifier for uninterrupted stretches of observations
  group_by(year) %>%
  mutate(i = ifelse(is.na(do)==T, 1, 0), 
         j = c(1,abs(diff(i)))) %>% 
  filter(is.na(do)==F) %>%
  mutate(series = cumsum(j)) %>% 
  ungroup() %>%
  # create unique index for each series
  # remove series with fewer than 24 observations
  mutate(unique_series = year + series/length(unique(series))) %>%
  group_by(unique_series) %>%
  mutate(series_length = length(unique_series)) %>%
  ungroup() %>%
  # recreate series index and make unique index for days
  # create index for observations (for joining later)
  # replace 0 par_int with smallest non-zero value
  mutate(unique_series = as.factor(unique_series) %>% as.numeric(),
         unique_day = paste(year, yday) %>% as.factor() %>% as.numeric(),
         index = 1:length(do),
         par_int = ifelse(par_int==0,0.00001, par_int)) %>%
  select(-i, -j) 

# return missing observations for check
sonde_check = data %>% 
  expand(year,yday,hour) %>%
  full_join(sonde_prep) %>%
  arrange(year,yday)

ggplot(sonde_check,aes(x=datetime,y=do)) + geom_point(size=0.2) + geom_line() + facet_wrap(vars(year),scales="free_x")

# export prepared data
if(length(years) == 1) {
  sonde_check %>%
    write_csv(paste("./ModelInputMeta/F/sonde_dat_",lake,"_",years,".csv",sep =""))
} else {
  sonde_check %>%
    write_csv(paste("./ModelInputMeta/F/sonde_dat_",lake,"_",min(years),"_",max(years),".csv",sep =""))
}


##==================================
## Package data for modeling
##==================================


# define variables in environment 
o2_freq = freq;
o2_obs = 1000*sonde_prep$do # convert to mg m^-3
o2_eq = 1000*sonde_prep$do_eq # convert to mg m^-3
light = sonde_prep$par_int
temp = sonde_prep$wtemp
wspeed = sonde_prep$wspeed
# sch_conv = sonde_prep$sch_conv
map_days = sonde_prep$unique_day
k = sonde_prep$k
if(length(years) == 1) {
  days_per_year = array(c({sonde_prep %>%
      group_by(year) %>%
      summarize(value = length(unique(unique_day)))}$value), dim = 1) #,dim = 1
} else {
  days_per_year = array(c({sonde_prep %>%
      group_by(year) %>%
      summarize(value = length(unique(unique_day)))}$value)) #,dim = 1 
}
obs_per_series = c({sonde_prep %>%
    group_by(unique_series) %>%
    summarize(value = length(unique_series))}$value) 
obs_per_day = c({sonde_prep %>%
    group_by(unique_day) %>%
    summarize(value = length(unique_day))}$value) 
z = sonde_prep$z
n_obs = length(o2_obs)
n_series = length(obs_per_series) 
n_days = sum(days_per_year)
n_years = length(days_per_year)

# export as .R
if(length(years)>1) {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("./ModelInputs/F/",lake,"_",min(years),"_",max(years),"_sonde_list.R",sep=""))
} else {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("./ModelInputs/F/",lake,"_",years,"_sonde_list.R",sep=""))
}


