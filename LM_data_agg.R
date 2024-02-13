#' Aggregates and cleans miniDOT data for eventual littoral metabolism modeling 
#' @description brings in climate data from NLDAS, light data from sensors and attenuation data from TERC
#'
#' @return Returns .csvs named by site for calculating metab fxns in "LM_modelprep" 
#' @export 

##===============================================================================
## Created  01/31/2024 by KAL
## look to https://github.com/nrlottig/nrlmetab for help with metab fxns
#===============================================================================

## KAL's temporary path reminders: 
## setwd("/Users/kellyloria/Documents/LittoralMetabModeling")
## PC: setwd("R:/Users/kloria/Documents/LittoralMetabModeling")

# install.packages("remotes")
# remotes::install_github("nrlottig/nrlmetab")

lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "tidyverse","data.table","xts","dygraphs",
         "nrlmetab","cowplot"), require, character.only=T)

source("./Littoral-Lake-Metabolism/saved_fxns/LM.o2.at.sat.R")
source("./Littoral-Lake-Metabolism/saved_fxns/LM.wind.scale.R")

##==========================
## Read in DO data
#===========================
ns_DO <- readRDS("./RawData/NS_miniDOT/flagged_all_100423.rds")
str(ns_DO)
ns_DO <- ns_DO %>% 
  mutate(datetime = as.POSIXct(Pacific_Standard_Time, format ="%Y-%m-%d %H:%M:%S")) %>%
  with_tz(tz = "America/Los_Angeles") %>%
  mutate(datetime = round_date(datetime, "5 mins"))

# create matching column to line up data by site:
ns_DO$Site <- paste(ns_DO$site, ns_DO$location, ns_DO$replicate, sep = "_")
unique(ns_DO$Site)
ns_DO$Site <- gsub("_3m_", "", ns_DO$Site)

# select just the NS sites:
ns_DOQ <- ns_DO%>%
  filter(Site=="BWNS1" | Site=="BWNS2" |Site=="BWNS3"|
           Site=="SSNS1" |   Site=="SSNS2" |   Site=="SSNS3" | 
           Site=="SHNS1" | Site=="SHNS2" | Site=="SHNS3" |
           Site=="GBNS1" |Site=="GBNS2" |Site=="GBNS3")
unique(ns_DOQ$Site)

# Look at flags:
# Flag 1 - "YES" indicates day of deployment or removal
# Flag 2 - "YES" indicates poor miniDOT instrument function
# Flag 3 - "YES" indicates poor miniwiper instrument function
# Flag 4 - "YES" indicates suspected biofouling due to any of the above, photos, or general DO trends

# # Filter non-flagged data
# ns_DOQ_f4 <- ns_DOQ %>% filter(Flag4=="NO")
# ns_DOQ_f3 <- ns_DOQ_f4 %>% filter(Flag3=="NO")
# ns_DOQ_f2 <- ns_DOQ_f3 %>% filter(Flag2=="NO")
# ns_DOQ_f1 <- ns_DOQ_f2 %>% filter(Flag1=="NO")

# For "NF" data:
ns_DOQ_f1 <- ns_DOQ %>% filter(Flag1=="NO")
ns_DOQ_f2 <- ns_DOQ_f1 %>% filter(Flag2=="NO")


plot_f2 <-
  ggplot(ns_DOQ_f2, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") + facet_grid(Name~.)
plot_f2

# Might still need to trim back data for odd values:
# zoom in plotly
library(plotly)

# select just the NS sites:
ns_DOQ<- as.data.frame(ns_DOQ_f2)
str(ns_DOQ)
summary(ns_DOQ)
range(ns_DOQ$datetime)

# 
# Create the plot using plot_ly
NS_plot <- plot_ly(data = ns_DOQ, x = ~datetime, y = ~Dissolved_O_mg_L, type = 'scatter',  mode = 'markers',  color = ~Site) %>%
  layout(xaxis = list(title = "Date and Time"),
         yaxis = list(title = "DO (mgL)"))

# other weird outlier 
# for just BWNS2 - battery issue: 
## BWNS1:
filtered_bwns1 <- ns_DOQ %>%
  filter(Site == "BWNS1")  %>%
  filter(datetime < as.POSIXct("2022-10-17 02:00:00") | datetime > as.POSIXct("2022-10-25 02:00:00"))%>%
  filter(datetime < as.POSIXct("2022-11-01 00:00:00") | datetime > as.POSIXct("2022-11-10 00:00:00"))%>%
  filter(datetime < as.POSIXct("2023-05-26 00:00:00") | datetime > as.POSIXct("2023-06-02 00:00:00"))
  

## BWNS2:
filtered_bwns2 <- ns_DOQ %>%
  filter(Site == "BWNS2")  %>%
  filter(datetime < as.POSIXct("2022-05-24 02:00:00") | datetime > as.POSIXct("2022-09-07 12:00:00")) %>%
  filter(datetime < as.POSIXct("2022-10-17 02:00:00") | datetime > as.POSIXct("2022-10-25 02:00:00")) %>%
  filter(datetime < as.POSIXct("2023-05-19 22:00:00") | datetime > as.POSIXct("2023-05-20 04:00:00"))

## BWNS3
filtered_bwns3 <- ns_DOQ %>%
  filter(Site == "BWNS3")  %>%
  filter(datetime < as.POSIXct("2022-10-17 02:00:00") | datetime > as.POSIXct("2022-10-25 02:00:00")) %>%
  filter(datetime < as.POSIXct("2023-03-15 10:00:00") | datetime > as.POSIXct("2023-05-25 02:00:00"))

# GBNS1
filtered_gbns1 <- ns_DOQ %>%
  filter(Site == "GBNS1")  %>%
  filter(datetime < as.POSIXct("2022-03-11 20:00:00") | datetime > as.POSIXct("2022-05-24 02:00:00")) %>%
  filter(datetime < as.POSIXct("2022-12-08 20:00:00") | datetime > as.POSIXct("2023-02-05 02:00:00")) %>%
  filter(datetime < as.POSIXct("2023-07-19 20:00:00") | datetime > as.POSIXct("2023-07-20 23:00:00"))

##GBNS2
filtered_gbns2 <- ns_DOQ %>%
  filter(Site == "GBNS2")  %>%
  filter(datetime < as.POSIXct("2022-04-18 20:00:00") | datetime > as.POSIXct("2022-05-06 00:00:00")) %>%
  filter(datetime < as.POSIXct("2022-04-18 20:00:00") | datetime > as.POSIXct("2022-05-06 00:00:00")) %>%
  filter(datetime < as.POSIXct("2023-04-02 20:00:00") | datetime > as.POSIXct("2023-05-26 00:00:00"))

## GBNS3
filtered_gbns3 <- ns_DOQ %>%
  filter(Site == "GBNS3")  %>%
  filter(datetime < as.POSIXct("2023-03-10 20:00:00") | datetime > as.POSIXct("2023-05-26 00:00:00"))

## SHNS1
filtered_shns1 <- ns_DOQ %>%
  filter(Site == "SHNS1")  %>%
  filter(datetime < as.POSIXct("2023-02-21 20:00:00") | datetime > as.POSIXct("2023-05-26 00:00:00"))

## SHNS2
filtered_shns2 <- ns_DOQ %>%
  filter(Site == "SHNS2")  %>%
  filter(datetime < as.POSIXct("2023-05-02 20:00:00") | datetime > as.POSIXct("2023-05-04 20:00:00"))

## SHNS3
filtered_shns3 <- ns_DOQ %>%
  filter(Site == "SHNS3")  %>%
  filter(datetime < as.POSIXct("2023-07-25 00:00:00") | datetime > as.POSIXct("2023-07-27 00:00:00")) 

## SSNS1
filtered_SSNS1 <- ns_DOQ %>%
  filter(Site == "SSNS1")  %>%
  filter(datetime < as.POSIXct("2022-10-17 02:00:00") | datetime > as.POSIXct("2022-10-25 02:00:00"))

filtered_SSNS2 <- ns_DOQ %>%
  filter(Site == "SSNS2")  


Filterdat <- rbind(filtered_bwns1, filtered_bwns2, filtered_bwns3, 
                   filtered_gbns1, filtered_gbns2, filtered_gbns3,
                   filtered_shns1, filtered_shns2, filtered_shns3,
                   filtered_SSNS1, filtered_SSNS2)


NS_plot <- plot_ly(data = Filterdat, x = ~datetime, y = ~Dissolved_O_mg_L, type = 'scatter',  mode = 'markers',  color = ~Site) %>%
  layout(xaxis = list(title = "Date and Time"),
         yaxis = list(title = "DO (mgL)"))

str(Filterdat)
Filterdat <- Filterdat%>%
  mutate(shore = case_when( # create broad variable to lineup climate and DO dat
    site == "BWNS2" ~ "BW", 
    site == "BWNS1" ~ "BW", 
    site == "BWNS3" ~ "BW", 
    site == "SHNS2" ~ "SH",
    site == "SHNS1" ~ "SH",
    site == "SHNS3" ~ "SH",
    site == "SSNS2" ~ "SS",
    site == "SSNS1" ~ "SS",
    site == "GBNS2" ~ "GB",
    site == "GBNS1" ~ "GB",
    site == "GBNS3" ~ "GB",
    TRUE ~ as.character(site)))
  
##==========================
## Read in climate data
#===========================
baro_dat <- read.csv("./RawData/NLDAS/processed_baro/nearshore_NLDAS_baro.csv") %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
  with_tz(tz = "America/Los_Angeles") %>%
  select(site, datetime, baro_Pa)

light_dat <- read.csv("./RawData/NLDAS/processed_light/nearshore_NLDAS_light.csv") %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
  with_tz(tz = "America/Los_Angeles") %>%
  dplyr::select(site, datetime, light)

wind_dat <- read.csv("./RawData/NLDAS/processed_windsp/nearshore_NLDAS_windsp.csv") %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
  with_tz(tz = "America/Los_Angeles") %>%
  dplyr::select(site, datetime, windsp_ms)

##====
## Merge climate datasets and change some grouping variables 
## final data selection: ##
clim_dat <- light_dat %>%
  left_join(wind_dat, by = c("datetime", "site")) %>%
  left_join(baro_dat, by = c("datetime", "site")) %>%
  filter(datetime > as.POSIXct("2021-06-01 00:00:00"))  %>%
  mutate(shore = case_when( # create broad variable to lineup climate and DO dat
    site == "BWNS2" ~ "BW", # dat is ~4km resolution so called it from NLDAS based on center miniDOT in each array
    site == "SHNS2" ~ "SH",
    site == "SSNS2" ~ "SS",
    site == "GBNS2" ~ "GB",
    TRUE ~ as.character(site)))

unique(clim_dat$shore)

clim_dat<- clim_dat %>%
  dplyr::group_by(shore)%>%
  fill(windsp_ms,.direction = "down")%>%
  fill(baro_Pa,.direction = "down")%>% 
  mutate(baro= (baro_Pa*0.01),
         par= (light* 2.114)) %>% dplyr::ungroup()

summary(clim_dat)
str(clim_dat)

####
## STOP MAKE SURE DATA IS MERGING AND INFILLING
## BW looks better nor but we maybe just make sure 
##


##===============================
## Merge DO and in climate data
#================================
# Left join climate to NS data:
filtered_dat <- Filterdat %>%
  dplyr::select(shore,site,Site, datetime, Temperature_deg_C, 
                Dissolved_O_mg_L, Dissolved_O_Saturation_perc)
str(filtered_dat)

filtered_datH <- filtered_dat %>%
  dplyr::group_by(Site, shore, datetime = floor_date(datetime, "hour")) %>%
  dplyr::summarise(do.obs = mean(Dissolved_O_mg_L, na.rm = TRUE),
                   wtr = mean(Temperature_deg_C, na.rm = TRUE)) 

NS_dat <- filtered_datH %>%
  left_join(clim_dat, by = c("shore", "datetime"))

##=====
## Temporary infill of climate data to allow for more accurate DO aggregation 
## DO obs are 15 
## light is 1 hr
## baro and windsp are every 3 hr 
## gas exchange calcs are at based on hr values 

summary(NS_dat)

##===============================
## NS shore functions
#================================
# a. check oxygen saturation calculation
## take the mean each hour for the cleaned miniDOT data & calc o2_sat
DOT_df <- NS_dat %>%
    mutate(do_eq=o2.at.sat.base(temp = wtr, baro=baro, altitude = 1897)) %>%  # Tahoe altitude in m  = 1897 
    mutate(o2_sat=do.obs/do_eq) %>%
    mutate(wspeed = wind.scale.base(windsp_ms, wnd.z = 10)) # height of anemometer (Units: meters)/ here NLDAS range coverage

##==============================================
## Attenuation estimations (and extrapolations)
#===============================================
# read in light dat:
PAR_dat <- read.csv("./RawData/benthic_light/PAR_calc_dat.csv") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(shore, date, Kd_fill, PAR_ave1, depth_cor, par_int_3m)

DOT_df$year <- lubridate::year(DOT_df$datetime)
DOT_df$yday <- lubridate::yday(DOT_df$datetime)
DOT_df$hour <- lubridate::hour(DOT_df$datetime)
DOT_df$date <- as.Date(DOT_df$datetime)
  
DOT_df1 <- left_join(DOT_df, PAR_dat, by=c("date", "shore"))

summary(DOT_df1)
## columns to save 
## do	wtemp	year	yday	hour	do_eq	o2_sat	par	wspeed	z	par_int	datetime

DOT_df4 <- DOT_df1%>%
  dplyr::rename(do=do.obs, wtemp= wtr, par_int = par_int_3m)


# add in accurate depth changes based on 3m initial placement:
DOT_df4$z<- c(3+DOT_df4$depth_cor)

summary(DOT_df4)

names(DOT_df4)
DOT_df5 <- DOT_df4 %>%
  dplyr::select(Site, do, wtemp, year, yday, hour, do_eq, o2_sat, par, wspeed, z, par_int, datetime)

summary(DOT_df5)

##===============================
## Export and save data:
#================================


## separate out downloads by site:
Export_csvs <- function(data, outputPath = "./") {
  # Ensure the outputPath ends with a "/"
  if (substring(outputPath, nchar(outputPath), nchar(outputPath)) != "/") {
    outputPath <- paste0(outputPath, "/")
  }
  
  # Split data based on unique "Site" values
  siteDataList <- split(data, data$Site)
  
  # Create and export CSV for each site
  for (siteName in names(siteDataList)) {
    siteData <- siteDataList[[siteName]]
    fileName <- paste0(outputPath, siteName, "_data.csv")
    write.csv(siteData, file = fileName, row.names = FALSE)
    cat("CSV exported for Site:", siteName, "- File:", fileName, "\n")
  }
}


# save
# Export_csvs(DOT_df5, outputPath = "./FinalInputs/NonFiltered/")

