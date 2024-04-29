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
Filterdat <- readRDS("./RawData/NS_miniDOT/24_NS_metab_F_DO.rds") %>% 
  mutate(date = as.Date(datetime)) 
str(Filterdat)

Filterdat <- as.data.frame(Filterdat)
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

## Add in some more joining date parameters
DOT_df$year <- lubridate::year(DOT_df$datetime)
DOT_df$yday <- lubridate::yday(DOT_df$datetime)
DOT_df$hour <- lubridate::hour(DOT_df$datetime)
DOT_df$date <- as.Date(DOT_df$datetime)


##==============================================
## Attenuation estimations (and extrapolations)
##===============================================
## read in light dat:
PAR_dat <- readRDS("./RawData/benthic_light/PAR_calc_dat.rds") %>%
  dplyr::select(shore, date, Kd_fill, PAR_ave1, sensor_depth, par_int)

PAR_dat <- as.data.frame(PAR_dat)

## join DO and benthic par data:
DOT_df1 <- left_join(DOT_df, PAR_dat, by=c("date", "shore"))

## quick check 
PAR_int_plot_3m <- ggplot(DOT_df1, aes(x = date, y = par_int, color=shore)) +
  geom_point(alpha = 0.75) +  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme_bw() + theme(legend.position = "bottom") 
PAR_int_plot_3m

summary(DOT_df1)

## infill the random NA
DOT_df2<- DOT_df1 %>%
  dplyr::group_by(shore)%>%
  fill(Kd_fill,.direction = "down")%>%
  fill(PAR_ave1,.direction = "down")%>% 
  fill(sensor_depth,.direction = "down")%>% 
  fill(par_int,.direction = "down")%>% 
  dplyr::ungroup()

summary(DOT_df2)
## rotate in infilled variables to check 
DOT_df2_plt <- ggplot(DOT_df2, aes(x = date, y = sensor_depth, color=shore)) +
  geom_point(alpha = 0.75) +  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme_bw() + theme(legend.position = "bottom") 




##================================================
## add in real z for sensor depth "real_NS_depth"
##================================================
depth_dat <- readRDS("./RawData/RBR\ profiles/24NS_depth_dat.rds") %>%
  dplyr::rename(real_NS_depth = sensor_depth) %>%
  dplyr::select(shore, Site, date, real_NS_depth) 

depth_dat <- as.data.frame(depth_dat)
str(depth_dat)
names(depth_dat)
names(DOT_df2)
str(DOT_df2)

DOT_df2<- as.data.frame(DOT_df2)

# Remove duplicates from depth_dat
depth_dat_unique <- distinct(depth_dat, Site, date, shore, .keep_all = TRUE)

# Perform the left join
DOT_df3 <- left_join(DOT_df2, depth_dat_unique, by = c("Site", "date", "shore"))


DOT_df2_plt <- ggplot(DOT_df3, aes(x = date, y = real_NS_depth, color=shore)) +
  geom_point(alpha = 0.75) +  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme_bw() + theme(legend.position = "bottom") 

summary(DOT_df3)

# infill some missing values 
DOT_df4<- DOT_df3 %>%
  dplyr::group_by(Site)%>%
  fill(real_NS_depth,.direction = "down")%>%
  dplyr::ungroup()

##===================
## columns to save 
##===================
## columns to save 
## do	wtemp	year	yday	hour	do_eq	o2_sat	par	wspeed	z	par_int	datetime

DOT_df5 <- DOT_df4%>%
  dplyr::rename(do=do.obs, wtemp= wtr, par_int = par_int, z=real_NS_depth)

summary(DOT_df5)

# select relevant columns:
DOT_df6 <- DOT_df5 %>%
  dplyr::select(Site, do, wtemp, year, yday, hour, do_eq, o2_sat, par, wspeed, z, par_int, datetime)

summary(DOT_df6)

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
# Export_csvs(DOT_df6, outputPath = "./FinalInputs/Filtered")

summary(DOT_df6)