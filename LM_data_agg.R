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
  mutate(datetime_rounded = round_date(datetime, "5 mins"))

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

plot <-
  ggplot(ns_DOQ, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") + facet_grid(Name~.)
plot

# # Filter non-flagged data
# ns_DOQ_f4 <- ns_DOQ %>% filter(Flag4=="NO")
# ns_DOQ_f3 <- ns_DOQ_f4 %>% filter(Flag3=="NO")
# ns_DOQ_f2 <- ns_DOQ_f3 %>% filter(Flag2=="NO")
# ns_DOQ_f1 <- ns_DOQ_f2 %>% filter(Flag1=="NO")

# For "NF" data:
ns_DOQ_f1 <- ns_DOQ %>% filter(Flag1=="NO")



plot_f1 <-
  ggplot(ns_DOQ_f1, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") + facet_grid(Name~.)
plot_f1

# Might still need to trim back data for odd values:
# zoom in plotly
library(plotly)
# 
# # Create the plot using plot_ly
# NS_plot <- plot_ly(data = ns_DOQ_f1, x = ~datetime, y = ~Dissolved_O_mg_L, type = 'scatter',  mode = 'lines',  color = ~Site) %>%
#   layout(xaxis = list(title = "Date and Time"),
#          yaxis = list(title = "DO (mgL)"))

# other weird outlier 
# for just BWNS2 - battery issue: 
filtered_data <- ns_DOQ_f1 %>%
  filter(Site != "BWNS2" & datetime < as.POSIXct("2023-05-19 22:00:00") | datetime > as.POSIXct("2023-05-20 2:00:00"))
# 
# NS_plot <- plot_ly(data = filtered_data, x = ~datetime, y = ~Dissolved_O_mg_L, type = 'scatter',  mode = 'lines',  color = ~Site) %>%
#   layout(xaxis = list(title = "Date and Time"),
#          yaxis = list(title = "DO (mgL)"))

str(filtered_data)
# odd dip on west shore Oct 18th -24th

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
  filter(datetime > as.POSIXct("2021-01-01 00:00:00"))  %>%
  mutate(shore = case_when( # create broad variable to lineup climate and DO dat
    site == "BWNS2" ~ "BW", # dat is ~4km resolution so called it from NLDAS based on center miniDOT in each array
    site == "SHNS2" ~ "SH",
    site == "SSNS2" ~ "SS",
    site == "GBNS2" ~ "GB",
    TRUE ~ as.character(site)))


##===============================
## Merge DO and in climate data
#================================
# Left join climate to NS data:
filtered_dat <- filtered_data %>%
  dplyr::select(site,Site,datetime_rounded, datetime, Temperature_deg_C, 
                Dissolved_O_mg_L, Dissolved_O_Saturation_perc)

NS_dat <- filtered_dat %>%
  left_join(clim_dat, by = c("datetime_rounded"="datetime" , "site"="shore")) 

##=====
## Temporary infill of climate data to allow for more accurate DO aggregation 
## DO obs are 15 
## light is 1 hr
## baro and windsp are every 3 hr 
## gas exchange calcs are at based on hr values 
NS_datF <- NS_dat %>%
  #dplyr::group_by(site)%>%
  fill(light, .direction = "down")%>%
  fill(windsp_ms,.direction = "down")%>%
  fill(baro_Pa,.direction = "down")%>% 
  mutate(baro= (baro_Pa*0.01)) # %>% dplyr::ungroup()

summary(NS_datF)

##===============================
## NS shore functions
#================================
# a. check oxygen saturation calculation
## take the mean each hour for the cleaned miniDOT data & calc o2_sat
DOT_df <- NS_datF %>%
    dplyr::group_by(Site, datetime = floor_date(datetime_rounded, "hour")) %>%
    dplyr::summarise(
      do.obs = mean(Dissolved_O_mg_L, na.rm = TRUE), 
      Lohman_sat = mean(Dissolved_O_Saturation_perc, na.rm = TRUE),
      wtr = mean(Temperature_deg_C, na.rm = TRUE),
      baro = mean(baro, na.rm = TRUE),
      windsp = mean(windsp_ms, na.rm = TRUE),
      par = mean(light, na.rm = TRUE)
                     ) %>%
    mutate(do_eq=o2.at.sat.base(temp = wtr, baro=baro, altitude = 1897)) %>%  # Tahoe altitude in m  = 1897 
    mutate(o2_sat=do.obs/do_eq) %>%
    mutate(wspeed = wind.scale.base(windsp, wnd.z = 10)) # height of anemometer (Units: meters)/ here NLDAS range coverage

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

DOT_df <- DOT_df%>%
  mutate(shore = case_when(
    Site == "BWNS2" ~ "BW", 
    Site == "BWNS1" ~ "BW", 
    Site == "BWNS3" ~ "BW", 
    Site == "SHNS2" ~ "SH",
    Site == "SHNS1" ~ "SH",
    Site == "SHNS3" ~ "SH",
    Site == "SSNS1" ~ "SS",
    Site == "SSNS3" ~ "SS",
    Site == "SSNS2" ~ "SS",
    Site == "GBNS1" ~ "GB",
    Site == "GBNS1" ~ "GB",
    Site == "GBNS2" ~ "GB",
  TRUE ~ as.character(Site)))
  
DOT_df1 <- left_join(DOT_df, PAR_dat, by=c("date", "shore"))

summary(DOT_df1)

DOT_df2 <- DOT_df1 %>%
  dplyr::group_by(date)%>%
  fill(Kd_fill, .direction = "down")%>%
  fill(par_int_3m, .direction = "down")%>%
  fill(depth_cor, .direction = "down")%>%
  dplyr::ungroup()

DOT_df3 <- DOT_df2 %>%
  dplyr::group_by(yday)%>%
  fill(Kd_fill, .direction = "up")%>%
  fill(baro, .direction = "up")%>%
  fill(wspeed, .direction = "up")%>%
  fill(do_eq, .direction = "up")%>%
  fill(o2_sat, .direction = "up")%>%
  dplyr::ungroup()

## columns to save 
## do	wtemp	year	yday	hour	do_eq	o2_sat	par	wspeed	z	par_int	datetime
# create an hour objective 

DOT_df4 <- DOT_df3%>%
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

