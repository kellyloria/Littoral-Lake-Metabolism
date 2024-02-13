#' Visualize littoral metabolism estimates 
#' 
#' @description figures of 2024 littoral metabolism estimates 
#' @param 
#' 
#' @return 
#' @export 
#' 

##==============================================================================
## Created  02/11/2024 by KAL
#===============================================================================

library(tidyverse)
library(lubridate)
# plotting packages:
library(ggplot2)
library(cowplot)
library(reshape2)
library(scales)
library(gridExtra)
library(ggpubr)
library(stringr)

## KAL's temporary path reminders: 
## setwd("/Users/kellyloria/Documents/LittoralMetabModeling")
## PC: setwd("R:/Users/kloria/Documents/LittoralMetabModeling")
getwd()
# 24_LM_figures 

##==============================================================================
## Function to add a column based on the file name to a data frame
##==============================================================================
add_site_column <- function(file_name) {
  df <- read.csv(file_name)
  # Extract information from the file name:
  site_info <- str_match(file_name, "([A-Za-z0-9_]+)__daily_full.csv")
  # Check if there is a match and extract the relevant part from the match
  if (!is.null(site_info) && length(site_info) == 2) {
    site_name <- site_info[2]
  } else {
    site_name <- "UnknownSite"
  }
  # Add a new column to the data frame with the extracted site name
  df$site <- site_name
  return(df)
}

##============================================
## Start with non filtered modeled data:
##============================================
# Specify the folder path:
folder_path <- "./24_plotdata/NotFiltered"
# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "__daily_full.csv$", full.names = TRUE)
# Apply the function to each CSV file and store the results in a list
processed_data_list <- lapply(csv_files, add_site_column)
# Combine the list of data frames into a single data frame
LM_data <- do.call(rbind, processed_data_list)

## subset data:
LM_data <-LM_data %>% 
  subset(name=="ER" | name=="GPP"|name=="NEP") #%>% subset(middle <= 40 & middle >= -40)

# add in column for date:
LM_data$origin <- as.Date(paste0(LM_data$year, "-01-01"),) 
LM_data$date <-as.Date(LM_data$yday, origin = LM_data$origin) 

## Plotting the "Not filtered model output"
## quick plot of all data in long form: 
p2 <- ggplot(data = LM_data %>% drop_na(year),aes(date, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point(alpha = 0.6)+
  scale_color_manual(values = c("#982649", "#003F91", "#333333")) +
  scale_fill_manual(values = c("#982649","#003F91","black")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y", 
               limits=c(as.Date("2021-06-10"), as.Date("2023-09-07")))+ 
  #ylim(-35, 35) +
  labs(y=expression(mmol~O[2]~m^-3~d^-1), x=NULL) + 
  theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  facet_grid((site~.))
p2
# ggsave(plot = p2, filename = paste("./24_LM_figures/24_all_NF.png",sep=""),width=10.5,height=12,dpi=300)


## Subset for expected values |35|
LM_data1 <-LM_data %>% subset(middle <= 35 & middle >= -35)

# quick plot of all data in long form: 
p3 <- ggplot(data = LM_data1 %>% drop_na(year),aes(date, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point(alpha = 0.6)+
  scale_color_manual(values = c("#982649", "#003F91", "#333333")) +
  scale_fill_manual(values = c("#982649","#003F91","black")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y", 
               limits=c(as.Date("2021-06-10"), as.Date("2023-09-07")))+ 
  ylim(-50, 50) +
  labs(y=expression(mmol~O[2]~m^-3~d^-1), x=NULL) + 
  theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  facet_grid((site~.))
p3
# ggsave(plot = p3, filename = paste("./24_LM_figures/24_all_NF_subset.png",sep=""),width=10.5,height=12,dpi=300)


##============================================
## Check out Filtered modeled data:
##============================================
# Specify the folder path:
folder_path <- "./24_plotdata/Filtered"
# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "__daily_full.csv$", full.names = TRUE)
# Apply the function to each CSV file and store the results in a list
processed_data_list <- lapply(csv_files, add_site_column)
# Combine the list of data frames into a single data frame
LM_dataF <- do.call(rbind, processed_data_list)

## subset data:
LM_dataF <-LM_dataF %>% 
  subset(name=="ER" | name=="GPP"|name=="NEP") %>%
  filter(site=="BWNS3"| site=="GBNS2"| site=="GBNS3"| site=="SSNS1")

# add in column for date:
LM_dataF$origin <- as.Date(paste0(LM_dataF$year, "-01-01"),) 
LM_dataF$date <-as.Date(LM_dataF$yday, origin = LM_dataF$origin) 

## Plotting the "Not filtered model output"
## quick plot of all data in long form: 
p4 <- ggplot(data = LM_dataF %>% drop_na(year),aes(date, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point(alpha = 0.6)+
  scale_color_manual(values = c("#982649", "#003F91", "#333333")) +
  scale_fill_manual(values = c("#982649","#003F91","black")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y", 
               limits=c(as.Date("2021-06-10"), as.Date("2023-09-07")))+ 
  #ylim(-35, 35) +
  labs(y=expression(mmol~O[2]~m^-3~d^-1), x=NULL) + 
  theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  facet_grid((site~.))
p4
# ggsave(plot = p4, filename = paste("./24_LM_figures/24_all_F.png",sep=""),width=10.5,height=12,dpi=300)


## Subset for expected values |35|
LM_dataF1 <-LM_dataF %>% subset(middle <= 35 & middle >= -35)

# quick plot of all data in long form: 
p5 <- ggplot(data = LM_dataF1 %>% drop_na(year),aes(date, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point(alpha = 0.6)+
  scale_color_manual(values = c("#982649", "#003F91", "#333333")) +
  scale_fill_manual(values = c("#982649","#003F91","black")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y", 
               limits=c(as.Date("2021-06-10"), as.Date("2023-09-07")))+ 
  ylim(-35, 35) +
  labs(y=expression(mmol~O[2]~m^-3~d^-1), x=NULL) + 
  theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  facet_grid((site~.))
p5
# ggsave(plot = p5, filename = paste("./24_LM_figures/24_all_F_subset2.png",sep=""),width=10.5,height=12,dpi=300)

### KEY model inputs:
# BWNS3, GBNS2, GBNS3, SSNS1

### RAW do:
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
ns_DOW <- ns_DO%>%
  filter(Site=="BWNS1" | Site=="BWNS2" |Site=="BWNS3"|
           Site=="SSNS1" |Site=="SSNS2" | Site=="SSNS3") 
           
ns_DOE <- ns_DO%>%
  filter(Site=="SHNS1" | Site=="SHNS2" | Site=="SHNS3" |
           Site=="GBNS1" |Site=="GBNS2" |Site=="GBNS3")
unique(ns_DOQ$Site)

# Look at flags:
# Flag 1 - "YES" indicates day of deployment or removal
# Flag 2 - "YES" indicates poor miniDOT instrument function
# Flag 3 - "YES" indicates poor miniwiper instrument function
# Flag 4 - "YES" indicates suspected biofouling due to any of the above, photos, or general DO trends

plot_w <-
  ggplot(ns_DOW, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") + 
  facet_grid(Site~.)
plot_w
# ggsave(plot = plot_w, filename = paste("./24_LM_figures/24w_NF_DO.png",sep=""),width=10.5,height=12,dpi=300)


plot_e <-
  ggplot(ns_DOE, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") + 
  facet_grid(Site~.)
plot_e
# ggsave(plot = plot_e, filename = paste("./24_LM_figures/24e_NF_DO.png",sep=""),width=10.5,height=12,dpi=300)


# # Filter non-flagged data
ns_DOQ_f4 <- ns_DOW %>% filter(Flag4=="NO")
ns_DOQ_f3 <- ns_DOQ_f4 %>% filter(Flag3=="NO")
ns_DOQ_f2 <- ns_DOQ_f3 %>% filter(Flag2=="NO")
ns_DOQ_f1 <- ns_DOQ_f2 %>% filter(Flag1=="NO")


plot_wF <-
  ggplot(ns_DOQ_f1, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") + 
  facet_grid(Site~.)
plot_wF
# ggsave(plot = plot_wF, filename = paste("./24_LM_figures/24w_F_DO.png",sep=""),width=10.5,height=12,dpi=300)




# # Filter non-flagged data
ns_DOQ_f4 <- ns_DOE %>% filter(Flag4=="NO")
ns_DOQ_f3 <- ns_DOQ_f4 %>% filter(Flag3=="NO")
ns_DOQ_f2 <- ns_DOQ_f3 %>% filter(Flag2=="NO")
ns_DOQ_f1 <- ns_DOQ_f2 %>% filter(Flag1=="NO")


plot_eF <-
  ggplot(ns_DOQ_f1, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") + 
  facet_grid(Site~.)
plot_eF
# ggsave(plot = plot_eF, filename = paste("./24_LM_figures/24e_F_DO.png",sep=""),width=10.5,height=12,dpi=300)

# think we need to filter data differently...


## copying over 
DOT_df5

# select just the NS sites:
ns_DOTW <- DOT_df5%>%
  filter(Site=="BWNS1" | Site=="BWNS2" |Site=="BWNS3"|
           Site=="SSNS1" |Site=="SSNS2" | Site=="SSNS3") 

ns_DOTE <- DOT_df5%>%
  filter(Site=="SHNS1" | Site=="SHNS2" | Site=="SHNS3" |
           Site=="GBNS1" |Site=="GBNS2" |Site=="GBNS3")
unique(ns_DOQ$Site)


plot_temp <-
  ggplot(ns_DOTW, aes(x=datetime, y=wtemp, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(Site~.)
plot_temp
# ggsave(plot = plot_temp, filename = paste("./24_LM_figures/24w_NF_temp.png",sep=""),width=10.5,height=12,dpi=300)


plote_temp <-
  ggplot(ns_DOTE, aes(x=datetime, y=wtemp, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(Site~.)
plote_temp
# ggsave(plot = plote_temp, filename = paste("./24_LM_figures/24e_NF_temp.png",sep=""),width=10.5,height=12,dpi=300)

names(DOT_df5)

plot_parint <-
  ggplot(DOT_df5, aes(x=datetime, y=par_int, colour = as.factor(Site))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") +facet_grid(Site~.)
plot_parint


plot_z <-
  ggplot(DOT_df5, aes(x=datetime, y=z, colour = as.factor(Site))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Z - mix depth (m)', x=NULL) +
  theme_bw() + 
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") #+facet_grid(Site~.)
plot_z
# ggsave(plot = plot_light, filename = paste("./24_LM_figures/24_NF_z.png",sep=""),width=10.5,height=4,dpi=300)


plot_light <-
  ggplot(clim_dat, aes(x=datetime, y=light, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'light dat (SWin)', x=NULL) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
               limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot_light
# ggsave(plot = plot_light, filename = paste("./24_LM_figures/24_NF_light.png",sep=""),width=10.5,height=10,dpi=300)



plot_wsp <-
  ggplot(clim_dat, aes(x=datetime, y=windsp_ms, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Wind sp (ms)', x=NULL) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot_wsp
# ggsave(plot = plot_wsp, filename = paste("./24_LM_figures/24_NF_windsp.png",sep=""),width=10.5,height=10,dpi=300)

clim_dat$baro <- (clim_dat$baro_Pa*0.01)

plot_baro <-
  ggplot(clim_dat, aes(x=datetime, y=baro, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") +facet_grid(shore~.)
plot_baro
# ggsave(plot = plot_baro, filename = paste("./24_LM_figures/24_NF_baro.png",sep=""),width=12.5,height=4,dpi=300)




plot <-
  ggplot(clim_dat1, aes(x=datetime, y=windsp_ms, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  #labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot



plot <-
  ggplot(NS_dat, aes(x=datetime, y=windsp_ms, colour = as.factor(Site))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  #labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(Site~.)
plot