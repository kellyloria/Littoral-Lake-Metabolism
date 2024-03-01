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
  site_info <- str_match(file_name, "([A-Za-z0-9_]+)__nu_daily_full.csv") # may have to remove nu
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

# folder_path <- "./24_plotdata/NotFiltered" # mac path
folder_path <- "./ModelOutput/F/" # PC path


# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "__nu_daily_full.csv$", full.names = TRUE)
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
# ggsave(plot = p2, filename = paste("./24_LM_figures/24_all_NF.png",sep=""),width=9,height=11,dpi=300)


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
# ggsave(plot = p3, filename = paste("./24_LM_figures/24_all_NF_subset.png",sep=""),width=9,height=9,dpi=300)


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
  filter(site=="BWNS3"| site=="GBNS3" | site=="SSNS1")

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
# ggsave(plot = p4, filename = paste("./24_LM_figures/24_all_F.png",sep=""),width=9,height=10,dpi=300)


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
# ggsave(plot = p5, filename = paste("./24_LM_figures/24_all_F_subset2.png",sep=""),width=9,height=9,dpi=300)

### KEY model inputs:
# BWNS3, GBNS2, GBNS3, SSNS1

# ### RAW do:
# ns_DO <- readRDS("./RawData/NS_miniDOT/flagged_all_100423.rds")
# str(ns_DO)
# ns_DO <- ns_DO %>% 
#   mutate(datetime = as.POSIXct(Pacific_Standard_Time, format ="%Y-%m-%d %H:%M:%S")) %>%
#   with_tz(tz = "America/Los_Angeles") %>%
#   mutate(datetime_rounded = round_date(datetime, "5 mins"))
# 
# # create matching column to line up data by site:
# ns_DO$Site <- paste(ns_DO$site, ns_DO$location, ns_DO$replicate, sep = "_")
# unique(ns_DO$Site)
# ns_DO$Site <- gsub("_3m_", "", ns_DO$Site)

## Read in final input / cleaned DO:
DO_dat = list.files(paste("./FinalInputs/NonFiltered/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  mutate(datetime = as.POSIXct((datetime), format ="%d-%m-%Y %H:%M:%S"))
str(DO_dat)


# select just the NS sites:
ns_DOW <- DO_dat%>%
  filter(Site=="BWNS1" | Site=="BWNS2" |Site=="BWNS3"|
           Site=="SSNS1" |Site=="SSNS2" | Site=="SSNS3") 
           
ns_DOE <- DO_dat%>%
  filter(Site=="SHNS1" | Site=="SHNS2" | Site=="SHNS3" |
           Site=="GBNS1" |Site=="GBNS2" |Site=="GBNS3")
unique(ns_DOQ$Site)


plot_w <- ggplot(ns_DOW, aes(x = datetime, y = do, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") + 
  facet_grid(Site ~ .)

plot_w
# ggsave(plot = plot_w, filename = paste("./24_LM_figures/24w_NF_DO.png",sep=""),width=8,height=8,dpi=300)



plot_w2 <- ggplot(ns_DOW, aes(x = datetime, y = do, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_w2

# ggsave(plot = plot_w2, filename = paste("./24_LM_figures/24w_NF_DO_lumped.png",sep=""),width=8,height=3,dpi=300)



plot_e <- ggplot(ns_DOE, aes(x = datetime, y = do, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") + 
  facet_grid(Site ~ .)

plot_e
# ggsave(plot = plot_e, filename = paste("./24_LM_figures/24e_NF_DO.png",sep=""),width=8,height=9,dpi=300)


plot_e2 <- ggplot(ns_DOE, aes(x = datetime, y = do, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_e2

# ggsave(plot = plot_e2, filename = paste("./24_LM_figures/24e_NF_DO_lumped.png",sep=""),width=8,height=3,dpi=300)



plot_all <- ggplot(DO_dat, aes(x = datetime, y = do, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_all

# ggsave(plot = plot_all, filename = paste("./24_LM_figures/24_NF_DO_lumped.png",sep=""),width=8,height=3,dpi=300)

## temp:
plot_wtemp <- ggplot(ns_DOW, aes(x = datetime, y = wtemp, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") + 
  facet_grid(Site ~ .)
plot_wtemp
# ggsave(plot = plot_wtemp, filename = paste("./24_LM_figures/24w_temp.png",sep=""),width=8,height=8,dpi=300)



plot_w2temp <- ggplot(ns_DOW, aes(x = datetime, y = wtemp, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_w2temp

# ggsave(plot = plot_w2temp, filename = paste("./24_LM_figures/24w_temp_lumped.png",sep=""),width=8,height=3,dpi=300)



plot_all_temp <- ggplot(DO_dat, aes(x = datetime, y = wtemp, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_all_temp



# ggsave(plot = plot_all_temp, filename = paste("./24_LM_figures/24_temp_lumped.png",sep=""),width=8,height=3,dpi=300)




plot_etemp <- ggplot(ns_DOE, aes(x = datetime, y = wtemp, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") + 
  facet_grid(Site ~ .)

plot_etemp
# ggsave(plot = plot_etemp, filename = paste("./24_LM_figures/24e_temp.png",sep=""),width=8,height=9,dpi=300)


plot_e2temp <- ggplot(ns_DOE, aes(x = datetime, y = wtemp, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_e2temp

# ggsave(plot = plot_e2temp, filename = paste("./24_LM_figures/24e_temp_lumped.png",sep=""),width=8,height=3,dpi=300)




plot_all_par_int <- ggplot(DO_dat, aes(x = datetime, y = par_int, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
plot_all_par_int
# ggsave(plot = plot_all_par_int, filename = paste("./24_LM_figures/24_parInt_lumped.png",sep=""),width=8,height=3,dpi=300)



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
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot_wsp
# ggsave(plot = plot_wsp, filename = paste("./24_LM_figures/24_NF_windsp.png",sep=""),width=8,height=8,dpi=300)


plot_wsp2 <-
  ggplot(clim_dat, aes(x=datetime, y=windsp_ms, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Wind sp (ms)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))# + theme(legend.position = "none") 
plot_wsp2

# ggsave(plot = plot_wsp2, filename = paste("./24_LM_figures/24_NF_windsp.png",sep=""),width=8,height=3,dpi=300)



clim_dat$baro <- (clim_dat$baro_Pa*0.01)

plot_baro <-
  ggplot(clim_dat, aes(x=datetime, y=baro, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot_baro
# ggsave(plot = plot_baro, filename = paste("./24_LM_figures/24_NF_baro.png",sep=""),width=8,height=8,dpi=300)



plot_baro2 <-
  ggplot(clim_dat, aes(x=datetime, y=baro, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") +facet_grid(shore~.)
plot_baro2
# ggsave(plot = plot_baro2, filename = paste("./24_LM_figures/24_NF_baro2.png",sep=""),width=8,height=3,dpi=300)




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





plot_baro <-
  ggplot(clim_dat, aes(x=datetime, y=baro, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot_baro
# ggsave(plot = plot_baro, filename = paste("./24_LM_figures/24_NF_baro.png",sep=""),width=8,height=8,dpi=300)



plot_baro2 <-
  ggplot(clim_dat, aes(x=datetime, y=baro, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'Baro pressure (mmHg)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") +facet_grid(shore~.)
plot_baro2
# ggsave(plot = plot_baro2, filename = paste("./24_LM_figures/24_NF_baro2.png",sep=""),width=8,height=3,dpi=300)



plot_light <-
  ggplot(clim_dat, aes(x=datetime, y=light, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'SWin (W/m2)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + theme(legend.position = "none") +facet_grid(shore~.)
plot_light
# ggsave(plot = plot_light, filename = paste("./24_LM_figures/24_NF_light.png",sep=""),width=8,height=8,dpi=300)



plot_light2 <-
  ggplot(clim_dat, aes(x=datetime, y=light, colour = as.factor(shore))) +
  geom_point(alpha=0.4) + #geom_line(alpha=0.25)+
  labs(y = 'SWin (W/m2)', x=NULL) +
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") +facet_grid(shore~.)
plot_light2
# ggsave(plot = plot_light2, filename = paste("./24_LM_figures/24_NF_light2.png",sep=""),width=8,height=3,dpi=300)





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


#=============================================
## Read in DO data - for decision making plot 
#==============================================
Filterdat <- readRDS("./RawData/NS_miniDOT/24_NS_flitedDO.rds") %>%
  mutate(date = as.Date(datetime)) 
str(Filterdat)
#
ns_DOQ <- as.data.frame(Filterdat)
ns_DOQ1 <- distinct(ns_DOQ, Site, datetime, shore, .keep_all = TRUE)


####
# depth_dat <- readRDS("./RawData/RBR\ profiles/24NS_depth_dat.rds") %>%
#   dplyr::rename(real_NS_depth = sensor_depth) %>%
#   dplyr::select(shore, Site, date, real_NS_depth) 

# depth_dat_unique <- distinct(depth_dat, Site, date, shore, .keep_all = TRUE)

# depth_dat_unique <- distinct(depth_dat, Site, date, shore, .keep_all = TRUE)
# ns_DOQ1 <- left_join(ns_DOQ, depth_dat_unique, by = c("Site", "date", "shore"))

depnotes <- read.csv("./RawData/RBR\ profiles/SensorMetaNotes.csv") %>%
  mutate(date = as.Date(date))%>%
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

depnotes$time <- c("11:00:00")

depnotes$datetime <- as.POSIXct(paste(depnotes$date, depnotes$time), format="%Y-%m-%d %H:%M:%S")

str(depnotes)

depnotes<- depnotes%>%
  dplyr::rename(Site ="site")

###
Dec_plot <- ggplot(ns_DOQ, aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
Dec_plot

depnotes_long <- depnotes %>%
  pivot_longer(cols = download:deployed, names_to = "event", values_to = "value", names_prefix = "") %>%
  filter(value == 1) %>%
  arrange(datetime, Site)

## adding an hour to stagger lines on plot:

# Assuming depnotes_long is your tibble
depnotes_long1 <- depnotes_long %>%
  group_by(Site, datetime) %>%
  mutate(
    time_offset = (row_number() - 1) * 24,  # Adjust the offset here (e.g., 2 for 2-hour offset)
    datetime = datetime + lubridate::hours(time_offset)
  ) %>%
  ungroup() %>%
  select(-time_offset)

###
Dec_plot_BW <- ggplot(ns_DOQ1%>% filter(shore=="BW"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + facet_grid(Site~.)
# Add vertical lines for each event date with different line types
p2_BW <- Dec_plot_BW +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1 %>% filter(shore=="BW"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))+
  facet_grid(Site~.)


###
Dec_plot_GB <- ggplot(ns_DOQ1%>% filter(shore=="GB"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + facet_grid(Site~.)
# Add vertical lines for each event date with different line types
p2_GB <- Dec_plot_GB +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1%>%filter(shore=="GB"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))+
  facet_grid(Site~.)



###
Dec_plot_SH <- ggplot(ns_DOQ1%>% filter(shore=="SH"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + facet_grid(Site~.)
# Add vertical lines for each event date with different line types
p2_SH <- Dec_plot_SH +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1%>%filter(shore=="SH"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))+
  facet_grid(Site~.)



###
Dec_plot_SS <- ggplot(ns_DOQ1%>% filter(shore=="SS"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + facet_grid(Site~.)
# Add vertical lines for each event date with different line types
p2_SS <- Dec_plot_SS +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1%>%filter(shore=="SS"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))+
  facet_grid(Site~.)




dec_grid <- ggarrange(
  p2_BW,
  p2_SS,
  p2_GB,
  p2_SH,
  nrow = 4,
  common.legend = TRUE # Use "none" to remove the legend entirely
)

dec_grid

# ggsave(plot = dec_grid, filename = paste("./24_LM_figures/NS24_decision_.png",sep=""),width=8,height=10,dpi=300)
# 


## V2 ## 

###
Dec_plot_BW <- ggplot(ns_DOQ1%>% filter(shore=="BW"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))
# Add vertical lines for each event date with different line types
p2_BW <- Dec_plot_BW +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1 %>% filter(shore=="BW"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))


###
Dec_plot_GB <- ggplot(ns_DOQ1%>% filter(shore=="GB"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))
# Add vertical lines for each event date with different line types
p2_GB <- Dec_plot_GB +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1%>%filter(shore=="GB"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))



###
Dec_plot_SH <- ggplot(ns_DOQ1%>% filter(shore=="SH"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
# Add vertical lines for each event date with different line types
p2_SH <- Dec_plot_SH +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1%>%filter(shore=="SH"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))


###
Dec_plot_SS <- ggplot(ns_DOQ1%>% filter(shore=="SS"), aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
  geom_point(alpha = 0.1) + 
  #geom_line(alpha = 0.25) +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) 
# Add vertical lines for each event date with different line types
p2_SS <- Dec_plot_SS +
  geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_long1%>%filter(shore=="SS"), 
             color = "grey25", alpha = 0.8) +
  scale_linetype_manual(values = c("download" = "solid", "cleaned" = "dotted", "copper" = "dashed", "paint" = "longdash", "deployed" = "solid"))



dec_grid <- ggarrange(
  p2_BW,
  p2_SS,
  p2_GB,
  p2_SH,
  nrow = 4,
  common.legend = TRUE # Use "none" to remove the legend entirely
)

dec_grid

# ggsave(plot = dec_grid, filename = paste("./24_LM_figures/NS24_decision2_.png",sep=""),width=8,height=8,dpi=300)
# 

# 
# # Filter data to include only the unique site-datetime combinations for each event
# depnotes_filtered <- depnotes %>%
#   filter(download == 1 | cleaned == 1 | copper == 1 | paint == 1 | deployed == 1) %>%
#   pivot_longer(cols = download:deployed, names_to = "event", values_to = "value") %>%
#   filter(value == 1)
# 
# # Add vertical lines for each event date with different line types
# p2 <- Dec_plot +
#   geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_filtered, 
#              color = "red", alpha = 0.8) +
#   scale_linetype_manual(values = c("download" = "dashed", "cleaned" = "dotted", "copper" = "solid", "paint" = "longdash", "deployed" = "twodash")) +
#   facet_grid(shore~.)
# 
# p2
# 
# # ggsave(plot = p2, filename = paste("./24_LM_figures/NS24_decision_.png",sep=""),width=8,height=3,dpi=300)
# 
# 
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# 
# # Assuming ns_DOQ1 contains your data
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# 
# # Filter data to include only the unique site-datetime combinations for each event
# depnotes_filtered <- depnotes %>%
#   filter(download == 1 | cleaned == 1 | copper == 1 | paint == 1 | deployed == 1) %>%
#   pivot_longer(cols = download:deployed, names_to = "event", values_to = "value") %>%
#   filter(value == 1)
# 
# # Iterate through each site
# for(site in unique(ns_DOQ1$site)) {
#   # Subset data for the current site
#   site_data <- ns_DOQ1 %>% filter(site == .data$site)
#   
#   # Create a plot for the current site
#   site_plot <- ggplot(site_data, aes(x = datetime, y = Dissolved_O_mg_L, colour = shore)) +
#     geom_point(alpha = 0.1) + 
#     theme_bw() +
#     scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
#                      limits = c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+ 
#     scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
#     theme(axis.text.x = element_text(size = 10), 
#           axis.text.y = element_text(size = 10),
#           axis.title.x = element_text(size = 12),
#           axis.title.y = element_text(size = 12),
#           plot.subtitle = element_text(size = 12)) +
#     ggtitle(paste("Site:", site)) +  # Title with site name
#     geom_vline(aes(xintercept = datetime, linetype = event), data = depnotes_filtered %>% filter(site == site), 
#                color = "red", alpha = 0.8) +
#     scale_linetype_manual(values = c("download" = "dashed", "cleaned" = "dotted", "copper" = "solid", "paint" = "longdash", "deployed" = "twodash")) 
#   
#   # Print the plot
#   print(site_plot)
# }




####
### Wind mix from models:
## load packages
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(rstan)
library(patchwork)
library(plotly)
source("./Littoral-Lake-Metabolism/saved_fxns/helper_functions.r")

# lake morph:
max_d <-160 #501 
lake.area <- 165400000#496200000
out.time.period <- "60 min"
tz <-  "US/Pacific"

# read in clean data:
sonde = list.files(paste("./FinalInputs/Filtered/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()
unique(sonde$year)


# Adjust data frame to relevant years
years = c(2021,2022,2023)
data <- sonde %>% filter(year %in% years)
summary(data)

# set conditions for mixing depth "z"
data <- data %>% 
  group_by(year,yday, Site) %>%
  mutate(obs = sum(!is.na(do))) %>%       #identify and filter records that have < 23 hrs of data 
  ungroup() #%>%
  #mutate(z = ifelse(z<=0.5,.5,z))%>% # can't have zero depth zmix
  #mutate(z = ifelse(z>=4.5,4.5,z))  #in littoral zone depth zmix can not be deeper than the littoral depth

# determine data frequency obs/day
freq <- calc.freq(data$datetime) # needs to be 24

# use lake metabolizer fxn to calculate gas exchange:
data <- data %>% 
  group_by(year,yday, Site) %>% #
  filter(obs>=(freq-(freq/24*2))) %>% #allow for 2 hours
  mutate(k600 = k.vachon.base(wnd = wspeed,lake.area = lake.area)) %>% #estimate K in m/day
  mutate(kgas = k600.2.kGAS.base(k600 = k600,temperature = wtemp,gas = "O2")) %>%  #m/d
  mutate(k = (kgas/freq)/z) 

# quick plot to check: 

K_plot <- ggplot(data, aes(colour = shore)) +
  geom_point(aes(y=do,x=k), alpha = 0.1) + 
  theme_bw() + facet_wrap(vars(Site),scales="free_x") +
  geom_smooth(aes(y=do,x=k),method="lm")+
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))+
  labs(y = "obs DO (mgL)", x = "k (m/day)" , 
       subtitle = "Final gas transfer velocity (k) normalized by observation frequency and mixing depth (z)")
# ggsave(plot = K_plot, filename = paste("./24_LM_figures/NS24_gastransferv_K.png",sep=""),width=8,height=6,dpi=300)


wpsd_plot <- ggplot(data, aes(colour = shore)) +
  geom_point(aes(y=do,x=wspeed), alpha = 0.1) + 
  theme_bw() + facet_wrap(vars(Site),scales="free_x") +
  geom_smooth(aes(y=do,x=wspeed),method="lm")+
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))+
  labs(y = "obs DO (mgL)", x = "wind sp (m/sec)" , 
       subtitle = "Wind speed and DO")
# ggsave(plot = wpsd_plot, filename = paste("./24_LM_figures/NS24_windsp_DO.png",sep=""),width=8,height=6,dpi=300)



wpsd_K_plot <- ggplot(data, aes(colour = shore)) +
  geom_point(aes(y=k,x=wspeed), alpha = 0.1) + 
  theme_bw() + facet_wrap(vars(Site),scales="free_x") +
  geom_smooth(aes(y=k,x=wspeed),method="lm")+
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))+
  labs(y = "k (m/day)", x = "wind sp (m/sec)" , 
       subtitle = "Final gas transfer velocity (k) and wind speed (m/s)")
# ggsave(plot = wpsd_K_plot, filename = paste("./24_LM_figures/NS24_windsp_K.png",sep=""),width=8,height=6,dpi=300)



KtimeS_plot <- ggplot(data, aes(colour = shore)) +
  geom_point(aes(y=k,x=datetime), alpha = 0.1) + 
  theme_bw() +
  scale_colour_manual(values = c(SS = "#136F63", BW = "#3283a8", GB = "#a67d17", SH = "#c76640")) +
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b-%y", 
                   limits=c(as.POSIXct("2021-06-10 01:00:00"), as.POSIXct("2023-09-07 24:00:00")))+
  facet_grid(shore~.) +
  labs(x = NULL, y = "k (m/day)")
# ggsave(plot = KtimeS_plot, filename = paste("./24_LM_figures/NS24_ktimesieries.png",sep=""),width=8,height=6,dpi=300)



## Look at ER and K
str(LM_data)
str(data)

site_data <- data %>% filter(Site == "GBNS2" |Site == "BWNS2" | 
                               Site == "GBNS1" |Site == "BWNS1") %>%
  group_by(Site, date)%>%
  summarise(k_daily = mean(k, na.rm=TRUE), 
            DO_daily = mean(do, na.rm=TRUE))

LM_data_pl <- left_join(LM_data, site_data)

# Plot ER against date

ER_plt <- ggplot(data = LM_data_pl %>% filter(name=="ER"),aes(k_daily , (middle*-1), color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = (lower*-1), ymax = (upper*-1), fill = name),
              linetype = 0, alpha = 0.2)+ geom_point(alpha = 0.6)+
  geom_smooth(method="lm",se=F) +
  scale_color_manual(values = c("#982649" )) + # "#003F91", "#333333"
  scale_fill_manual(values = c("#982649")) +
  labs(y="Daily ER * -1", x="Daily k") +  theme_bw() +
  facet_grid((site~.))

GPP_plt <- ggplot(data = LM_data_pl %>% filter(name=="GPP"),aes(k_daily , middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+ geom_point(alpha = 0.6)+
  geom_smooth(method="lm",se=F) +
  scale_color_manual(values = c("#003F91" )) + # "#003F91", "#333333"
  scale_fill_manual(values = c("#003F91")) +
  labs(y="Daily GPP", x="Daily k") +  theme_bw() +
  facet_grid((site~.))



k_grid <- ggarrange(
  GPP_plt,
  ER_plt,
  ncol = 2,
  legend =c("bottom"),
  common.legend = F # Use "none" to remove the legend entirely
)

k_grid

# ggsave(plot = k_grid, filename = paste("./24_LM_figures/NS24_gasex_.png",sep=""),width=5,height=6,dpi=300)
# 



ggplot(site_data, aes(x = date, y = k)) +
  
  geom_point() +  # Add points
  geom_smooth(method = "loess", se = FALSE) +  # Add smoothed line
  labs(x = "Date", y = "Ecosystem Respiration (ER)", title = "Ecosystem Respiration Over Time")
LM_data