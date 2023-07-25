## Load packages

## Load packages
lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "tidyverse","data.table","xts","dygraphs",
         "nrlmetab","cowplot"), require, character.only=T) #"LakeMetabolizer"

WPAR = list.files(paste("/Users/kellyloria/Desktop/LakeTahoeNS/PAR_FA10B7CAD952_W/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

WPAR1 <- WPAR %>%
  dplyr::select(dateTime, data1, data2, loggerUid)%>%
  dplyr::rename(wtr = data2, PAR = data1, datetime=dateTime, sieral=loggerUid) %>%
  mutate(datetime = as.POSIXct((datetime), format ="%d-%m-%Y %H:%M:%S"))


WPAR_plot <- qplot(datetime, PAR, data = WPAR1, geom="point", color =sieral, size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8"),0.8)) + # , "#226b5b", "#a84e32","#b57633","#164778"
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))



range(na.omit(WPAR1$datetime))


WPAR2 <- WPAR1 %>% unique()

## DO -- subset to time frame of interest & plot
vis_data_PAR <- function(x){
  
  x <- subset(x, x$datetime > "2022-01-01 00:00:00")
  
  # Then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$PAR, order.by = x$datetime)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

vis_data_PAR(WPAR2) 

WPAR_adj <- WPAR2[-which(WPAR2$datetime <= "2022-03-24 12:00:00"),]
vis_data_PAR(WPAR_adj) 
WPAR_adj <- WPAR_adj[-which(WPAR_adj$datetime >= "2022-07-15 08:00:00" & WPAR_adj$datetime <= "2022-07-15 09:00:00"),]
vis_data_PAR(WPAR_adj) 
WPAR_adj <- WPAR_adj[-which(WPAR_adj$datetime >= "2022-10-17 12:50:00"),]
vis_data_PAR(WPAR_adj) 

# write.csv(WPAR_adj, file = "/Users/kellyloria/Desktop/LakeTahoeNS/CleanRawDat/Climate/PAR_FA10B7CAD952_W_clean.csv", row.names = TRUE)


EPAR = list.files(paste("/Users/kellyloria/Desktop/LakeTahoeNS/PAR_F37459D7C4C7_E/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

EPAR1 <- EPAR %>%
  select(dateTime, data1, data2, loggerUid)%>%
  rename(wtr = data2, PAR = data1, datetime=dateTime, sieral=loggerUid) %>%
  mutate(datetime = as.POSIXct((datetime), format ="%d-%m-%Y %H:%M:%S"))


vis_data_PAR(EPAR1) 

EPAR_adj <- EPAR1[-which(EPAR1$datetime >= "2022-03-24 10:30:00" & EPAR1$datetime <= "2022-05-05 10:30:000"),]
vis_data_PAR(EPAR_adj)

EPAR_adj <- EPAR_adj[-which(EPAR_adj$datetime>= "2022-05-21 12:30:000"),]
vis_data_PAR(EPAR_adj)

# write.csv(EPAR_adj, file = "/Users/kellyloria/Desktop/LakeTahoeNS/CleanRawDat/Climate/PAR_F37459D7C4C7_E_clean.csv", row.names = TRUE)


## bring in climate data:
west_ws <- read.csv("./CleanRawDat/Climate/WestNSclimate.csv")

## Adjust climate data
climate_adj <- function(x){
  
  ## rename and convert solar_rad to PAR
  c <- x %>% rename(datetime = Date_Time, solar_rad = solar_radiation_set_1, wspeed = wind_speed_set_1) %>%
    mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
    mutate(par = solar_rad*2.114) %>%
    select(datetime, par, wspeed)
  
  ## take the mean each hour
  c <- c %>%
    group_by(datetime = floor_date(datetime, "hour")) %>%
    summarise(Spar = mean(par, na.rm = TRUE), wspeed = mean(wspeed, na.rm = TRUE))
  
  ## adjust datetime from UTC to local PT time
  c$datetime <- c$datetime - hours(8)
  
  return(c)
  
  
}

#east_clim <- climate_adj(east_ws)
west_clim <- climate_adj(west_ws)

##
WPAR_adj1 <- WPAR_adj%>%
  group_by(datetime = floor_date(datetime, "hour")) %>%
  summarise(Hpar = mean(PAR, na.rm = TRUE))


## full join 
WPAR_dat <- WPAR_adj1 %>%
  full_join(west_clim)

## calculate the diffuse attenuation of PAR 
# Where Zi is a depth within the water column, 
# and Epar is PAR m-1s-1 at depth Zi (Kosten et al., 2009). 

WPAR_dat$Kd <- c(1/(19.75-0))*log(WPAR_dat$Spar/WPAR_dat$Hpar)


##The diffuse attenuation coefficient (Kd) was calculated by the log linear function,
#where (Ez) is irradiance at depth Z, (E0) is irradiance at the surface, and Kd is the slope of the log linearized function [Kirk, 1994b].

#WPAR_dat$Kd2 <- log(WPAR_dat$Spar/WPAR_dat$Hpar)/(19.75)

# write.csv(WPAR_dat1, file = "/Users/kellyloria/Desktop/LakeTahoeNS/CleanRawDat/Climate/PAR_FA10B7CAD952_W_Kdest.csv", row.names = TRUE)

WPAR_dat1 <- WPAR_dat %>%
  replace(WPAR_dat<0, NA)

# replace inf with NA

library(dplyr)
fortify.zoo(WPAR_dat1) %>% mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x)))


WPAR_plot <- qplot(datetime, Kd, data = WPAR_dat1, geom="point", size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8"),0.8)) + # , "#226b5b", "#a84e32","#b57633","#164778"
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


###
########################################################
extcoef <- read.csv("TERC_2021_Kd_coeffs.csv", header=T) 

## modify kd dataframe
head(extcoef)
extcoef$datetime <- as.POSIXct(as.character(extcoef$datetime), format="%m/%d/%Y")
extcoef$yday <- lubridate::yday(extcoef$datetime)
## add in time:
extcoef$datetime2 <- as.POSIXct(c(
  "2021-07-01 12:00:00",
  "2021-08-20 12:00:00",
  "2021-08-23 12:00:00",
  "2021-08-25 12:00:00",
  "2021-08-27 12:00:00",
  "2021-09-02 12:00:00",
  "2021-09-21 12:00:00",
  "2021-09-30 12:00:00"), 
  tz="America/Los_Angeles",
  format = c("%Y-%m-%d %H:%M:%OS"))



extcoef22<-extcoef
extcoef22$datetime2 <- extcoef22$datetime2 %m+% years(1)
extcoef<- rbind(extcoef, extcoef22)


###
# WPAR_dat1
##


WPAR_dat2 <- left_join(WPAR_dat1, extcoef[,c("datetime2", "yday","PAR_Kd")], by=c("datetime"="datetime2"))


WPAR_plot <- qplot(PAR_Kd, Kd, data = WPAR_dat2, geom="point", size=I(2)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8"),0.8)) + # , "#226b5b", "#a84e32","#b57633","#164778"
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))



extcoef$year <- lubridate::year(extcoef$datetime)

## create data frame for interpolation
Kd_int21 <- as.data.frame(cbind("year" = rep(2021,length(seq(from = extcoef$yday[1], to = 365))), ## you added the :2022
                                "yday" = seq(from = extcoef$yday[1], to = 365)))

Kd_int22 <- as.data.frame(cbind("year" = rep(2022,length(seq(1:365))), ## you added the :2022
                                "yday" = seq(1:365)))

Kd_int22all<-rbind(Kd_int21, Kd_int22)

Kd_int <- left_join(Kd_int22all, extcoef[,c("year", "yday","PAR_Kd")], by=c("year", "yday"), all = T)

## interpolate between Kd values to create a daily time series for 2021
Kd_int <- Kd_int21 %>%
  mutate(PAR_Kd = na.approx(PAR_Kd, maxgap = Inf, na.rm=F))
#check
plot(Kd_int$PAR_Kd)
sapply(Kd_int, class)

### Apply these Kd values to all time series
calc_par_int <-function(df, Kd_df){
  
  ## calc yday for data frame
  df$yday <- lubridate::yday(df$datetime)
  
  ## merge Kd time series with data
  dat <- left_join(df, Kd_df, by=("yday"))
  
  ## calc par_int
  dat <- dat %>%
    mutate(par_int = (par - par*exp(-PAR_Kd*3))/(PAR_Kd*3)) # multipler = "(extcoef*3)" should be depth of water column (3m in this case)
  
  # filter dates
  # dat <- dat %>%
  #   filter(yday >= 1 & yday <=365)
  
  return(dat)
  
}

GS1_allT <- calc_par_int(GS1_all, Kd_int)



