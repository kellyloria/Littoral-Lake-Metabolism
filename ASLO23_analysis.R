# setwd("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/")

library(tidyverse)
library(lubridate)
#plotting packages:
library(ggplot2)
library(cowplot)
library(reshape2)
library(scales)
library(gridExtra)

se <- function(dat){
  se <- sd(dat)/sqrt(length(dat))
  return(se)}


#############
BWNS1 <- read.csv("./plotDat/BWNS1__daily_full_sensor2021.csv")
BWNS1$date <- as.Date(BWNS1$yday, origin="2021-01-01")
BWNS1$site <- "BWNS1"
range(BWNS1$date)

BWNS1_22 <- read.csv("./plotDat/BWNS1__daily_full_sensor2022.csv")
BWNS1_22$date <- as.Date(BWNS1_22$yday, origin="2022-01-01")
BWNS1_22$site <- "BWNS1"
range(BWNS1_22$date)

BWNS2 <- read.csv("./plotDat/BWNS2__daily_full_sensor2021.csv")
BWNS2$date <- as.Date(BWNS2$yday, origin="2021-01-01")
BWNS2$site <- "BWNS2"

BWNS2_22 <- read.csv("./plotDat/BWNS2__daily_full_sensor2022.csv")
BWNS2_22$date <- as.Date(BWNS2_22$yday, origin="2022-01-01")
BWNS2_22$site <- "BWNS2"

BWNS3 <- read.csv("./plotDat/BWNS3__daily_full_sensor2021.csv")
BWNS3$date <- as.Date(BWNS3$yday, origin="2021-01-01")
BWNS3$site <- "BWNS3"

BWNS3_22 <- read.csv("./plotDat/BWNS3__daily_full_sensor2022.csv")
BWNS3_22$date <- as.Date(BWNS3_22$yday, origin="2022-01-01")
BWNS3_22$site <- "BWNS3"


GBNS1 <- read.csv("./plotDat/GBNS1__daily_full_sensor2021.csv")
GBNS1$date <- as.Date(GBNS1$yday, origin="2021-01-01")
GBNS1$site <- "GBNS1"
range(GBNS1$date)

GBNS1_22 <- read.csv("./plotDat/GBNS1__daily_full_sensor2022.csv")
GBNS1_22$date <- as.Date(GBNS1_22$yday, origin="2022-01-01")
GBNS1_22$site <- "GBNS1"


GBNS2 <- read.csv("./plotDat/GBNS2__daily_full_sensor2021.csv")
GBNS2$date <- as.Date(GBNS2$yday, origin="2021-01-01")
GBNS2$site <- "GBNS2"

GBNS2_22 <- read.csv("./plotDat/GBNS2__daily_full_sensor2022.csv")
GBNS2_22$date <- as.Date(GBNS2_22$yday, origin="2022-01-01")
GBNS2_22$site <- "GBNS2"

GBNS3 <- read.csv("./plotDat/GBNS3__daily_full_sensor2021.csv")
GBNS3$date <- as.Date(GBNS3$yday, origin="2021-01-01")
GBNS3$site <- "GBNS3"

GBNS3_22 <- read.csv("./plotDat/GBNS3__daily_full_sensor2022.csv")
GBNS3_22$date <- as.Date(GBNS3_22$yday, origin="2022-01-01")
GBNS3_22$site <- "GBNS3"

lmetab <- rbind(BWNS1, BWNS1_22, BWNS2, BWNS2_22, BWNS3, BWNS3_22, GBNS1, 
                GBNS1_22, 
                GBNS2, 
                GBNS2_22, 
                GBNS3, 
                GBNS3_22)

lmetab_21 <-lmetab %>% 
  subset(year==2021) %>%
  subset(name=="ER" | name=="GPP"|name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)

lmetab_22 <-lmetab %>% 
  subset(year==2022) %>%
  subset(name=="ER" | name=="GPP"|name=="NEP") %>%
  subset(middle <= 40 & middle >= -40) 

lmetab_w21 <-lmetab_21 %>% 
  subset(site=="BWNS1" | site=="BWNS2"| site=="BWNS3") 

lmetab_w22 <-lmetab_22 %>% 
  subset(site=="BWNS1" | site=="BWNS2"| site=="BWNS3") 

wmetab<- rbind(lmetab_w21, lmetab_w22)

lmetab_e21 <-lmetab_21 %>% 
  subset(site=="GBNS1" | site=="GBNS2"| site=="GBNS3") 

lmetab_e22 <-lmetab_22 %>% 
  subset(site=="GBNS1" | site=="GBNS2"| site=="GBNS3") 

emetab<- rbind(lmetab_e21, lmetab_e22)
lametab<- rbind(lmetab_21, lmetab_22)

p2 <- ggplot(data = lametab %>% drop_na(year),aes(date, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point(size= 3, alpha = 0.6)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
             color = "#4c4d4c") +
  #geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("#982649", "#003F91", "#333333")) +
  scale_fill_manual(values = c("#982649","#003F91","black")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+ #new
  ylim(-35, 35) +
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) + 
  theme_classic() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  facet_grid((site~.))
p2
# ggsave(plot = p2, filename = paste("./figures/GBNS_all.png",sep=""),width=12.5,height=11,dpi=300)

range(lametab$date)

################
# Average model output from good models:
# BW 2021: all, 2022: NS1 and 2
BW1sum <- rbind(BWNS1, BWNS1_22)
BWs1GPP<- BW1sum%>%
  subset(name=="GPP") %>%
  subset(middle <= 30 & middle >= 0)
# long to wide
BW1GPP <- reshape(data=BWs1GPP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

BW2sum <- rbind(BWNS2)
BWs2GPP<- BW2sum%>%
  subset(name=="GPP") %>%
  subset(middle <= 30 & middle >= 0)
# long to wide
BW2GPP <- reshape(data=BWs2GPP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


BW3sum <- rbind(BWNS3, BWNS3_22)
BWs3GPP<- BW3sum%>%
  subset(name=="GPP") %>%
  subset(middle <= 30 & middle >= 0)
# long to wide
BW3GPP <- reshape(data=BWs3GPP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


westGPP<- rbind(BW1GPP,BW2GPP, BW3GPP)
westGPP$shore <- "west"


# GB1 2021, GB 2021 and 2022
GB1sum <- rbind(GBNS1)
GBs1GPP<- GB1sum%>%
  subset(name=="GPP") %>%
  subset(middle <= 30 & middle >= 0)
# long to wide
GB1GPP <- reshape(data=GBs1GPP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


GB3sum <- rbind(GBNS3, GBNS3_22)
GBs3GPP<- GB3sum%>%
  subset(name=="GPP") %>%
  subset(middle <= 30 & middle >= 0)
# long to wide
GB3GPP <- reshape(data=GBs3GPP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

eastGPP<- rbind(GB1GPP,GB3GPP)
eastGPP$shore <- "east"

NS_GPP <- rbind(eastGPP, westGPP)


NS_GPP_df <- NS_GPP %>% 
  group_by(date, shore) %>%
  dplyr::summarise(
    GPP_mean = mean(middle.GPP, na.rm=T),
    GPP_sd = sd(middle.GPP,  na.rm=T),
    n = n(),
    GPP_se = GPP_sd / sqrt(n),
    GPPL_mean = mean(lower, na.rm=T),
    GPPL_sd = sd(lower,  na.rm=T),
    GPPU_mean = mean(upper, na.rm=T),
    GPPU_sd = sd(upper,  na.rm=T))



#  dplyr::select(site,TN_ugL_mean, TN_ugL_sd, TN_ugL_se)

p2 <- ggplot(data = NS_GPP_df, aes(date, GPP_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = GPPL_mean, ymax = GPPU_mean, fill = shore),
              linetype = 0, alpha = 0.3) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(-1, 38) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal")

# ggsave(plot = p2, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_GPP.png",sep=""),width=12,height=4.5,dpi=300)
p2

######
# ER #
BWs1ER<- BW1sum%>%
  subset(name=="ER") %>%
  subset(middle <= 0 & middle >= -30)
# long to wide
BW1ER <- reshape(data=BWs1ER, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

BWsER<- BW2sum%>%
  subset(name=="ER") %>%
  subset(middle <= 0 & middle >= -30)
# long to wide
BW2ER <- reshape(data=BWsER, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


BWs3ER<- BW3sum%>%
  subset(name=="ER") %>%
  subset(middle <=0 & middle >= -30)
# long to wide
BW3ER <- reshape(data=BWs3ER, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


westER<- rbind(BW1ER,BW2ER, BW3ER)
westER$shore <- "west"

# GB1 2021, GB 2021 and 2022
GBs1ER<- GB1sum%>%
  subset(name=="ER") %>%
  subset(middle <=0 & middle >= -30)
# long to wide
GB1ER <- reshape(data=GBs1ER, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

GBs3ER<- GB3sum%>%
  subset(name=="ER") %>%
  subset(middle <= 0 & middle >= -30)
# long to wide
GB3ER <- reshape(data=GBs3ER, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

eastER<- rbind(GB1ER,GB3ER)
eastER$shore <- "east"

NS_ER <- rbind(eastER, westER)

NS_ER_df <- NS_ER %>% 
  group_by(date, shore) %>%
  dplyr::summarise(
    ER_mean = mean(middle.ER, na.rm=T),
    ER_sd = sd(middle.ER,  na.rm=T),
    n = n(),
    ER_se = ER_sd / sqrt(n),
    ERL_mean = mean(lower, na.rm=T),
    ERL_sd = sd(lower,  na.rm=T),
    ERU_mean = mean(upper, na.rm=T),
    ERU_sd = sd(upper,  na.rm=T))

p3 <- ggplot(data = NS_ER_df, aes(date, ER_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = ERL_mean, ymax = ERU_mean, fill = shore),
              linetype = 0, alpha = 0.3) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(-38, 1) +
  labs(y=expression("ER"~mmol~O[2]~m^-3~d^-1)) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p3

# ggsave(plot = p3, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_ER.png",sep=""),width=12,height=4,dpi=300)


######
# NEP #
BWs1NEP<- BW1sum%>%
  subset(name=="NEP") %>%
  subset(middle <= 30 & middle >= -30)
# long to wide
BW1NEP <- reshape(data=BWs1NEP, idvar="date",
                 v.names = "middle",
                 timevar = "name",
                 direction="wide")

BWsNEP<- BW2sum%>%
  subset(name=="NEP") %>%
  subset(middle <= 30 & middle >= -30)
# long to wide
BW2NEP <- reshape(data=BWsNEP, idvar="date",
                 v.names = "middle",
                 timevar = "name",
                 direction="wide")


BWs3NEP<- BW3sum%>%
  subset(name=="NEP") %>%
  subset(middle <=30 & middle >= -30)
# long to wide
BW3NEP <- reshape(data=BWs3NEP, idvar="date",
                 v.names = "middle",
                 timevar = "name",
                 direction="wide")


westNEP<- rbind(BW1NEP,BW2NEP, BW3NEP)
westNEP$shore <- "west"

# GB1 2021, GB 2021 and 2022
GBs1NEP<- GB1sum%>%
  subset(name=="NEP") %>%
  subset(middle <=30 & middle >= -30)
# long to wide
GB1NEP <- reshape(data=GBs1NEP, idvar="date",
                 v.names = "middle",
                 timevar = "name",
                 direction="wide")

GBs3NEP<- GB3sum%>%
  subset(name=="NEP") %>%
  subset(middle <= 30 & middle >= -30)
# long to wide
GB3NEP <- reshape(data=GBs3NEP, idvar="date",
                 v.names = "middle",
                 timevar = "name",
                 direction="wide")

eastNEP<- rbind(GB1NEP,GB3NEP)
eastNEP$shore <- "east"

NS_NEP <- rbind(eastNEP, westNEP)

NS_NEP_df <- NS_NEP %>% 
  group_by(date, shore) %>%
  dplyr::summarise(
    NEP_mean = mean(middle.NEP, na.rm=T),
    NEP_sd = sd(middle.NEP,  na.rm=T),
    n = n(),
    NEP_se = NEP_mean / sqrt(n),
    NEPL_mean = mean(lower, na.rm=T),
    NEPL_sd = sd(lower,  na.rm=T),
    NEPU_mean = mean(upper, na.rm=T),
    NEPU_sd = sd(upper,  na.rm=T))


p4 <- ggplot(data = NS_NEP_df, aes(date, NEP_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = NEPL_mean, ymax = NEPU_mean, fill = shore),
              linetype = 0, alpha = 0.3) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(-25, 25) +
  labs(y=expression("NEP"~mmol~O[2]~m^-3~d^-1)) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p4

#ggsave(plot = p4, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_NEP.png",sep=""),width=7,height=5,dpi=300)


############################
# dataframes of responses:
head(NS_NEP_df)
head(NS_ER_df)
head(NS_GPP_df)

## Transformed variables:
# need to create a df of east west stream flow, nutrients and weather covariates
# Richardsnon and schitdt if possible

## Streamflow data:
## Add in flow
library(dataRetrieval)
## FLOW
# library(dataRetrieval)
## Add in flow data
siteNo <- "10336730"
pCode <- c("00060")
start.date <- "2021-06-01"
end.date <- "2022-10-01"

GBflow <- readNWISdata(siteNumbers = siteNo,
                       parameterCd = pCode,
                       startDate = start.date,
                       endDate = end.date)

GBflow.ts <- GBflow %>% select("dateTime", "X_00060_00003") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00003") %>%
  select("datetime", "dischargeCFS")

GBflow.ts$Site<- "GB"
GBflow.ts$shore<-"east"

siteNo <- "10336660"
pCode <- c("00060")
start.date <- "2021-06-01"
end.date <- "2022-10-01"

flow <- readNWISdata(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

BWflow.ts <- flow %>% select("dateTime", "X_00060_00003") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00003") %>%
  select("datetime", "dischargeCFS")

BWflow.ts$Site<- "BW"
BWflow.ts$shore<-"west"

flow<- rbind(BWflow.ts, GBflow.ts)

siteNo <- "10336730"
pCode <- c("00065")
GBstage <- readNWISuv(siteNumbers = siteNo,
                      parameterCd = pCode,
                      startDate = start.date,
                      endDate = end.date)
GBstage.ts <- GBstage %>% select("dateTime", "X_00065_00000") %>% 
  dplyr::rename(datetime = "dateTime", stage_ft = "X_00065_00000") %>%
  select("datetime", "stage_ft")
GBstage.ts$shore<- "east"

siteNo <- "10336660"
pCode <- c("00065")
BWstage <- readNWISuv(siteNumbers = siteNo,
                      parameterCd = pCode,
                      startDate = start.date,
                      endDate = end.date)
BWstage.ts <- BWstage %>% select("dateTime", "X_00065_00000") %>% 
  dplyr::rename(datetime = "dateTime", stage_ft = "X_00065_00000") %>%
  select("datetime", "stage_ft")
BWstage.ts$shore<- "west"


stage_temp <- rbind(GBstage.ts,BWstage.ts)
stage_temp$date <- as.Date(stage_temp$datetime, format= "%Y-%m-%d")

stage_df <- stage_temp %>% 
  group_by(date, shore) %>%
  dplyr::summarise(
    stage_ftm = mean(stage_ft, na.rm=T),
    stage_sd = sd(stage_ft,  na.rm=T))


## add in SPC
GBL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBL_SPC.csv")
GBL22_SPC$solar.time <- as.POSIXct(GBL22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
GBL22_SPC$site<- "GBL"
GBL22_SPC$shore<- "east"

BWL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWL_SPC.csv")
BWL22_SPC$solar.time <- as.POSIXct(BWL22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
BWL22_SPC$site<- "BWL"
BWL22_SPC$shore<- "west"


SPC_dat<- rbind(BWL22_SPC, GBL22_SPC)
SPC_datD <- SPC_dat %>% 
  mutate(date= as.Date(datetime)) %>%
  group_by(date, site, shore) %>% 
  summarise(Stream_SPCmean = mean(SPC,na.rm = T),
            Stream_SPCstd = sd(SPC, na.rm = T),
            Stream_Wtmean = mean(wtr,na.rm = T),
            Stream_Wtstd = sd(wtr, na.rm = T))

# Stream df
Streamdf <- left_join(flow, SPC_datD[,c("date", "shore", "Stream_SPCmean", "Stream_SPCstd", "Stream_Wtmean", "Stream_Wtstd")], 
                      by=c('datetime'='date', 'shore'='shore'))

Streamdf <- left_join(Streamdf, stage_df[,c("date", "shore", "stage_ftm", "stage_sd")], 
                      by=c('datetime'='date', 'shore'='shore'))


# add in DO mean reach reach depth
BW_DO <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_BWLInputs.csv")
BW_DO$shore <- "west"
GB_DO <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_GBL_modelInputs.csv")
GB_DO1<- GB_DO[,c(-2)]
GB_DO1$shore <- "east"

streamDO<- rbind(BW_DO,GB_DO1)
streamDO$date <- as.Date(streamDO$solar.time, format= "%Y-%m-%d")

StreamDO_dat <- streamDO %>% 
  group_by(date, shore) %>% 
  summarise(Stream_DOobs_mean = mean(DO.obs,na.rm = T),
            Stream_DOobs_sd = sd(DO.obs, na.rm = T),
            Stream_DOsat_mean = mean(DO.sat,na.rm = T),
            Stream_DOsat_sd = sd(DO.sat, na.rm = T),
            Stream_depth_mean = mean(depth,na.rm = T),
            Stream_depth_sd = sd(depth, na.rm = T),
            Stream_light_mean = mean(light,na.rm = T),
            light_sd = sd(light, na.rm = T),
            )

Streamdf <- left_join(Streamdf, StreamDO_dat[,c("date", "shore", "Stream_DOobs_mean", "Stream_DOobs_sd",
                                                "Stream_DOsat_mean", "Stream_DOsat_sd", "Stream_depth_mean", "Stream_depth_sd",
                                                "Stream_light_mean")], 
                      by=c('datetime'='date', 'shore'='shore'))

### add in nutrients from lower sites
library(PerformanceAnalytics)
Streamdf_cor <- Streamdf[, c(5:17)]
chart.Correlation(Streamdf_cor, histogram=TRUE, pch=19)

#################
# weather data ##

col_names <- names(read_csv("./AggRawData/W_weather/D9413.2022-10-31.csv",  skip = 10))
climate.temp1 <- read_csv("./AggRawData/W_weather/D9413.2022-10-31.csv", 
                          col_names = col_names,
                          skip=13)
climate.temp1$datetimes <- round_date(climate.temp1$Date_Time, "1 hour")
summary(climate.temp1)
hist(climate.temp1$precip_accum_24_hour_set_1) # bad
hist(climate.temp1$precip_accum_since_local_midnight_set_1) bad


col_names <- names(read_csv("./AggRawData/W_weather/HMDC1.2022-10-31.csv",  skip = 10))

climate.temp2 <- read_csv("./AggRawData/W_weather/HMDC1.2022-10-31.csv", 
                          col_names = col_names,
                          skip=13)
climate.temp2$datetimes <- c(round_date(climate.temp2$Date_Time, "1 hour")) 
summary(climate.temp2)

hist(climate.temp2$precip_accum_set_1) # good

# bind the two datasets together
Wclim <- left_join(climate.temp1, climate.temp2[c("datetimes", "solar_radiation_set_1")],
                   by = c("datetimes" = "datetimes"))
summary(Wclim) # observations still in UTC

# look at data...
qplot(Date_Time, solar_radiation_set_1, data = Wclim, geom="point") +
  scale_x_datetime(labels = date_format("%b"), 
                   breaks = date_breaks("672 hours")) +theme_bw() # just gaps

shortcheck <- subset(Wclim, Date_Time>="2022-05-02 00:00:00" & Date_Time<="2022-05-03 00:00:00")
shortcheck$Date_Time<- c((shortcheck$Date_Time)-hours(6))
SCplot <- qplot(Date_Time, solar_radiation_set_1, data = shortcheck, geom="point") +
  scale_x_datetime(labels = date_format("%H:%M"), 
                   breaks = date_breaks("2 hours")) +theme_bw() 

# write.table(x = Wclim, file = "WestNSclimate.txt", row.names = TRUE)
# write.csv (x = Wclim, file = "./AggRawData/Climate/WestNSclimate.csv", row.names = TRUE)

### East shore Climate dat aggregation:
col_names <- names(read_csv("./AggRawData/E_weather/F9917.2022-10-31.csv",  skip = 10))

climate.temp3 <- read_csv("./AggRawData/E_weather/F9917.2022-09-01.csv", 
                          col_names = col_names,
                          skip=14)
#colnames(climate.temp1)<- col_names
summary(climate.temp3)
names(climate.temp3)
hist(climate.temp3$precip_accum_24_hour_set_1)
hist(climate.temp3$precip_accum_since_local_midnight_set_1)

library(ggplot2)
# Function to create multiple plots in one pane
create_time_series_plots <- function(df, cols_to_plot) {
  
  # Convert timestamp column to datetime
  df$Date_Time <- as.POSIXct(df$Date_Time)
  
  # Create a list to store the plots
  plot_list <- list()
  
  # Loop through the columns to plot
  for (col in cols_to_plot) {
    # Create a plot for each column
    plot <- ggplot(df, aes(x = Date_Time, y = .data[[col]])) +
      geom_point() +
      labs(title = col) +
      theme_minimal()
    # Add the plot to the list
    plot_list[[col]] <- plot
  }
  
  # Return the list of plots
  return(plot_list)
}

# Specify the columns to plot
cols_to_plot <- c(col_names)
# Call the function to create the plots
plots <- create_time_series_plots(climate.temp3, cols_to_plot[c(3:17)])
# Print the plots
print(plots)
grid.arrange(grobs = plots, ncol = 3)

Wplots <- create_time_series_plots(Wclim, cols_to_plot[c(3:17)])
# Print the plots
print(Wplots)
grid.arrange(grobs = Wplots, ncol = 3)


# notes on col_names 
# in general 
# don't need sea level pressure, 
# cardinal direction is a character, wind chill is "na"
# wind_direction set 1 is good to go
# don't need altimiter

climate.temp3$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(climate.temp3$Date_Time, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

climate_E <- climate.temp3 %>%
  dplyr::select(Station_ID,
                datetime,
                air_temp_set_1,
                relative_humidity_set_1,
                wind_speed_set_1,
                wind_direction_set_1,
                wind_gust_set_1,
                precip_accum_since_local_midnight_set_1, # werid 
                solar_radiation_set_1,
                precip_accum_24_hour_set_1,
                dew_point_temperature_set_1d,
                pressure_set_1d,
                heat_index_set_1d)
climate_E$shore<- "east"


Wclim$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(Wclim$Date_Time, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

climate_W <- Wclim %>%
  dplyr::select(Station_ID,
                datetime,
                air_temp_set_1,
                relative_humidity_set_1,
                wind_speed_set_1,
                wind_direction_set_1,
                wind_gust_set_1,
                precip_accum_since_local_midnight_set_1, # werid 
                solar_radiation_set_1,
                precip_accum_24_hour_set_1,
                dew_point_temperature_set_1d,
                pressure_set_1d,
                heat_index_set_1d)
climate_W$shore <- "west"


#weather covariate df with climate_W2, and climate_E
weatherdf<- rbind(climate_W, climate_E)
weatherdf$date <- as.Date(weatherdf$datetime, format="%y-%m-%d")

weatherdf2 <- weatherdf %>% 
  group_by(date, shore) %>% 
  summarise(airT_mean = mean(air_temp_set_1,na.rm = T),
            airT_min = min(air_temp_set_1,na.rm = T),
            airT_max = max(air_temp_set_1,na.rm = T),
            airT_sd = sd(air_temp_set_1,na.rm = T),
            RH_mean = mean(relative_humidity_set_1, na.rm = T),
            RH_sd = sd(relative_humidity_set_1, na.rm = T),
            Windsp_mean = mean(wind_speed_set_1,na.rm = T),
            Windsp_min = min(wind_speed_set_1,na.rm = T),
            Windsp_max = max(wind_speed_set_1,na.rm = T),
            Windsp_sd = sd(wind_speed_set_1,na.rm = T),
            Winddr_mean = mean(wind_direction_set_1,na.rm = T),
            Winddr_min = min(wind_direction_set_1,na.rm = T),
            Winddr_max = max(wind_direction_set_1,na.rm = T),
            Winddr_sd = sd(wind_direction_set_1,na.rm = T),
            Windg_mean = mean(wind_gust_set_1,na.rm = T),
            Windg_min = min(wind_gust_set_1,na.rm = T),
            Windg_max = max(wind_gust_set_1,na.rm = T),
            Windg_sd = sd(wind_gust_set_1,na.rm = T),
            SolarR_mean = mean(solar_radiation_set_1,na.rm = T),
            SolarR_sd = sd(solar_radiation_set_1,na.rm = T),
            DewP_mean = mean(dew_point_temperature_set_1d,na.rm = T),
            DewP_sd = sd(dew_point_temperature_set_1d,na.rm = T),
            Pressure_mean = mean(pressure_set_1d,na.rm = T),
            Pressure_sd = sd(pressure_set_1d,na.rm = T),
            HeatI_mean = mean(heat_index_set_1d,na.rm = T),
            HeatI_sd = sd(heat_index_set_1d,na.rm = T))

# bring in precip
#############
SNOTELdat <- read.csv("./plotDat/SNOTELdat.csv")
SNOTELdat$date <- as.Date(SNOTELdat$date)

SNOTELdat1 <- SNOTELdat %>%
  subset(date > as.Date("2020-09-30") &
           date < as.Date("2023-04-01"))%>%
  select(date,snow_water_equivalent, 
         precipitation_cumulative, precipitation,
         shore)

weatherdf3<-left_join(SNOTELdat1, weatherdf2, 
                     by=c('date'='date', 'shore'='shore'))

### need to left join weather data with NS lake data
# BW1sum <- rbind(BWNS1, BWNS1_22)
# BW3sum <- rbind(BWNS3, BWNS3_22)
# BW2sum <- rbind(BWNS2)
BWNS1_DO<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_BWNS1_2021.csv")
BWNS1_DO$date <- as.Date(BWNS1_DO$yday, origin="2021-01-01")
BWNS1_DO$site <- "BWNS1"
BWNS1_DO$shore <- "west"
range(BWNS1_DO$date)

BWNS1_DO2<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_BWNS1_2022.csv")
BWNS1_DO2$date <- as.Date(BWNS1_DO2$yday, origin="2022-01-01")
BWNS1_DO2$site <- "BWNS1"
BWNS1_DO2$shore <- "west"
range(BWNS1_DO2$date)

BWNS2_DO<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_BWNS2_2021.csv")
BWNS2_DO$date <- as.Date(BWNS2_DO$yday, origin="2021-01-01")
BWNS2_DO$site <- "BWNS2"
BWNS2_DO$shore <- "west"
range(BWNS2_DO$date)

BWNS3_DO<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_BWNS3_2021.csv")
BWNS3_DO$date <- as.Date(BWNS3_DO$yday, origin="2021-01-01")
BWNS3_DO$site <- "BWNS3"
BWNS3_DO$shore <- "west"
range(BWNS3_DO$date)

BWNS3_DO2<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_BWNS3_2022.csv")
BWNS3_DO2$date <- as.Date(BWNS3_DO2$yday, origin="2022-01-01")
BWNS3_DO2$site <- "BWNS3"
BWNS3_DO2$shore <- "west"
range(BWNS3_DO2$date)

westNSDO<- rbind(BWNS1_DO, BWNS1_DO2, BWNS2_DO, BWNS3_DO, BWNS3_DO2)

# East sites
# GB1sum <- rbind(GBNS1)
# GB3sum <- rbind(GBNS3, GBNS3_22)

GBNS1_DO<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_GBNS1_2021.csv")
GBNS1_DO$date <- as.Date(GBNS1_DO$yday, origin="2021-01-01")
GBNS1_DO$site <- "GBNS1"
GBNS1_DO$shore <- "east"
range(GBNS1_DO$date)

GBNS3_DO<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_GBNS3_2021.csv")
GBNS3_DO$date <- as.Date(GBNS3_DO$yday, origin="2021-01-01")
GBNS3_DO$site <- "GBNS1"
GBNS3_DO$shore <- "east"
range(GBNS3_DO$date)


GBNS3_DO2<- read_csv("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/Sondefiles/sonde_prep_GBNS3_2022.csv")
GBNS3_DO2$date <- as.Date(GBNS3_DO2$yday, origin="2022-01-01")
GBNS3_DO2$site <- "GBNS1"
GBNS3_DO2$shore <- "east"
range(GBNS3_DO2$date)

eastNSDO<- rbind(GBNS1_DO, GBNS3_DO, GBNS3_DO2)

NS_DO <- rbind(eastNSDO, westNSDO)

NS_DO_df <- NS_DO %>% 
  group_by(date, shore) %>%
  dplyr::summarise(
    DO_mean = mean(do, na.rm=T),
    DO_sd = sd(do,  na.rm=T),
    par_mean = mean(par, na.rm=T),
    par_sd = sd(par,  na.rm=T),
    wtemp_mean = mean(wtemp, na.rm=T),
    wtemp_sd = sd(wtemp,  na.rm=T))

## join DO and weather 
NS_DO_df2<-left_join(weatherdf3, NS_DO_df, 
                      by=c('date'='date', 'shore'='shore'))
NS_DO_df3<-left_join(NS_DO_df2, Streamdf, 
                     by=c('date'='datetime', 'shore'='shore'))

Streamdf


NS_GPP_df1<-left_join(NS_DO_df3, NS_GPP_df[,c("GPP_mean", "date", "shore")], 
                     by=c('date'='date', 'shore'='shore'))

Streamdf


NS_ER_df1<-left_join(NS_DO_df3, NS_ER_df[,c("ER_mean", "date", "shore")], 
                      by=c('date'='date', 'shore'='shore'))

Streamdf

head(NS_DO_df2)
dim(NS_DO_df2)

## 
## 
# Need to break stuff out by stream data, weather station, environmental data








RF_temp1<- na.omit(NS_DO_df3[,c(-1,-5,-26,-27, -30,-31,-33, -39)])
str(RF_temp1)

RF_temp2<- na.omit(NS_GPP_df1[,c(-1,-5,-26,-27, -30,-31,-32, -33, -39)])
str(RF_temp2)
RF_temp2 <- RF_temp2 %>% 
  subset(GPP_mean>0)
summary(RF_temp2)


hist_gpp <- ggplot(NS_GPP_df1, aes(x = GPP_mean, fill=shore)) +
  geom_histogram(alpha = 0.8) + theme_bw() +
  scale_fill_manual(values=c("#a67d17", "#3283a8")) +
  xlab("GPP")

hist_ER <- ggplot(NS_ER_df, aes(x = ER_mean, fill=shore)) +
  geom_histogram(alpha = 0.8) + theme_bw() +
  scale_fill_manual(values=c("#a67d17", "#3283a8")) +
  xlab("ER")
 

all_grid <- ggarrange(hist_gpp,
                      hist_ER,
                      ncol = 2, nrow = 1,
                      #widths = c(1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")


# ggsave(plot = all_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_Metabresponse.png",sep=""),width=5,height=2.5,dpi=300)


RF_temp4<- na.omit(NS_ER_df1[,c(-1,-5,-26,-27, -30,-31,-32, -33, -39)])
str(RF_temp4)


# time to wrangle in chemistry. 




set.seed(111)
#boruta <- Boruta(vwc ~ ., data = lagData[,c(-1)], doTrace = 2, maxRuns = 500)
#print(boruta)

library(Boruta)
library(mlbench)
library(randomForest)

boruta1 <- Boruta(RF_temp1$DO_mean ~ ., data = RF_temp1[,c(-1,-5,-26,-27, -30,-31,-32, -33, -39)], doTrace = 2, maxRuns = 500)
print(boruta1)

boruta3 <- Boruta(RF_temp2$GPP_mean ~ ., data = RF_temp2[,c(-44)], doTrace = 2, maxRuns = 500)
print(boruta3)

boruta4 <- Boruta(RF_temp4$ER_mean ~ ., data = RF_temp4[,c(-44)], doTrace = 2, maxRuns = 500)
print(boruta4)



png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/RF_DO.png', width = 1000, height = 1000, res = 150)
plot(boruta1, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta1$ImpHistory),function(i)
  boruta1$ImpHistory[is.finite(boruta1$ImpHistory[,i]),i])
names(lz) <- colnames(boruta1$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta1$ImpHistory), cex.axis = 0.45)
dev.off()


png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/RF_GPP.png', width = 1000, height = 1000, res = 150)
plot(boruta3, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta3$ImpHistory),function(i)
  boruta3$ImpHistory[is.finite(boruta3$ImpHistory[,i]),i])
names(lz) <- colnames(boruta3$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta3$ImpHistory), cex.axis = 0.45)
dev.off()


png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/RF_ER.png', width = 1000, height = 1000, res = 150)
plot(boruta4, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta4$ImpHistory),function(i)
  boruta4$ImpHistory[is.finite(boruta4$ImpHistory[,i]),i])
names(lz) <- colnames(boruta4$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta4$ImpHistory), cex.axis = 0.45)
dev.off()


p5 <- ggplot(data = NS_DO_df2, aes(date, DO_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = DO_mean-DO_sd, ymax = DO_mean+DO_sd, fill = shore),
              linetype = 0, alpha = 0.2) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(7, 9.5) +
  labs(y=expression("DO mgL")) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 

#ggsave(plot = p5, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_DO.png",sep=""),width=7,height=5,dpi=300)


p <- ggplot(data = NS_DO_df2, aes(date, wtemp_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = wtemp_mean-wtemp_sd, ymax = wtemp_mean+wtemp_sd, fill = shore),
              linetype = 0, alpha = 0.2) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(4.5, 23) +
  labs(y=expression("Water temperature (C)")) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 

#ggsave(plot = p, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_Wtemp.png",sep=""),width=7,height=5,dpi=300)

p7 <- ggplot(data = weatherdf3, aes(date, Windsp_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = Windsp_mean-Windsp_sd, ymax = Windsp_mean+Windsp_sd, fill = shore),
              linetype = 0, alpha = 0.2) +
  geom_point(aes(color = shore),alpha = 0.5) +
  geom_line()+
  #ylim(4.5, 23) +
  #labs(y=expression("Water temperature (C)")) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p7

p7 <- ggplot(data = weatherdf3, aes(date, airT_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = airT_mean-airT_sd, ymax = airT_mean+airT_sd, fill = shore),
              linetype = 0, alpha = 0.2) +
  geom_point(aes(color = shore),alpha = 0.5) +
  geom_line()+
  #ylim(4.5, 23) +
  #labs(y=expression("Water temperature (C)")) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p7


p7 <- ggplot(data = weatherdf3, aes(date, precipitation, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  # geom_ribbon(aes(ymin = Precip24_mean-Precip24_sd, ymax = Precip24_mean+Precip24_sd, fill = shore),
  #             linetype = 0, alpha = 0.2) +
  geom_line(aes(color = shore),alpha = 0.8) +
  #geom_line()+
  #ylim(4.5, 23) +
  #labs(y=expression("Water temperature (C)")) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p7


p7 <- ggplot(data = weatherdf3, aes(date, snow_water_equivalent, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  # geom_ribbon(aes(ymin = Precip24_mean-Precip24_sd, ymax = Precip24_mean+Precip24_sd, fill = shore),
  #             linetype = 0, alpha = 0.2) +
  geom_line(aes(color = shore),alpha = 0.8) +
  #geom_line()+
  #ylim(4.5, 23) +
  #labs(y=expression("Water temperature (C)")) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y")+
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p7




p6 <- ggplot(data = weatherdf3, aes(date, precipitation_cumulative, color = shore))+
  # geom_ribbon(aes(ymin = Precip24_mean-Precip24_sd, ymax = Precip24_mean+Precip24_sd, fill = shore),
  #             linetype = 0, alpha = 0.2) +
  geom_line(aes(color = shore),alpha = 0.8) +
  #geom_line()+
  #ylim(4.5, 23) +
  labs(y=expression("Cumulative precipitation (mm)"), x="") + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b-%y")+
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p6


p5 <- ggplot(data = weatherdf3, aes(date, snow_water_equivalent, color = shore))+
  # geom_ribbon(aes(ymin = Precip24_mean-Precip24_sd, ymax = Precip24_mean+Precip24_sd, fill = shore),
  #             linetype = 0, alpha = 0.2) +
  geom_line(aes(color = shore),alpha = 0.8) +
  #geom_line()+
  #ylim(4.5, 23) +
  labs(y=expression("SWE (mm)"), x="") + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b-%y")+
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p5



p7 <- ggplot(data = weatherdf3, aes(date, precipitation, color = shore))+
  # geom_ribbon(aes(ymin = Precip24_mean-Precip24_sd, ymax = Precip24_mean+Precip24_sd, fill = shore),
  #             linetype = 0, alpha = 0.2) +
  geom_line(aes(color = shore),alpha = 0.8) +
  #geom_line()+
  #ylim(4.5, 23) +
  labs(y=expression("Precipitation (mm)"), x="") + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b-%y")+
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p7



# total grid
library(gridExtra)
library(ggpubr)

all_grid <- ggarrange(p7,
                      p6,
                      p5,
                      ncol = 1, nrow = 3,
                      widths = c(1,1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")


# ggsave(plot = all_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_HydroClim.png",sep=""),width=12,height=8.5,dpi=300)

###
library(Boruta)
library(mlbench)
library(randomForest)

set.seed(111)
#boruta <- Boruta(vwc ~ ., data = lagData[,c(-1)], doTrace = 2, maxRuns = 500)
#print(boruta)

boruta2 <- Boruta(vwcInterval ~ ., data = lagData[,c(-1:-4)], doTrace = 2, maxRuns = 500)
print(boruta2)

plot(boruta2, las = 2, cex.axis = 0.5)



# left join in nearhore DO with weatherr data











#######################
#######################
## add in stream metab
BWL <- read.csv("./plotDat/BWL_daily.csv")
BWL$date <- as.Date(BWL$date, origin="2021-01-01")
BWL$site <- "BWL"
BWL$shore <- "west"


GBL <- read.csv("./plotDat/GBL_daily.csv")
GBL$date <- as.Date(GBL$date, origin="2021-01-01")
GBL$site <- "GBL"
GBL$shore <- "east"
