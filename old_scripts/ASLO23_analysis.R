# setwd("/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/")

library(tidyverse)
library(lubridate)
#plotting packages:
library(ggplot2)
library(cowplot)
library(reshape2)
library(scales)
library(gridExtra)
library(dataRetrieval)

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
  geom_line(alpha = 0.5)+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = GPPL_mean, ymax = GPPU_mean, fill = shore),
              linetype = 0, alpha = 0.3) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(-1, 38) +
  labs(y=expression("GPP"~(mmol~O[2]~m^-3~d^-1))) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  theme(axis.title.x=element_blank()) 

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
  geom_line(alpha = 0.5)+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = ERL_mean, ymax = ERU_mean, fill = shore),
              linetype = 0, alpha = 0.3) +
  geom_point(aes(color = shore),alpha = 0.5) +
  ylim(-38, 1) +
  labs(y=expression("ER"~(mmol~O[2]~m^-3~d^-1))) + 
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  theme(axis.title.x=element_blank()) 
p3


SER_grid <- ggarrange(p2,
                      p3,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")


# ggsave(plot = SER_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_ER.png",sep=""),width=12,height=6,dpi=300)

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
GBL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_GBL_SPC.csv")
GBL22_SPC$solar.time <- as.POSIXct(GBL22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
GBL22_SPC$site<- "GBL"
GBL22_SPC$shore<- "east"

BWL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_BWL_SPC.csv")
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
BW_DO <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/23_BWLmodelInputs.csv")
BW_DO$shore <- "west"
GB_DO <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/23_GBL_modelInputs.csv")
#GB_DO1<- GB_DO[,c(-2)]
GB_DO$shore <- "east"

streamDO<- rbind(BW_DO,GB_DO)
summary(streamDO)
streamDO <- streamDO %>%
  mutate(solar.time=as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S",
                                     tz = "UTC"),
         date=as.Date(solar.time, format= "%Y-%m-%d"))
summary(streamDO)

  
streamDO <- streamDO[!is.na(streamDO$date), ]

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
Streamdf_cor <- Streamdf[, c(2,5:17)]
chart.Correlation(Streamdf_cor, histogram=TRUE, pch=19)

#################
# weather data ##

Wclim <- read_csv("./AggRawData/W_weather/WestNSclimate_23.csv") %>%
  mutate(Date_Time=as.POSIXct(Date_Time, format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC"),
         date=as.Date(Date_Time, format= "%Y-%m-%d"))

summary(Wclim)
hist(Wclim$air_temp_set_1) # bad


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
climate.temp3 <- read_csv("./AggRawData/E_weather/EastNSclimate_23.csv") %>%
  mutate(Date_Time=as.POSIXct(Date_Time, format = "%Y-%m-%d %H:%M:%S",
                              tz = "UTC"),
         date=as.Date(Date_Time, format= "%Y-%m-%d"))
  
#colnames(climate.temp1)<- col_names
summary(climate.temp3)
names(climate.temp3)
hist(climate.temp3$air_temp_set_1)

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
                solar_radiation_set_1,
                dew_point_temperature_set_1d,
                pressure_set_1d,
                heat_index_set_1d,
                wind_cardinal_direction_set_1d)
climate_E$shore<- "east"

Wclim$datetime <- (as.POSIXct(round_date(
  as.POSIXct(Wclim$Date_Time, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))


climate_W <- Wclim %>%
  dplyr::select(Station_ID,
                datetime,
                air_temp_set_1,
                relative_humidity_set_1,
                wind_speed_set_1,
                wind_direction_set_1,
                wind_gust_set_1,
                solar_radiation_set_1,
                dew_point_temperature_set_1d,
                pressure_set_1d,
                heat_index_set_1d,
                wind_cardinal_direction_set_1d)
climate_W$shore <- "west"

#weather covariate df with climate_W2, and climate_E
weatherdf<- rbind(climate_W, climate_E)
weatherdf$date <- as.Date(weatherdf$datetime, format="%y-%m-%d")

weatherdf$sin_direction<- sin(weatherdf$wind_direction_set_1 * pi / 180)
weatherdf$cos_direction<- cos(weatherdf$wind_direction_set_1 * pi / 180)


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
            Winddr_sin = mean(sin_direction,na.rm = T),
            Winddr_sin_sd = sd(sin_direction,na.rm = T),
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


## plot 

tempP_p <- ggplot(weatherdf, aes(x=datetime, y=wind_cardinal_direction_set_1d, color =(shore))) + 
  geom_point(size=1) +  geom_line(size=0.95)


tempP_p <- ggplot(weatherdf2, aes(x=date, y=Winddr_mean, color =(shore))) + 
  geom_point(size=1) +  geom_line(size=0.95) + 
  # geom_errorbar(aes(ymin = temp_mean - temp_se, ymax = temp_mean + temp_se), 
  #               width = 0, alpha = 0.7, position = position_dodge(width = 0.5)) +
  scale_color_manual(values=alpha(c("#3283a8", "#a67d17"),0.6)) + # 33637d
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "4 month") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) 




tempP_plot <- ggplot(weatherdf2, aes(x=date, y=Windsp_mean, color =(shore))) + 
  geom_point(size=1) +  geom_line(size=0.95)+ 
  labs(x =NULL, y = "Wind speed (m/s)") +
  # geom_errorbar(aes(ymin = temp_mean - temp_se, ymax = temp_mean + temp_se), 
  #               width = 0, alpha = 0.7, position = position_dodge(width = 0.5)) +
  scale_color_manual(values=alpha(c("#3283a8", "#a67d17"),0.6)) + # 33637d
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "4 month") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) 

# ggsave(plot = tempP_plot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/CH2_wind.png",sep=""),width=10,height=3,dpi=300)







# bring in precip
#############
SNOTELdat <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/23_precip.csv")
SNOTELdat$date <- as.Date(SNOTELdat$date)
range(SNOTELdat$date)

SNOTELdat1 <- SNOTELdat %>% 
  rename(shore='label') %>%
  subset(date > as.Date("2021-03-01") &
           date < as.Date("2023-09-28"))%>%
  dplyr::select(date,snow_water_equivalent, precipitation_cumulative, precipitation, shore)

weatherdf3<-left_join(SNOTELdat1, weatherdf2, 
                     by=c('date'='date', 'shore'='shore'))

### need to left join weather data with NS lake data

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
summary(NS_DO_df3)

NS_GPP_df1<-left_join(NS_DO_df3, NS_GPP_df[,c("GPP_mean", "GPP_sd", "date", "shore")], 
                     by=c('date'='date', 'shore'='shore'))

NS_GPP_df1


NS_ER_df1<-left_join(NS_DO_df3, NS_ER_df[,c("ER_mean", "ER_sd", "date", "shore")], 
                      by=c('date'='date', 'shore'='shore'))

Streamdf

head(NS_DO_df2)
dim(NS_DO_df2)

NS_metab_df1<-left_join(NS_GPP_df1, NS_ER_df[,c("ER_mean", "ER_sd", "date", "shore")], 
                     by=c('date'='date', 'shore'='shore'))



## 
## 
# Need to break stuff out by stream data, weather station, environmental data


##
metab_NS2 <- left_join(NS_GPP_df1, NS_ER_df1[c("date","shore","ER_mean", "ER_sd")], by=c('date'='date', 'shore'='shore'))
##
mod<- lmer(ER_mean~GPP_mean + (1|shore), data=NS_metab_df1)
summary(mod)

NS_metab_dfe<- NS_metab_df1%>%
  filter(shore=="east")
summary(NS_metab_dfe)

NS_metab_dfw<- NS_metab_df1%>%
  filter(shore=="west")
summary(NS_metab_dfw)

mod<- lm(ER_mean~GPP_mean, data=NS_metab_dfe)
summary(mod)

mod<- lm(ER_mean~GPP_mean, data=NS_metab_dfw)
summary(mod)
library(MuMIn)
r.squaredGLMM(mod)

NEP_plt_ST <- ggplot(metab_NS2, aes(x = GPP_mean, y=ER_mean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_mean - ER_sd, 
                     ymax = ER_mean + ER_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = GPP_mean - GPP_sd, 
                    xmax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("ER"~mmol~O[2]~m^-3~d^-1), x=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.5)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18))+
  annotate("text",
           y = max(na.omit(metab_NS2$ER_mean)),  # Adjust the x and y coordinates as needed
           x = max(na.omit(metab_NS2$GPP_mean)),
           label = expression("BW"~R^2~"=0.87"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  ) +
  annotate("text",
           y = max(na.omit(metab_NS2$ER_mean)-5),  # Adjust the x and y coordinates as needed
           x = max(na.omit(metab_NS2$GPP_mean)),
           label = expression("GB"~R^2~"=0.89"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  )
  



mod<- lm(ER_mean~wtemp_mean, data=NS_metab_dfe)
summary(mod)

mod<- lm(ER_mean~wtemp_mean, data=NS_metab_dfw)
summary(mod)
r.squaredGLMM(mod)

NEP_plt_T <- ggplot(NS_metab_df1, aes(x = wtemp_mean, y=ER_mean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_mean - ER_sd, 
                    ymax = ER_mean + ER_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = wtemp_mean - wtemp_sd,
                     xmax = wtemp_mean + wtemp_sd), alpha=c(0.5)) +
  labs(y=expression("ER"~mmol~O[2]~m^-3~d^-1), x="Water temp. (C)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.5)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  annotate("text",
           y = min(na.omit(NS_metab_df1$ER_mean)+5),  # Adjust the x and y coordinates as needed
           x = min(na.omit(NS_metab_df1$wtemp_mean)+7),
           label = expression("BW"~R^2~"=0.28"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  ) +
  annotate("text",
           y = min(na.omit(NS_metab_df1$ER_mean)),  # Adjust the x and y coordinates as needed
           x = min(na.omit(NS_metab_df1$wtemp_mean)+7),
           label = expression("GB"~R^2~"=0.66"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  )



mod<- lm(GPP_mean~Windsp_mean, data=NS_metab_dfe)
summary(mod)

mod<- lm(GPP_mean~Windsp_mean, data=NS_metab_dfw)
summary(mod)
r.squaredGLMM(mod)

NEP_plt_wind <- ggplot(NS_metab_df1, aes(y = GPP_mean, x=Windsp_mean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = Windsp_mean - Windsp_sd,
                     xmax = Windsp_mean + Windsp_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x=expression("Wind speed"~(m~s^-1))) + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  annotate("text",
           y = max(na.omit(NS_metab_df1$GPP_mean)),  # Adjust the x and y coordinates as needed
           x = max(na.omit(NS_metab_df1$Windsp_mean)+3),
           label = expression("BW"~R^2~"=0.00"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  ) +
  annotate("text",
           y = max(na.omit(NS_metab_df1$GPP_mean)-5),  # Adjust the x and y coordinates as needed
           x = max(na.omit(NS_metab_df1$Windsp_mean)+3),
           label = expression("GB"~R^2~"=0.12"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  )


##
all_grid <- ggarrange(NEP_plt_ST,
                      NEP_plt_T,
                      NEP_plt_wind,
                      ncol = 3, nrow = 1,
                      #widths = c(1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")

# ggsave(plot = all_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_lakemeDiag.png",sep=""),width=11,height=3.5,dpi=300)

##






RF_temp1<- na.omit(NS_DO_df3[,c(-1,-5,-26,-27, -30,-31,-33, -39)])
str(RF_temp1)

names(NS_GPP_df1)

RF_temp2<- na.omit(NS_GPP_df1[,c(-1,-5,-26,-27, -30,-31,-32, -33, -39)])
str(RF_temp2)

StreamP_RF_GPP <- NS_GPP_df1 %>% 
  subset(GPP_mean>0) %>%
  dplyr::select(date, shore, dischargeCFS, wtemp_mean, wtemp_sd, 
                Stream_SPCmean, Stream_SPCstd, 
                Stream_Wtmean, Stream_Wtstd, 
                stage_ftm, stage_sd, Stream_DOobs_mean,
                Stream_DOobs_sd, Stream_DOsat_mean, Stream_DOsat_sd,
                Stream_depth_mean, Stream_depth_sd, Stream_light_mean, 
                GPP_mean, GPP_sd) 
summary(StreamP_RF_GPP)


StreamP_RF_ER <- NS_ER_df1 %>% 
  subset(ER_mean<0) %>%
  dplyr::select(date, shore, dischargeCFS, wtemp_mean, wtemp_sd, 
                Stream_SPCmean, Stream_SPCstd, 
                Stream_Wtmean, Stream_Wtstd, 
                stage_ftm, stage_sd, Stream_DOobs_mean,
                Stream_DOobs_sd, Stream_DOsat_mean, Stream_DOsat_sd,
                Stream_depth_mean, Stream_depth_sd, Stream_light_mean, 
                ER_mean, ER_sd) 
summary(StreamP_RF_ER)


hist_gpp <- ggplot(StreamP_RF_GPP, aes(x = GPP_mean, fill=shore)) +
  geom_histogram(alpha = 0.8) + theme_bw() +
  scale_fill_manual(values=c("#a67d17", "#3283a8")) +
  xlab("GPP")

hist_ER <- ggplot(StreamP_RF_ER, aes(x = ER_mean, fill=shore)) +
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

## GPP stream mdoel ##
Streamdf_cor <- StreamP_RF_GPP[, c(3:18,21)]
chart.Correlation(Streamdf_cor, histogram=TRUE, pch=19)

StreamP_RF_GPP$TempDif <- c(StreamP_RF_GPP$wtemp_mean-StreamP_RF_GPP$Stream_Wtmean)
hist(log(StreamP_RF_GPP$GPP_mean+1))

SG_mod_null <- lmer(log(GPP_mean+1) ~(1|shore), data= StreamP_RF_GPP)
summary(SG_mod_null)

SG_mod_1 <- lmer(log(GPP_mean+1) ~ 
                   #scale(TempDif) + 
                    #scale(dischargeCFS) +
                   #scale(Stream_SPCmean) +
                   #scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_GPP)
summary(SG_mod_1)
hist(residuals(SG_mod_1))


SG_mod_2 <- lmer(log(GPP_mean+1) ~ 
                   scale(TempDif) + 
                   #scale(dischargeCFS) +
                   #scale(Stream_SPCmean) +
                   #scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_GPP)
summary(SG_mod_2)
hist(residuals(SG_mod_2))


SG_mod_3 <- lmer(log(GPP_mean+1) ~ 
                   #scale(TempDif) + 
                   scale(dischargeCFS) +
                   #scale(Stream_SPCmean) +
                   #scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_GPP)
summary(SG_mod_3)
hist(residuals(SG_mod_3))
r.squaredGLMM(SG_mod_3)


SG_mod_4 <- lmer(log(GPP_mean+1) ~ 
                   #scale(TempDif) + 
                   #scale(dischargeCFS) +
                   scale(Stream_SPCmean) +
                   #scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_GPP)
summary(SG_mod_4)
hist(residuals(SG_mod_4))
r.squaredGLMM(SG_mod_4)

SG_mod_5 <- lmer(log(GPP_mean+1) ~ 
                   #scale(TempDif) + 
                   #scale(dischargeCFS) +
                   #scale(Stream_SPCmean) +
                   scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_GPP)
summary(SG_mod_5)
hist(residuals(SG_mod_5))
r.squaredGLMM(SG_mod_5)


SG_mod_6 <- lmer(log(GPP_mean+1) ~ 
                  #scale(TempDif) + 
                   scale(log(dischargeCFS)+1) +
                   scale(Stream_SPCmean) +
                   scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_GPP)
summary(SG_mod_6)
r.squaredGLMM(SG_mod_6)
hist(residuals(SG_mod_6))

coef_table <- summary(SG_mod_6)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(SG_mod_6),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(dischargeCFS)"] <- "Discharge"
  RowNames[RowNames == "scale(Stream_SPCmean)"] <- "SPC"
  RowNames[RowNames == "scale(Stream_Wtmean)"] <- "Stream temp."
})


coeffplotGP <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter", title=expression("GLMM: GPP"~(mmol~O[2]~m^-3~d^-1))) +
  theme_bw() +
  annotate(
    "text",
    x = max(na.omit(plot_data2$Estimate)),  # Adjust the x and y coordinates as needed
    y = c(1.2),
    label = expression(R^2~"=0.64"), 
    parse = TRUE,
    hjust = 1,  # Adjust horizontal justification
    vjust = 1   # Adjust vertical justification
  )

# ggsave(plot = coeffplotGP, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_GPP_stream.png",sep=""),width=3.75,height=2.25,dpi=300)


AIC(SG_mod_null, SG_mod_1, SG_mod_2, SG_mod_3, SG_mod_4, SG_mod_5, SG_mod_6)


gpp_plt_ST <- ggplot(StreamP_RF_GPP, aes(y = GPP_mean, x=Stream_Wtmean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
    geom_errorbarh(aes(xmin = Stream_Wtmean - Stream_Wtstd, 
                       xmax = Stream_Wtmean + Stream_Wtstd), alpha=c(0.2)) +
    geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                      ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="Stream temp. (C)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          plot.subtitle = element_text(size = 18)) 

gpp_plt2_SSPC <- ggplot(StreamP_RF_GPP, aes(y = GPP_mean, x=Stream_SPCmean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbarh(aes(xmin = Stream_SPCmean - Stream_SPCstd, 
                     xmax = Stream_SPCmean + Stream_SPCstd), alpha=c(0.2)) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="Stream SPC") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 

gpp_plt2_Q <- ggplot(StreamP_RF_GPP, aes(y = GPP_mean, x=log(dischargeCFS+1), color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F, lty=2) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="log(Q+1)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 

SGPP_grid <- ggarrange(gpp_plt_ST,
                      gpp_plt2_SSPC,
                      gpp_plt2_Q,
                      ncol = 3, nrow = 1,
                      #widths = c(1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")

# ggsave(plot = SGPP_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_GPP_stream.png",sep=""),width=12,height=4,dpi=300)


# bring in stream metabolism:

metabdat <- list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/23_output/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()
names(metabdat)
summary(metabdat)

metdat <- metabdat %>%
  mutate(date= as.Date(date, format="%m/%d/%y")) %>%
  rename(GPP_meanS= "GPP_mean", GPP_sdS="GPP_sd", ER_meanS="ER_mean", ER_sdS="ER_sd")%>%
  filter(Site=="BWL" | Site=="GBL")%>%
  dplyr::select(Site, shore, date, 
                GPP_meanS, GPP_sdS,
                ER_meanS, ER_sdS)
summary(metdat)

unique(metdat$Site)

StreamEco_GPP <- StreamP_RF_GPP%>%
  left_join(metdat)

StreamEco_GPP_w <- StreamEco_GPP %>%
  filter(shore=="west")

SG_mod <- lm(log(GPP_mean+1) ~
                   scale(GPP_meanS), data= StreamEco_GPP_w)
summary(SG_mod)
r.squaredGLMM(SG_mod)
hist(residuals(SG_mod))

StreamEco_GPP$GPP_mean_g<- (StreamEco_GPP$GPP_mean/15.9994)

gpp_gpp_w <- ggplot(StreamEco_GPP_w, aes(y = GPP_mean, x=GPP_meanS, color=shore)) +
  geom_point(alpha = 0.8) + ylim(0,30)+
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd,
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = GPP_meanS - GPP_sdS,
                    xmax = GPP_meanS + GPP_sdS), alpha=c(0.5)) +
  labs(y=expression("GPP"~(mmol~O[2]~m^-3~d^-1)), x=expression("Stream GPP"~(g~O[2]~m^-2~d^-1))) + 
  scale_color_manual(values=alpha(c("#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  annotate("text",
    x = max(na.omit(StreamEco_GPP_w$GPP_meanS)),  # Adjust the x and y coordinates as needed
    y = max(na.omit(StreamEco_GPP_w$GPP_mean)),
    label = expression(R^2~"=0.16"), 
    parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  )


StreamEco_GPP_e <- StreamEco_GPP %>%
  filter(shore=="east")

SG_mod <- lm(log(GPP_mean+1) ~
               scale(GPP_meanS), data= StreamEco_GPP_e)
summary(SG_mod)
r.squaredGLMM(SG_mod)
hist(residuals(SG_mod))

gpp_gpp_e <- ggplot(StreamEco_GPP_e, aes(y = GPP_mean, x=GPP_meanS, color=shore)) +
  geom_point(alpha = 0.8) + ylim(0,30)+
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd,
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = GPP_meanS - GPP_sdS,
                    xmax = GPP_meanS + GPP_sdS), alpha=c(0.5)) +
  labs(y=expression("GPP"~(mmol~O[2]~m^-3~d^-1)), x=expression("Stream GPP"~(g~O[2]~m^-2~d^-1))) + 
  scale_color_manual(values=alpha(c("#a67d17"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  annotate("text",
           x = max(na.omit(StreamEco_GPP_e$GPP_meanS)),  # Adjust the x and y coordinates as needed
           y = max(na.omit(StreamEco_GPP_w$GPP_mean)),
           label = expression(R^2~"=0.18"), 
           parse = TRUE,  hjust = 1,  vjust = 1   # Adjust vertical justification
  )


StreamEco_ER <- StreamP_RF_ER%>%
  left_join(metdat)

StreamEco_ER$ER_meanPos <- StreamEco_ER$ER_mean * c(-1)
StreamEco_ER$ER_meanSpos <- StreamEco_ER$ER_meanS * c(-1)


StreamEco_ER_w <- StreamEco_ER %>%
  filter(shore=="west")

SG_mod <- lm((ER_meanPos) ~
               scale(ER_meanSpos), data= StreamEco_ER_w)
summary(SG_mod)
r.squaredGLMM(SG_mod)
hist(residuals(SG_mod))

er_er_w <- ggplot(StreamEco_ER_w, aes(y = ER_meanPos, x=ER_meanSpos, color=shore)) +
  geom_point(alpha = 0.8) + ylim(0, 25)+
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd,
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = ER_meanSpos - ER_sdS,
                    xmax = ER_meanSpos + ER_sdS), alpha=c(0.5)) +
  labs(y=expression("|ER|"~(mmol~O[2]~m^-3~d^-1)), x=expression("Stream |ER|"~(g~O[2]~m^-2~d^-1))) + 
  scale_color_manual(values=alpha(c("#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  annotate("text",
           x = max(na.omit(StreamEco_ER_w$ER_meanSpos)),  # Adjust the x and y coordinates as needed
           y = max(na.omit(StreamEco_ER_w$ER_meanPos)-4),
           label = expression(R^2~"=0.09"), 
           parse = TRUE,  hjust = 1,  vjust = 1)


StreamEco_ER_e <- StreamEco_ER %>%
  filter(shore=="east")

SG_mod <- lm((ER_meanPos) ~
               scale(ER_meanSpos), data= StreamEco_ER_e)
summary(SG_mod)
r.squaredGLMM(SG_mod)
hist(residuals(SG_mod))

er_er_e <- ggplot(StreamEco_ER_e, aes(y = ER_meanPos, x=ER_meanSpos, color=shore)) +
  geom_point(alpha = 0.8) + ylim(0, 25)+
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd,
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = ER_meanSpos - ER_sdS,
                    xmax = ER_meanSpos + ER_sdS), alpha=c(0.5)) +
  labs(y=expression("|ER|"~(mmol~O[2]~m^-3~d^-1)), x=expression("Stream |ER|"~(g~O[2]~m^-2~d^-1))) + 
  scale_color_manual(values=alpha(c("#a67d17"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  annotate("text",
           x = max(na.omit(StreamEco_ER_e$ER_meanSpos)),  # Adjust the x and y coordinates as needed
           y = max(na.omit(StreamEco_ER_e$ER_meanPos)+5),
           label = expression(R^2~"=0.44"), 
           parse = TRUE,  hjust = 1,  vjust = 1)



SGPP_grid <- ggarrange(gpp_gpp_w,
                       gpp_gpp_e,
                       er_er_w,
                       er_er_e,
                       ncol = 2, nrow = 2,
                       #widths = c(1, 0.7),
                       common.legend = TRUE,
                       legend = "bottom")

# ggsave(plot = SGPP_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_Metab_Metab.png",sep=""),width=7,height=7,dpi=300)





















### make a GPP grid ##
#########
#########
StreamP_RF_ER$TempDif <- c(StreamP_RF_ER$wtemp_mean-StreamP_RF_ER$Stream_Wtmean)


hist(StreamP_RF_ER$ER_mean)
SE_mod_null <- lmer(ER_mean ~ 
                   (1|shore), data= StreamP_RF_ER)
summary(SE_mod_null)
hist(residuals(SE_mod_null))

StreamP_RF_ER$ER_meanPos <- StreamP_RF_ER$ER_mean * c(-1)

SE_mod_1 <- lmer(ER_meanPos~ 
                      #scale(TempDif) + 
                      scale(log(dischargeCFS+1)) +
                      scale(Stream_SPCmean) +
                      scale(Stream_Wtmean)+
                      (1|shore), data= StreamP_RF_ER)
summary(SE_mod_1)
hist(residuals(SE_mod_1))

SE_mod_2 <- lmer(ER_meanPos~ 
                   scale(log(dischargeCFS+1)) +
                   scale(Stream_SPCmean) +
                   scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_ER)
summary(SE_mod_2)
hist(residuals(SE_mod_2))
r.squaredGLMM(SE_mod_2)

coef_table <- summary(SE_mod_2)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(SE_mod_2),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(log(dischargeCFS + 1))"] <- "Discharge"
  RowNames[RowNames == "scale(Stream_SPCmean)"] <- "SPC"
  RowNames[RowNames == "scale(Stream_Wtmean)"] <- "Stream temp."
})


coeffplotGP <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter", title=expression("GLMM: |ER|"~(mmol~O[2]~m^-3~d^-1))) +
  theme_bw() +
  annotate(
    "text",
    x = max(na.omit(plot_data2$Estimate)),  # Adjust the x and y coordinates as needed
    y = c(3.2),
    label = expression(R^2~"=0.36"), 
    parse = TRUE,
    hjust = 0.5,  # Adjust horizontal justification
    vjust = 1   # Adjust vertical justification
  )

# ggsave(plot = coeffplotGP, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_ER_stream.png",sep=""),width=3.5,height=2.25,dpi=300)



SE_mod_3 <- lmer(ER_mean~ 
                   #scale(TempDif) + 
                   scale(dischargeCFS) +
                   #scale(Stream_SPCmean) +
                   #scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_ER)
summary(SE_mod_3)
hist(residuals(SE_mod_3))
r.squaredGLMM(SE_mod_3)

SE_mod_4 <- lmer(ER_mean~ 
                   #scale(TempDif) + 
                   #scale(dischargeCFS) +
                   scale(Stream_SPCmean) +
                   #scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_ER)
summary(SE_mod_4)
hist(residuals(SE_mod_4))
r.squaredGLMM(SE_mod_4)

SE_mod_5 <- lmer(ER_mean~ 
                   #scale(TempDif) + 
                   #scale(dischargeCFS) +
                   #scale(Stream_SPCmean) +
                   scale(Stream_Wtmean)+
                   (1|shore), data= StreamP_RF_ER)
summary(SE_mod_5)
r.squaredGLMM(SE_mod_5)
hist(residuals(SE_mod_5))

AIC(SE_mod_5, SE_mod_4, SE_mod_3, SE_mod_2, SE_mod_1, SE_mod_null)


ER_plt_STemp <- ggplot(StreamP_RF_ER, aes(y = ER_meanPos, x=Stream_Wtmean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbarh(aes(xmin = Stream_Wtmean - Stream_Wtstd, 
                     xmax = Stream_Wtmean + Stream_Wtstd), alpha=c(0.2)) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="Stream temp. (C)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


ER_plt_ST <- ggplot(StreamP_RF_ER, aes(y = ER_meanPos, x=Stream_SPCmean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = Stream_SPCmean - Stream_SPCstd, 
                     xmax = Stream_SPCmean + Stream_SPCstd), alpha=c(0.2)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="Stream SPC") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


ER_plt_Q <- ggplot(StreamP_RF_ER, aes(y = ER_meanPos, x=log(dischargeCFS+1), color=shore)) +
  geom_point(alpha = 0.8) + ylim(0,25)+
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  labs(y=expression("|E|R"~mmol~O[2]~m^-3~d^-1), x="log(Q+1)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


SER_grid <- ggarrange(ER_plt_STemp,
                      ER_plt_ST,
                      ER_plt_Q,
                       ncol = 3, nrow = 1,
                       #widths = c(1, 0.7),
                       common.legend = TRUE,
                       legend = "bottom")

# ggsave(plot = SER_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_ER_stream.png",sep=""),width=12,height=4,dpi=300)


###
###
##
# Local models 
lake_RF_GPP <- NS_GPP_df1 %>% 
  subset(GPP_mean>0) %>%
  dplyr::select(1:37,53, 54) 
summary(lake_RF_GPP)

lake_RF_ER <- NS_ER_df1 %>% 
  subset(ER_mean<0) %>%
  dplyr::select(1:37,53, 54) 
summary(lake_RF_ER)

LG_mod_null <- lmer(log(GPP_mean+1) ~ 
                   (1|shore), data= lake_RF_GPP)
summary(LG_mod_null)

LG_mod_1 <- lmer(log(GPP_mean+1) ~ 
                   scale(airT_mean) + 
                   scale(SolarR_mean) +
                   scale(Windsp_sd) +
                   scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_GPP)
summary(LG_mod_1)
vif(LG_mod_1)
hist(residuals(LG_mod_1))
r.squaredGLMM(LG_mod_1)

##

coef_table <- summary(LG_mod_1)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(LG_mod_1),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(airT_mean)"] <- "Air temp."
  RowNames[RowNames == "scale(SolarR_mean)"] <- "Solar rad."
  RowNames[RowNames == "scale(Windsp_sd)"] <- "Wind var."
  RowNames[RowNames == "scale(log(precipitation + 1))"] <- "Precip."
})


coeffplotGP <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter", title=expression("GLMM: GPP"~(mmol~O[2]~m^-3~d^-1))) +
  theme_bw() +
  annotate(
    "text",
    x = min(na.omit(plot_data2$Estimate)+0.75),  # Adjust the x and y coordinates as needed
    y = c(4.5),
    label = expression(R^2~"=0.57"), 
    parse = TRUE,
    hjust = 1,  # Adjust horizontal justification
    vjust = 1   # Adjust vertical justification
  )

# ggsave(plot = coeffplotGP, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_GPPclim.png",sep=""),width=3.5,height=2.5,dpi=300)


##

LG_mod_2 <- lmer(log(GPP_mean+1) ~ 
                   scale(airT_mean) + 
                   #scale(SolarR_mean) +
                   #scale(Windsp_sd) +
                   #scale(precipitation)+
                   (1|shore), data= lake_RF_GPP)
summary(LG_mod_2)
hist(residuals(LG_mod_2))

LG_mod_3 <- lmer(log(GPP_mean+1) ~ 
                   #scale(wtemp_mean) + 
                   scale(SolarR_mean) +
                   #scale(Windsp_sd) +
                   #scale(precipitation)+
                   (1|shore), data= lake_RF_GPP)
summary(LG_mod_3)
hist(residuals(LG_mod_3))

LG_mod_4 <- lmer(log(GPP_mean+1) ~ 
                   #scale(wtemp_mean) + 
                   #scale(SolarR_mean) +
                   scale(Windsp_sd) +
                   #scale(precipitation)+
                   (1|shore), data= lake_RF_GPP)
summary(LG_mod_4)
hist(residuals(LG_mod_4))


LG_mod_5 <- lmer(log(GPP_mean+1) ~ 
                   #scale(wtemp_mean) + 
                   #scale(SolarR_mean) +
                   #scale(Windsp_sd) +
                   scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_GPP)
summary(LG_mod_5)
hist(residuals(LG_mod_5))

## SHOULD make a figure for all of it

AIC(LG_mod_5, LG_mod_4, LG_mod_3, LG_mod_2, LG_mod_1, LG_mod_null)


gpp_plt_WS <- ggplot(lake_RF_GPP, aes(y = GPP_mean, x=Windsp_sd, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="Wind speed variance") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 

gpp_plt_SR <- ggplot(lake_RF_GPP, aes(y = GPP_mean, x=SolarR_mean, color=shore)) +
  geom_point(alpha = 0.8) +
  xlim(0,400)+
  geom_smooth(method="lm", se=F) +
  # geom_errorbarh(aes(xmin = SolarR_mean - SolarR_sd, 
  #                    xmax = SolarR_mean + SolarR_sd), alpha=c(0.2)) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="Solar radiation (Wms)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


gpp_plt_temp <- ggplot(lake_RF_GPP, aes(y = GPP_mean, x=airT_mean, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbarh(aes(xmin = airT_mean - airT_sd, 
                     xmax = airT_mean + airT_sd), alpha=c(0.2)) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="Air temperature (C)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


gpp_plt_precp <- ggplot(lake_RF_GPP, aes(y = GPP_mean, x=log(precipitation+1), color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="log(precipitation + 1)(mm)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


##
library(ggpubr)
LGPP_grid <- ggarrange(gpp_plt_WS,
                       gpp_plt_SR,
                       gpp_plt_temp,
                       gpp_plt_precp,
                       ncol = 2, nrow = 2,
                       #widths = c(1, 0.7),
                       common.legend = TRUE,
                       legend = "bottom")

# ggsave(plot = LGPP_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_GPP_climP.png",sep=""),width=6.5,height=6.5,dpi=300)

##
lake_RF_ER$ER_meanPos <- lake_RF_ER$ER_mean * c(-1)

LE_mod_null <- lmer((ER_mean) ~ 
                      (1|shore), data= lake_RF_ER)
summary(LE_mod_null)

LE_mod_1 <- lmer(ER_meanPos ~ 
                   scale(airT_mean) + 
                   scale(SolarR_mean) +
                   scale(Windsp_sd) +
                   scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_ER)
summary(LE_mod_1)
vif(LE_mod_1)
hist(residuals(LE_mod_1))
r.squaredGLMM(LE_mod_1)
###

coef_table <- summary(LE_mod_1)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(LE_mod_1),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(airT_mean)"] <- "Air temp."
  RowNames[RowNames == "scale(SolarR_mean)"] <- "Solar rad."
  RowNames[RowNames == "scale(Windsp_sd)"] <- "Wind var."
  RowNames[RowNames == "scale(log(precipitation + 1))"] <- "Precip."
})


coeffplotER <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter",  title=expression("GLMM: |ER|"~(mmol~O[2]~m^-3~d^-1))) +
  theme_bw() +
  annotate(
    "text",
    x = min(na.omit(plot_data2$Estimate)+0.75),  # Adjust the x and y coordinates as needed
    y = c(4.5),
    label = expression(R^2~"=0.44"), 
    parse = TRUE,
    hjust = 1,  # Adjust horizontal justification
    vjust = 1   # Adjust vertical justification
  )

# ggsave(plot = coeffplotER, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_ERclim.png",sep=""),width=3.5,height=2.5,dpi=300)


###



LE_mod_2 <- lmer(ER_mean ~ 
                   scale(airT_mean) + 
                   #scale(SolarR_mean) +
                   #scale(Windsp_sd) +
                   #scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_ER)
summary(LE_mod_2)
hist(residuals(LE_mod_2))

LE_mod_3 <- lmer(ER_mean ~ 
                   #scale(airT_mean) + 
                   scale(SolarR_mean) +
                   #scale(Windsp_sd) +
                   #scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_ER)
summary(LE_mod_3)
hist(residuals(LE_mod_3))

LE_mod_4 <- lmer(ER_mean ~ 
                   #scale(airT_mean) + 
                   #scale(SolarR_mean) +
                   scale(Windsp_sd) +
                   #scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_ER)
summary(LE_mod_4)
hist(residuals(LE_mod_4))


LE_mod_5 <- lmer(ER_mean ~ 
                   #scale(airT_mean) + 
                   #scale(SolarR_mean) +
                   #scale(Windsp_sd) +
                   scale(log(precipitation+1))+
                   (1|shore), data= lake_RF_ER)
summary(LE_mod_5)
hist(residuals(LE_mod_5))

AIC(LE_mod_5, LE_mod_4, LE_mod_3, LE_mod_2, LE_mod_1, LE_mod_null)

##
# weather models 

ER_plt_ST <- ggplot(lake_RF_ER, aes(y = ER_meanPos, x=airT_mean, color=shore)) +
  geom_point(alpha = 0.8) + ylim(0,25)+
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  geom_errorbarh(aes(xmin = airT_mean - airT_sd,
                     xmax = airT_mean + airT_sd), alpha=c(0.2)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="Air temp. (C)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


ER_plt_SR <- ggplot(lake_RF_ER, aes(y = ER_meanPos, x=SolarR_mean, color=shore)) +
  geom_point(alpha = 0.8) +
  xlim(0,400)+
  #geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="Solar radiation (Wms)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


ER_plt_WS <- ggplot(lake_RF_ER, aes(y = ER_meanPos, x=Windsp_sd, color=shore)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="Wind speed variance") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


ER_plt_prec <- ggplot(lake_RF_ER, aes(y = ER_meanPos, x=log(precipitation+1), color=shore)) +
  geom_point(alpha = 0.8) +
  #geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanPos - ER_sd, 
                    ymax = ER_meanPos + ER_sd), alpha=c(0.5)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="log(precipitation+1) (mm)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 

LGPP_grid <- ggarrange(gpp_plt_WS,
                       gpp_plt_SR,
                       gpp_plt_temp,
                       gpp_plt_precp,
                       ncol = 2, nrow = 2,
                       #widths = c(1, 0.7),
                       common.legend = TRUE,
                       legend = "bottom")


SER_grid <- ggarrange(ER_plt_WS,
                      ER_plt_SR,
                      ER_plt_ST,
                      ER_plt_prec,
                      ncol = 2, nrow = 2,
                      #widths = c(1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")

# ggsave(plot = SER_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_ER_climP.png",sep=""),width=7,height=7,dpi=300)



NS_GPP_df1w <- NS_GPP_df1%>%
  filter(shore=="west")
unique(NS_GPP_df1w$shore)

NS_ER_df1 <- NS_ER_df1%>%
  filter(shore=="west")
unique(NS_GPP_df1w$shore)

summary(NS_ER_df1) # 0 to 28.208

NS_GPP_df1e <- NS_GPP_df1%>%
  filter(shore=="east")
unique(NS_GPP_df1e$shore)


NS_GPP_df1e <- NS_ER_df%>%
  filter(shore=="east")
unique(NS_GPP_df1e$shore)

summary(NS_GPP_df1e) 










RF_temp4<- na.omit(NS_ER_df1[,c(-1,-5,-26,-27, -30,-31,-32, -33, -39)])
str(RF_temp4)


# time to wrangle in chemistry. 
## PW chem 
PWchem <- read.csv("/Users/kellyloria/Downloads/ProcessedPoreWater.csv", header=T)
str(PWchem)

PWCHEM <- PWchem %>%
  dplyr::rename(pw_NO3="Accepted_NO3", pw_PO4= "Accepted_PO4", pw_NH3="Accepted_NH3")%>%
  dplyr::select(Site, date, pw_NO3, pw_PO4, pw_NH3)%>%
  mutate(date = as.Date((date), format ="%m/%d/%y"))

str(PWCHEM)
PWCHEM$pw_PO4 <- as.numeric(PWCHEM$pw_PO4)
PWCHEM$pw_NO3 <- as.numeric(PWCHEM$pw_NO3)

unique(PWCHEM$Site)

PWCHEM_L <- PWCHEM %>%
  mutate(shore = case_when(
    Site %in% c("BWNS1", 
                "BWNS2",
                "BWNS1",
                "BW10m",
                "BW20m",
                "BW0.5m"
    ) ~ "west",
    Site %in% c("GBNS1", 
                "GBNS2",
                "GBNS3",
                "GB20m",
                "GB10m",
                "GB0.5m"
    ) ~ "east",
    TRUE ~ "unknown"  # Assign "unknown" for unmatched site IDs (optional)
  ))

PWCHEM_L2 <- PWCHEM_L %>%
  mutate(location = case_when(
    Site %in% c( "BW0.5m", 
                 "GB0.5m"
    ) ~ "0.5m",
    Site %in% c("BWNS1", 
                "BWNS2",
                "BWNS3",
                "GBNS1",
                "GBNS2",
                "GBNS3",
                "GB20m",
                "GB10m",
                "BW20m",
                "BW10m"
    ) ~ "3m",
    TRUE ~ "unknown"  # Assign "unknown" for unmatched site IDs (optional)
  ))

PWCHEM_L2 <- PWCHEM_L2 %>%
  filter(shore == "east"|shore == "west") 

PWCHEM_L3 <- PWCHEM_L2 %>%
  filter(location == "0.5m"|location == "3m") 

NO3_plot <- ggplot(PWCHEM_L2, aes(x=date, y=(pw_NO3), color =(shore), shape=location)) + 
  geom_point(size=2) + labs(x ="Date (%m-%y)", y = "Pore water N03 (mgL)") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "3 month") +
  scale_color_manual(values=alpha(c("#3283a8",   # "#F9C74F"
                                    "#a67d17"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  theme(axis.title.x=element_blank())

Nh3_plot <- ggplot(PWCHEM_L2, aes(x=date, y=(pw_NH3), color =(shore), shape=location)) + 
  geom_point(size=2) + labs(x ="Date (%m-%y)", y = "Pore water NH3 (mgL)") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "3 month") +
  scale_color_manual(values=alpha(c("#3283a8",   # "#F9C74F"
                                    "#a67d17"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  theme(axis.title.x=element_blank())

PO4_plot <- ggplot(PWCHEM_L2, aes(x=date, y=(pw_PO4), color =(shore), shape=location)) + 
  geom_point(size=2) + labs(x ="Date (%m-%y)", y = "Pore water PO4 (ugL)") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "3 month") +
  scale_color_manual(values=alpha(c("#3283a8",   # "#F9C74F"
                                    "#a67d17"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) +
  theme(axis.title.x=element_blank())


Lakechem_grid <- ggarrange(NO3_plot,
                      Nh3_plot,
                      PO4_plot,
                      ncol = 1, nrow = 3,
                      #widths = c(1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")

# ggsave(plot = Lakechem_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_GPP_lakechem.png",sep=""),width=6.5,height=7.5,dpi=300)


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



p6 <- ggplot(data = weatherdf3, aes(date, precipitation_cumulative, color = shore))+
  # geom_ribbon(aes(ymin = Precip24_mean-Precip24_sd, ymax = Precip24_mean+Precip24_sd, fill = shore),
  #             linetype = 0, alpha = 0.2) +
  geom_line(aes(color = shore),alpha = 0.8, size=2) +
  #geom_line()+
  #ylim(4.5, 23) +
  labs(y=expression("Total precipitation (mm)"), x="") + 
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
  geom_line(aes(color = shore),alpha = 0.8, size=2) +
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
  geom_line(aes(color = shore),alpha = 0.7, size=1.25) +
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
                      #p5,
                      ncol = 1, nrow = 2,
                      widths = c(1,1, 0.7),
                      common.legend = TRUE,
                      legend = "bottom")


# ggsave(plot = all_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_HydroClim.png",sep=""),width=9.5,height=8,dpi=300)

# ggsave(plot = all_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_HydroClimSmall.png",sep=""),width=9,height= 5 ,dpi=300)



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







NS_metab_df1$NEP <- NS_metab_df1$GPP_mean +NS_metab_df1$ER_mean
summary(NS_metab_df1)



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




### LAKE STAGE ###

## FLOW
# library(dataRetrieval)
## Add in flow data
siteNo <- "10337000"
pCode <- c("00065")
start.date <- "2021-01-01"
end.date <- "2023-10-01"

GBflow <- readNWISdata(siteNumbers = siteNo,
                       parameterCd = pCode,
                       startDate = start.date,
                       endDate = end.date)

lakelevel.ts <- GBflow %>% select("dateTime", "X_00065_32400") %>% 
  dplyr::rename(date = "dateTime", lake_level = "X_00065_32400") %>%
  select("date", "lake_level")


lake_RF_GPP_1<- lake_RF_GPP%>%
  full_join(lakelevel.ts)

lake_RF_ER_1<- lake_RF_ER%>%
  full_join(lakelevel.ts)



LG_mod_5 <- lmer(log(GPP_mean+1) ~ 
                   scale(lake_level)+
                   (1|shore), data= lake_RF_GPP_1)
summary(LG_mod_5)
hist(residuals(LG_mod_5))

lake_RF_GPP_1e<- lake_RF_GPP_1%>%
  filter(shore=="east")
LG_mod_5 <- lm(log(GPP_mean+1) ~ 
                   scale(lake_level), data= lake_RF_GPP_1e)
summary(LG_mod_5)
hist(residuals(LG_mod_5))
r.squaredGLMM(LG_mod_5)


lake_RF_GPP_1w<- lake_RF_GPP_1%>%
  filter(shore=="")
LG_mod_5 <- lm(log(GPP_mean+1) ~ 
                 scale(lake_level), data= lake_RF_GPP_1e)
summary(LG_mod_5)
hist(residuals(LG_mod_5))
r.squaredGLMM(LG_mod_5)


gpp_plt_WS <- ggplot(lake_RF_GPP_1, aes(y = GPP_mean, x=lake_level, color=shore)) +
  geom_point(alpha = 0.8) + xlim(2.5,5)+ ylim(0,30)+
  #geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = GPP_mean - GPP_sd, 
                    ymax = GPP_mean + GPP_sd), alpha=c(0.5)) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1), x="Lake level (m)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 


lake_RF_ER_1$ER_meanP <- lake_RF_ER_1$ER_mean * c(-1)

LG_mod_5 <- lmer(ER_meanP ~ 
                   scale(lake_level)+
                   (1|shore), data= lake_RF_ER_1)
summary(LG_mod_5)
hist(residuals(LG_mod_5))



ER_plt_WS <- ggplot(lake_RF_ER_1, aes(y = ER_meanP, x=lake_level, color=shore)) +
  geom_point(alpha = 0.8) + xlim(2.5,5)+ ylim(0,30)+
  #geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin = ER_meanP - ER_sd, 
                    ymax = ER_meanP + ER_sd), alpha=c(0.5)) +
  labs(y=expression("|ER|"~mmol~O[2]~m^-3~d^-1), x="Lake level (m)") + 
  scale_color_manual(values=alpha(c("#a67d17", "#3283a8"),0.75)) + theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.subtitle = element_text(size = 18)) 



Lakechem_grid <- ggarrange(gpp_plt_WS,
                           ER_plt_WS,
                           ncol = 2, nrow = 1,
                           #widths = c(1, 0.7),
                           common.legend = TRUE,
                           legend = "bottom")

# ggsave(plot = Lakechem_grid, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/23_metab_lakelevel.png",sep=""),width=8.5,height=5,dpi=300)



#
PRISM22 <- read.csv("/Users/kellyloria/Downloads/Temp_NicuFigures/PRISM_ppt_tmin_tmean_tmax_vpdmin_vpdmax_provisional_4km_20221001_20230930.csv", skip=10)

str(PRISMdata)
PRISMdata <- PRISM22 %>%
  mutate(date=as.Date(Date, format="%Y-%m-%d"))%>%
           filter(Name=="BWNS1"|Name=="BWNS2" |Name=="BWNS3"|
                    Name=="GBNS1"|Name=="GBNS2"| Name=="GBNS3") %>%
  mutate(shore = case_when(
    Name %in% c("GBNS1", "GBNS2", "GBNS3") ~ "GB",
    Name %in% c("BWNS1", "BWNS2", "BWNS3") ~ "BW",
    TRUE ~ NA_character_  # Handle other cases if any
  ))

PRISMdata1 <- PRISMdata %>%
  filter(Name=="BWNS2" | Name=="GBNS2") 


precip_plot <- 
  ggplot(PRISMdata1, aes(x=date, y=ppt..mm., colour = as.factor(shore))) + 
  geom_line() +
  labs(y = 'PRISM precip (mm)', x=NULL) +
  scale_color_manual(values=alpha(c("#3283a8", "#a67d17"),0.9)) +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "4 month") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))
precip_plot




