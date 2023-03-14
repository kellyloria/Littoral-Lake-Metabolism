library(tidyverse)
library(lubridate)
#plotting packages:
library(ggplot2)
library(cowplot)
library(reshape2)
library(scales)
library(gridExtra)


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

# ggsave(plot = p2, filename = paste("./figures/BWNS_all.png",sep=""),width=12,height=11,dpi=300)

# ggsave(plot = p2, filename = paste("./figures/LA_all.png",sep=""),width=12,height=18,dpi=300)


## total grid:
# all_grid <- ggarrange(tempgrid,
#                       Vel_grid,
#                       PS_grid,
#                       NDVIgrid,
#                       ncol = 1, nrow = 4,
#                       widths = c(1,1, 0.7),
#                       common.legend = TRUE, 
#                       legend = "bottom")
# 

#### Safe summary estimates

# BW 2021: all, 2022: NS1 and 2
BW1sum <- rbind(BWNS1, BWNS1_22)
BWs1NEP<- BW1sum%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
# long to wide

#library(reshape2)
BW1NEP <- reshape(data=BWs1NEP, idvar="date",
                          v.names = "middle",
                          timevar = "name",
                          direction="wide")


BW2sumNEP<- BWNS2%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
BW2NEP <- reshape(data=BW2sumNEP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

BW3sumNEP <- rbind(BWNS3, BWNS3_22)
BW3sumNEP<- BW3sumNEP%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
BW3NEP <- reshape(data=BW3sumNEP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


# BW 2021: all, 2022: NS1 and 2
BW1sum <- rbind(BWNS1, BWNS1_22)
BWs1NEP<- BW1sum%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
# long to wide

#library(reshape2)
BW1NEP <- reshape(data=BWs1NEP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


BW2sumNEP<- BWNS2%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
BW2NEP <- reshape(data=BW2sumNEP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")


GB1sumNEP<- GBNS1%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
GB1NEP <- reshape(data=GB1sumNEP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

GBNS3_nep <- rbind(GBNS3, GBNS3_22)

GB3sumNEP<- GBNS3_nep%>%
  subset(name=="NEP") %>%
  subset(middle <= 40 & middle >= -40)
GB3NEP <- reshape(data=GB3sumNEP, idvar="date",
                  v.names = "middle",
                  timevar = "name",
                  direction="wide")

# long to wide



se <- function(dat){
  se <- sd(dat)/sqrt(length(dat))
  return(se)}

BW_NEP <- rbind(BW1NEP, BW2NEP,BW3NEP)
BW_NEP$shore <-"west"

# subset for oct 01
BW_NEP21<- subset(BW_NEP, date>as.Date("2022-03-01"))

mean(BW_NEP21$middle.NEP)
se(BW_NEP21$middle.NEP)


GB_NEP <- rbind(GB1NEP, GB3NEP)
GB_NEP$shore <- "east"
# subset for oct 01
GB_NEP21<- subset(GB_NEP, date>as.Date("2021-03-01"))
mean(GB_NEP21$middle.NEP)
se(GB_NEP21$middle.NEP)





sumNEP <-rbind(BW_NEP,GB_NEP)


sumNEPplot <- ggplot(data = sumNEP %>% drop_na(year),aes(date, middle.NEP, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = shore),
              linetype = 0, alpha = 0.2)+
  geom_line()+ geom_point(size= 3, alpha = 0.6)+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
             color = "#4c4d4c") +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+ #new
  #ylim(-15, 15) +
  scale_y_continuous(breaks = seq(-15, 24, by = 3))+
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) + 
  theme_classic() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'right', 
        legend.direction = "vertical")

# ggsave(plot = sumNEPplot, filename = paste("./figures/LA_NEP_all.png",sep=""),width=8.5,height=6.5,dpi=300)



p2 <- ggplot(data = GB_NEP %>% drop_na(year),aes(date, middle.NEP, color = site))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site),
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


max(BW_NEP$middle.NEP)





p2 <- ggplot(data = BW_NEP %>% drop_na(year),aes(date, middle.NEP, color = site))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site),
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

# LAKE NEP drivers

# streamflow?
# DOdat from 2022Figures 

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
str(flow)
sumNEPF <- left_join(sumNEP, flow,  by=c('date'='datetime', 'shore'='shore'))

library(lmerTest)
library(lme4)
library(MuMIn)
hist(sumNEPF$dischargeCFS)
hist(log(sumNEPF$dischargeCFS +1))
hist((sumNEPF$middle.NEP))


mod1<- glm(middle.NEP~ scale(dischargeCFS)+ (shore), data=sumNEPF)
summary(mod1)

mod2<- lmer(middle.NEP~ scale(dischargeCFS)+ (shore) + (1|site), data=sumNEPF)
summary(mod2)


mod3<- lmer(middle.NEP~ scale(dischargeCFS)+ (1|site), data=sumNEPF)
summary(mod3)

hist(residuals(mod2))
r.squaredGLMM(mod2)





sumNEPplot <- ggplot(data = sumNEPF %>% drop_na(year),aes(log(dischargeCFS), middle.NEP, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_pointrange(aes(ymin = lower, ymax = upper, fill = shore), alpha = 0.2)+
  #geom_line()+ 
  geom_point(size= 1, alpha = 0.4)+
  #geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
            # color = "#4c4d4c") +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  geom_smooth(method="lm", se=F) +
 # scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+ #new
  #ylim(-15, 15) +
  scale_y_continuous(breaks = seq(-15, 24, by = 3))+
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) + 
  theme_classic() + 
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'right', 
        legend.direction = "vertical") 
#+ facet_grid((shore~.))

# ggsave(plot = sumNEPplot, filename = paste("./figures/NEP_flow_all.png",sep=""),width=4.25,height=4,dpi=300)




# stream SPC
sumNEPF <- left_join(sumNEP, flow,  by=c('date'='datetime', 'shore'='shore'))



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
  summarise(SPCmean = mean(SPC,na.rm = T),
            SPCstd = sd(SPC, na.rm = T))

sumNEPFSPC <- left_join(sumNEPF, SPC_datD[,c("date", "shore", "SPCmean", "SPCstd")],  by=c('date'='date', 'shore'='shore'))



sumNEPplot <- ggplot(data = sumNEPFSPC %>% drop_na(year),aes(log(SPCmean), middle.NEP, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_pointrange(aes(ymin = lower, ymax = upper, 
                      fill = shore), alpha = 0.2)+
  #geom_line()+ 
  geom_point(size= 1, alpha = 0.4)+
  #geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
  # color = "#4c4d4c") +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  geom_smooth(method="lm", se=F) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+ #new
  #ylim(-15, 15) +
  scale_y_continuous(breaks = seq(-15, 24, by = 3))+
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) + 
  theme_classic() + 
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'right', 
        legend.direction = "vertical") 
#+ facet_grid((shore~.))

# ggsave(plot = sumNEPplot, filename = paste("./figures/NEP_SPC_all.png",sep=""),width=4.25,height=4,dpi=300)


mod1<- lmer(middle.NEP~ scale(SPCmean)+ shore+(1|site), data=sumNEPFSPC)
summary(mod1)


mod2<- lmer(middle.NEP~ scale(SPCmean) + scale(dischargeCFS)+ shore + (1|site), data=sumNEPFSPC)
summary(mod2)

hist(residuals(mod1))
r.squaredGLMM(mod1)
# 
# Soil_dat_l$date2<- Soil_dat_l$date - 20
# 
# sumNEPFSPC_A <- left_join(sumNEPFSPC, Soil_dat_l[,c("date2", "AFDMmgmL", "shore")],  by=c('date'='date2', 'shore'='shore'))
# summary(sumNEPFSPC_A)
# 
# sumNEPFSPC_A <- left_join(Soil_dat_l, sumNEPFSPC[,c("date", "middle.NEP", "shore")],  by=c('date2'='date', 'shore'='shore'))
# summary(sumNEPFSPC_A)
# 
# 
# sumNEPAplot <- ggplot(data = sumNEPFSPC %>% drop_na(year),aes((AFDMmgmL), middle.NEP, color = shore))+
#   geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
#   geom_pointrange(aes(ymin = lower, ymax = upper, 
#                       fill = shore), alpha = 0.2)+
#   #geom_line()+ 
#   geom_point(size= 1, alpha = 0.4)+
#   #geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
#   # color = "#4c4d4c") +
#   scale_color_manual(values=c("#a67d17", "#3283a8")) +
#   scale_fill_manual(values = c("#a67d17", "#3283a8")) +
#   geom_smooth(method="lm", se=F) +
#   # scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+ #new
#   #ylim(-15, 15) +
#   scale_y_continuous(breaks = seq(-15, 24, by = 3))+
#   labs(y=expression(mmol~O[2]~m^-3~d^-1)) + 
#   theme_classic() + 
#   theme(axis.text=element_text(size=11),
#         axis.title=element_text(size=11),
#         #axis.text.x=element_text(angle=60, hjust=1), # new
#         legend.position = 'right', 
#         legend.direction = "vertical") 
# 
# 




## STREAM PLOT

BWL <- read.csv("./BWL_daily.csv")
BWL$date <- as.Date(BWL$date, origin="2021-01-01")
BWL$site <- "BWL"
BWL$shore <- "west"


GBL <- read.csv("./GBL_daily.csv")
GBL$date <- as.Date(GBL$date, origin="2021-01-01")
GBL$site <- "GBL"
GBL$shore <- "east"


Eplot_sp <- ggplot(GBL, aes(x = GPP_mean, y = ER_mean)) + ylab("Ecosystem respiration") + 
  xlab("Gross primary productivity") + #ylim(-25, 0) + xlim(0,5) + 
  geom_point(aes(x = GPP_mean, y = ER_mean), shape= 17, col = alpha(c("#a67d17"),0.5)) +
  geom_abline(intercept = 0, slope = -1, col = "grey50")+
  stat_density2d(aes(colour = ..level..)) +
  scale_colour_gradient(
    low = "#faefd4",
    high = "#a67d17",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )  +
  theme_classic()


Wplot_sp <- ggplot(BWL, aes(x = GPP_mean, y = ER_mean)) + ylab("Ecosystem respiration") + 
  xlab("Gross primary productivity") + #ylim(-25, 0) + xlim(0,5) + 
  geom_point(aes(x = GPP_mean, y = ER_mean), shape= 17, col = alpha(c("#a67d17"),0.5)) +
  geom_abline(intercept = 0, slope = -1, col = "grey50")+
  stat_density2d(aes(colour = ..level..)) +
  scale_colour_gradient(
    low = "#faefd4",
    high = "#a67d17",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )  +
  theme_classic()



BWL$NEP<- BWL$GPP_daily_mean + BWL$ER_daily_mean
BWL$NEP_sd<- sd(BWL$GPP_daily_mean + BWL$ER_daily_mean)

GBL$NEP<- GBL$GPP_daily_mean + GBL$ER_daily_mean

names(GBL)

streamNEP <- rbind(GBL, BWL)

StreamGPPplot <- ggplot(data = streamNEP, aes(date, GPP_daily_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50") +
  geom_point(size= 1, alpha = 0.6) +
  geom_line() +
  geom_pointrange(aes(ymin =(GPP_2.5pct), 
                      ymax = (GPP_97.5pct)), alpha = 0.75) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
             color = "#4c4d4c") +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y")+ #new
  scale_y_continuous(breaks = seq(-30, 25, by = 5))+
  labs(y=expression(GPPmmol~O[2]~m^-3~d^-1)) + 
  theme_bw() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),# new
        legend.position = 'right')




StreamERplot <- ggplot(data = streamNEP, aes(date, ER_daily_mean, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50") +
  geom_point(size= 1, alpha = 0.6) +
  geom_line() +
  geom_pointrange(aes(ymin =(ER_2.5pct), 
                      ymax = (ER_97.5pct)), alpha = 0.75) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
             color = "#4c4d4c") +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y")+ #new
  scale_y_continuous(breaks = seq(-30, 25, by = 5))+
  labs(y=expression(ERmmol~O[2]~m^-3~d^-1)) + 
  theme_bw() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = 'right')




## total grid:
NEP_grid <- ggarrange(StreamGPPplot,
                       StreamERplot,
                       ncol = 1, nrow = 2,
                       common.legend = TRUE, 
                       legend = "bottom")

# save plot to figures folder
#  ggsave(plot = NEP_grid, filename = paste("./NEP_grid.png",sep=""),width=8,height=6.5,dpi=300)



# ggsave(plot = StreamNEPplot, filename = paste("./figures/SM_NEP_all.png",sep=""),width=9.5,height=6.5,dpi=300)

sumNEP_SM <- left_join(sumNEPF, streamNEP[,c("date", "shore", "NEP", "GPP_daily_mean", "ER_daily_mean")],  by=c('date'='date', 'shore'='shore'))



sumNEPplot <- ggplot(data = sumNEP_SM %>% drop_na(year),aes(GPP_daily_mean, middle.NEP, color = shore))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_pointrange(aes(ymin = lower, ymax = upper, 
                      fill = shore), alpha = 0.2)+
  #geom_line()+ 
  geom_point(size= 1, alpha = 0.4)+
  #geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
  # color = "#4c4d4c") +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  geom_smooth(method="lm", se=F) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+ #new
  #ylim(-15, 15) +
  #scale_y_continuous(breaks = seq(-25, 25, by = 5))+
  #scale_x_continuous(breaks = seq(-30, 25, by = 5))+
  labs(y="Nearshore NEP", expression(mmol~O[2]~m^-3~d^-1),
       x="Stream NEP", expression(mmol~O[2]~m^-3~d^-1)) + 
  theme_classic() + 
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11), # new
        legend.position = 'right', 
        legend.direction = "vertical") + facet_grid((shore~.))

ggsave(plot = sumNEPplot, filename = paste("./figures/LA_SM_GNEP_all.png",sep=""),width=5,height=7,dpi=300)

