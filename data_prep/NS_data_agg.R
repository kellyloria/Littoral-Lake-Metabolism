## 
lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "tidyverse","data.table","xts","dygraphs",
         "nrlmetab","cowplot"), require, character.only=T)

source("./saved_fxns/LM.o2.at.sat.R")
source("./saved_fxns/LM.wind.scale.R")

# read in data:
# Flag 1 - "YES" indicates day of deployment or removal
# Flag 2 - "YES" indicates poor miniDOT instrument function
# Flag 3 - "YES" indicates poor miniwiper instrument function
# Flag 4 - "YES" indicates suspected biofouling due to any of the above, photos, or general DO trends

ns_DO <- readRDS("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/NS_miniDOT/flagged_all_100423.rds")

ns_DO <- ns_DO %>% mutate(datetime = as.POSIXct((Pacific_Standard_Time), format ="%Y-%m-%d %H:%M:%S"))
summary(ns_DO)

# create matching column to line up data by site:
str(ns_DO)
unique(ns_DO$site)
unique(ns_DO$location)
unique(ns_DO$replicate)

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



plot <-
  ggplot(ns_DOQ, aes(x=datetime, y=Dissolved_O_mg_L, colour = as.factor(Site))) +
  geom_point(alpha=0.1) + geom_line(alpha=0.25)+
  #labs(y = 'light dat', x=NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) #+ theme(legend.position = "none") + facet_grid(Name~.)
plot

