# NS ag
library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(reshape2)
library(scales)
library(gridExtra)
library(ggpubr)

#########
# BWNS1 #
rawdat = list.files(paste("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/DOdat/Blackwood/NS1/cat"), full.names = T) 
  
my.data <- lapply(rawdat,
                  read.delim,
                  header=TRUE, sep=",", skip = 8)
  
my.data <- do.call("rbind", my.data)
col_names<- c("Unix.Timestamp", "UTC_Date_._Time", "Pacific.Standard.Time", "Battery",
              "Temperature", "Dissolved.Oxygen", "Dissolved.Oxygen.Saturation", "Q")
colnames(my.data) <- col_names
str(my.data)

# correct time
my.data$c <- as.POSIXct(my.data$Pacific.Standard.Time, origin="1970-01-01")
head(my.data)

# round time to the nearest 15 minute. 
my.data$Pacific.Standard.Time <-format(round_date(as.POSIXct(paste(my.data$Pacific.Standard.Time)), unit="5 mins"))

## add in meta for sensor ID:
# Define timestamp ranges and corresponding labels
timestamp_range1 <- c(as.POSIXct("2021-06-11 12:00:00"), as.POSIXct("2021-10-15 10:00:00"))
label1 <- c("7450-195441")

timestamp_range2 <- c(as.POSIXct("2021-10-09 18:00:00"), as.POSIXct("2022-05-24 11:00:00"))
label2 <- c("7450-174159")

timestamp_range3 <- c(as.POSIXct("2022-05-24 14:00:00"), as.POSIXct("2022-10-17 12:00:00"))
label3 <- c("7450-276557")

# Add a new column with sieral labels based on timestamp ranges
my.data <- my.data %>%
  mutate(label = case_when(
    Pacific.Standard.Time >= timestamp_range1[1] & Pacific.Standard.Time <= timestamp_range1[2] ~ label1,
    Pacific.Standard.Time >= timestamp_range2[1] & Pacific.Standard.Time <= timestamp_range2[2] ~ label2,
    Pacific.Standard.Time >= timestamp_range3[1] & Pacific.Standard.Time <= timestamp_range3[2] ~ label3,
  ))

my.data <- na.omit(my.data)

my.data <- subset(my.data, 
                Battery > 2.0 & 
                Q>0.7) 

# BWNS1 
my.data <- my.data%>%
  select("Pacific.Standard.Time", 
         "Temperature", 
         "Dissolved.Oxygen", 
         "Dissolved.Oxygen.Saturation", 
         "Q", 
         "Battery", 
         "label")

BWNS1<- my.data
BWNS1$site <- "BWNS1"
# ggplot(BWNS1, aes(Pacific.Standard.Time, Dissolved.Oxygen)) + geom_point()

# plot_grid(
#   ggplot(BWNS1, aes(Pacific.Standard.Time, Dissolved.Oxygen)) + geom_point(),
#   ggplot(BWNS1, aes(Pacific.Standard.Time, Temperature)) + geom_point(),
#   ggplot(BWNS1, aes(Pacific.Standard.Time, Q)) + geom_point(),
#   ncol=1, align="hv")

# write.csv(x = BWNS1, file = "/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/AggRawData/AggregatedDO/BWNS1_v2.csv", row.names = TRUE)

#########
# BWNS2 #
rawdat = list.files(paste("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/DOdat/Blackwood/NS2/cat"), full.names = T) 

my.data <- lapply(rawdat,
                  read.delim,
                  header=TRUE, sep=",", skip = 8)

my.data <- do.call("rbind", my.data)
col_names<- c("Unix.Timestamp", "UTC_Date_._Time", "Pacific.Standard.Time", "Battery",
              "Temperature", "Dissolved.Oxygen", "Dissolved.Oxygen.Saturation", "Q")
colnames(my.data) <- col_names
str(my.data)

# correct time
my.data$c <- as.POSIXct(my.data$Pacific.Standard.Time, origin="1970-01-01")
head(my.data)

# round time to the nearest 15 minute. 
my.data$Pacific.Standard.Time <-format(round_date(as.POSIXct(paste(my.data$Pacific.Standard.Time)), unit="5 mins"))

## add in meta for sensor ID:
# Define timestamp ranges and corresponding labels
timestamp_range1 <- c(as.POSIXct("2021-06-11 12:00:00"), as.POSIXct("2021-10-15 10:00:00"))
label1 <- c("7450-336792")

timestamp_range2 <- c(as.POSIXct("2021-10-15 18:00:00"), as.POSIXct("2022-05-24 11:00:00"))
label2 <- c("7450-287080")


# Add a new column with sieral labels based on timestamp ranges
my.data <- my.data %>%
  mutate(label = case_when(
    Pacific.Standard.Time >= timestamp_range1[1] & Pacific.Standard.Time <= timestamp_range1[2] ~ label1,
    Pacific.Standard.Time >= timestamp_range2[1] & Pacific.Standard.Time <= timestamp_range2[2] ~ label2,
  ))

my.data <- na.omit(my.data)
my.data <- subset(my.data, 
                  Battery > 2.0 & 
                    Q>0.7) 
# BWNS2
my.data <- my.data%>%
  select("Pacific.Standard.Time", 
         "Temperature", 
         "Dissolved.Oxygen", 
         "Dissolved.Oxygen.Saturation", 
         "Q", 
         "Battery", 
         "label")

BWNS2<- my.data
BWNS2$site <- "BWNS2"

# write.csv(x = BWNS2, file = "/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/AggRawData/AggregatedDO/BWNS2_v2.csv", row.names = TRUE)

#########
# BWNS3 #
rawdat = list.files(paste("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/DOdat/Blackwood/NS3/cat"), full.names = T) 

my.data <- lapply(rawdat,
                  read.delim,
                  header=TRUE, sep=",", skip = 8)

my.data <- do.call("rbind", my.data)
col_names<- c("Unix.Timestamp", "UTC_Date_._Time", "Pacific.Standard.Time", "Battery",
              "Temperature", "Dissolved.Oxygen", "Dissolved.Oxygen.Saturation", "Q")
colnames(my.data) <- col_names
str(my.data)

# correct time
my.data$c <- as.POSIXct(my.data$Pacific.Standard.Time, origin="1970-01-01")
head(my.data)

# round time to the nearest 15 minute. 
my.data$Pacific.Standard.Time <-format(round_date(as.POSIXct(paste(my.data$Pacific.Standard.Time)), unit="5 mins"))

## add in meta for sensor ID:
# Define timestamp ranges and corresponding labels
timestamp_range1 <- c(as.POSIXct("2021-06-11 12:00:00"), as.POSIXct("2021-10-15 10:00:00"))
label1 <- c("7450-162475")

timestamp_range2 <- c(as.POSIXct("2021-10-17 18:00:00"), as.POSIXct("2022-05-24 11:00:00"))
label2 <- c("7450-276557")

timestamp_range3 <- c(as.POSIXct("2022-05-24 14:00:00"), as.POSIXct("2022-10-17 12:00:00"))
label3 <- c("7450-287080")


# Add a new column with sieral labels based on timestamp ranges
my.data <- my.data %>%
  mutate(label = case_when(
    Pacific.Standard.Time >= timestamp_range1[1] & Pacific.Standard.Time <= timestamp_range1[2] ~ label1,
    Pacific.Standard.Time >= timestamp_range2[1] & Pacific.Standard.Time <= timestamp_range2[2] ~ label2,
    Pacific.Standard.Time >= timestamp_range3[1] & Pacific.Standard.Time <= timestamp_range3[2] ~ label3,
  ))

my.data <- na.omit(my.data)
my.data <- subset(my.data, 
                  Battery > 2.0 & 
                    Q>0.7) 
# BWNS3 
my.data <- my.data%>%
  select("Pacific.Standard.Time", 
         "Temperature", 
         "Dissolved.Oxygen", 
         "Dissolved.Oxygen.Saturation", 
         "Q", 
         "Battery", 
         "label")

BWNS3<- my.data
BWNS3$site <- "BWNS3"

# write.csv(x = BWNS3, file = "/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/AggRawData/AggregatedDO/BWNS3_v2.csv", row.names = TRUE)

### GB ###


##########
#  GB 1  #
rawdat = list.files(paste("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/DOdat/Glenbrook/Nearshore1/cat"), full.names = T) 

my.data <- lapply(rawdat,
                  read.delim,
                  header=TRUE, sep=",", skip = 8)

my.data <- do.call("rbind", my.data)
col_names<- c("Unix.Timestamp", "UTC_Date_._Time", "Pacific.Standard.Time", "Battery",
              "Temperature", "Dissolved.Oxygen", "Dissolved.Oxygen.Saturation", "Q")
colnames(my.data) <- col_names
str(my.data)

# correct time
my.data$c <- as.POSIXct(my.data$Pacific.Standard.Time, origin="1970-01-01")
head(my.data)

# round time to the nearest 15 minute. 
my.data$Pacific.Standard.Time <-format(round_date(as.POSIXct(paste(my.data$Pacific.Standard.Time)), unit="5 mins"))

## add in meta for sensor ID:
# Define timestamp ranges and corresponding labels
timestamp_range1 <- c(as.POSIXct("2021-06-11 12:00:00"), as.POSIXct("2021-10-01 09:00:00"))
label1 <- c("7450-193411")

timestamp_range2 <- c(as.POSIXct("2021-10-02 10:00:00"), as.POSIXct("2022-05-23 11:00:00"))
label2 <- c("7450-278010")

timestamp_range3 <- c(as.POSIXct("2022-05-24 09:00:00"), as.POSIXct("2022-10-18 10:00:00"))
label3 <- c("7450-224208")

# Add a new column with sieral labels based on timestamp ranges
my.data <- my.data %>%
  mutate(label = case_when(
    Pacific.Standard.Time >= timestamp_range1[1] & Pacific.Standard.Time <= timestamp_range1[2] ~ label1,
    Pacific.Standard.Time >= timestamp_range2[1] & Pacific.Standard.Time <= timestamp_range2[2] ~ label2,
    Pacific.Standard.Time >= timestamp_range3[1] & Pacific.Standard.Time <= timestamp_range3[2] ~ label3,
  ))

my.data <- na.omit(my.data)
my.data <- subset(my.data, 
                  Battery > 2.0 & 
                    Q>0.7) 
# GBNS1 
my.data <- my.data%>%
  select("Pacific.Standard.Time", 
         "Temperature", 
         "Dissolved.Oxygen", 
         "Dissolved.Oxygen.Saturation", 
         "Q", 
         "Battery", 
         "label")

GBNS1<- my.data
GBNS1$site <- "GBNS1"
# write.csv(x = GBNS1, file = "/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/AggRawData/AggregatedDO/GBNS1_v2.csv", row.names = TRUE)

##########
#  GB 2  #
rawdat = list.files(paste("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/DOdat/Glenbrook/Nearshore2/cat"), full.names = T) 

my.data <- lapply(rawdat,
                  read.delim,
                  header=TRUE, sep=",", skip = 8)

my.data <- do.call("rbind", my.data)
col_names<- c("Unix.Timestamp", "UTC_Date_._Time", "Pacific.Standard.Time", "Battery",
              "Temperature", "Dissolved.Oxygen", "Dissolved.Oxygen.Saturation", "Q")
colnames(my.data) <- col_names
str(my.data)

# correct time
my.data$c <- as.POSIXct(my.data$Pacific.Standard.Time, origin="1970-01-01")
head(my.data)

# round time to the nearest 15 minute. 
my.data$Pacific.Standard.Time <-format(round_date(as.POSIXct(paste(my.data$Pacific.Standard.Time)), unit="5 mins"))

## add in meta for sensor ID:
# Define timestamp ranges and corresponding labels
timestamp_range1 <- c(as.POSIXct("2021-06-11 12:00:00"), as.POSIXct("2021-10-01 09:00:00"))
label1 <- c("7450-265933")

timestamp_range2 <- c(as.POSIXct("2021-10-02 10:00:00"), as.POSIXct("2022-05-23 11:00:00"))
label2 <- c("7450-224208")

timestamp_range3 <- c(as.POSIXct("2022-05-24 09:00:00"), as.POSIXct("2022-10-18 10:00:00"))
label3 <- c("7450-278010")

# Add a new column with sieral labels based on timestamp ranges
my.data <- my.data %>%
  mutate(label = case_when(
    Pacific.Standard.Time >= timestamp_range1[1] & Pacific.Standard.Time <= timestamp_range1[2] ~ label1,
    Pacific.Standard.Time >= timestamp_range2[1] & Pacific.Standard.Time <= timestamp_range2[2] ~ label2,
    Pacific.Standard.Time >= timestamp_range3[1] & Pacific.Standard.Time <= timestamp_range3[2] ~ label3,
  ))

my.data <- na.omit(my.data)
my.data <- subset(my.data, 
                  Battery > 2.0 & 
                    Q>0.7) 
# GBNS2 
my.data <- my.data%>%
  select("Pacific.Standard.Time", 
         "Temperature", 
         "Dissolved.Oxygen", 
         "Dissolved.Oxygen.Saturation", 
         "Q", 
         "Battery", 
         "label")

GBNS2<- my.data
GBNS2$site <- "GBNS2"
# write.csv(x = GBNS2, file = "/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/AggRawData/AggregatedDO/GBNS2_v2.csv", row.names = TRUE)


##########
#  GB 3  #
rawdat = list.files(paste("/Users/kellyloria/Documents/LittoralMetabModeling/RawData/DOdat/Glenbrook/Nearshore3/cat"), full.names = T) 

my.data <- lapply(rawdat,
                  read.delim,
                  header=TRUE, sep=",", skip = 8)

my.data <- do.call("rbind", my.data)
col_names<- c("Unix.Timestamp", "UTC_Date_._Time", "Pacific.Standard.Time", "Battery",
              "Temperature", "Dissolved.Oxygen", "Dissolved.Oxygen.Saturation", "Q")
colnames(my.data) <- col_names
str(my.data)

# correct time
my.data$c <- as.POSIXct(my.data$Pacific.Standard.Time, origin="1970-01-01")
head(my.data)

# round time to the nearest 15 minute. 
my.data$Pacific.Standard.Time <-format(round_date(as.POSIXct(paste(my.data$Pacific.Standard.Time)), unit="5 mins"))

## add in meta for sensor ID:
# Define timestamp ranges and corresponding labels
timestamp_range1 <- c(as.POSIXct("2021-06-11 12:00:00"), as.POSIXct("2021-10-01 09:00:00"))
label1 <- c("7450-224208")

timestamp_range2 <- c(as.POSIXct("2021-10-02 10:00:00"), as.POSIXct("2022-07-06 12:00:00"))
label2 <- c("7450-227604")

timestamp_range3 <- c(as.POSIXct("2022-07-06 18:00:00"), as.POSIXct("2022-10-18 10:00:00"))
label3 <- c("7450-193411")

# Add a new column with sieral labels based on timestamp ranges
my.data <- my.data %>%
  mutate(label = case_when(
    Pacific.Standard.Time >= timestamp_range1[1] & Pacific.Standard.Time <= timestamp_range1[2] ~ label1,
    Pacific.Standard.Time >= timestamp_range2[1] & Pacific.Standard.Time <= timestamp_range2[2] ~ label2,
    Pacific.Standard.Time >= timestamp_range3[1] & Pacific.Standard.Time <= timestamp_range3[2] ~ label3,
  ))

my.data <- na.omit(my.data)
my.data <- subset(my.data, 
                  Battery > 2.0 & 
                    Q>0.7) 
# GBNS3 
my.data <- my.data%>%
  select("Pacific.Standard.Time", 
         "Temperature", 
         "Dissolved.Oxygen", 
         "Dissolved.Oxygen.Saturation", 
         "Q", 
         "Battery", 
         "label")

GBNS3<- my.data
GBNS3$site <- "GBNS3"
# write.csv(x = GBNS3, file = "/Users/kellyloria/Documents/LittoralMetabModeling/Littoral-Lake-Metabolism/AggRawData/AggregatedDO/GBNS3_v2.csv", row.names = TRUE)

