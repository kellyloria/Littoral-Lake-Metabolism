# temporary timesieres plot
GS1
climate.temp3
GS1_all
GS1$datetime

temp_p <- full_join(GS1, climate.temp3[,c(10,18)])

temp_p <- na.omit(temp_p)
temp_p$datetime <-temp_p$datetime- 9


temp_p <- GS1_all %>%
  subset(datetime>as.POSIXct("2021-07-25 01:00:00 UTC")&
           datetime<as.POSIXct("2021-12-01 01:00:00 UTC"))


temp_p2 <- GS1_all %>%
  subset(datetime>as.POSIXct("2021-09-03 19:00:00 UTC")&
           datetime<as.POSIXct("2021-09-08 00:00:00 UTC"))

temp_p2$datetime <- temp_p2$datetime - hr
hr<-hours(6)
p3 <- ggplot(data = temp_p) +
  geom_point(aes(x=datetime, y=do.obs),alpha = 0.5) +
  geom_line(aes(x=datetime, y=do.obs), alpha = 0.8) +
  #ylim(-38, 1) +
  labs(y="DO (mgL)") + 
  labs(x="Date time") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y")+
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p3


ggsave(plot = p3, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_DOcalcexample.png",sep=""),width=9,height=3,dpi=300)

coeff<- c(10)


p4 <- ggplot(data = temp_p2) +
  geom_point(aes(x=datetime, y=do.obs),alpha = 0.5) +
  geom_line(aes(x=datetime, y=do.obs), alpha = 0.8) 


ggplot(temp_p2, aes(datetime, do.obs)) +
  geom_col() +
  geom_line(aes(y = a + Par*b), color = "red") +
  scale_y_continuous("Precipitation", sec.axis = sec_axis(~ (. - a)/b, name = "Temperature")) +
  scale_x_continuous("Month", breaks = 1:12) +
  ggtitle("Climatogram for Oslo (1961-1990)")  


p4 <- ggplot(data = temp_p2) +
  geom_line(aes(x=datetime, y=do.obs), alpha = 0.8) +
  geom_point(aes(x=datetime, y=do.obs))+
  #ylim(-38, 1) +
  labs(y="DO (mgL)") + 
  labs(x="Date time") +
  scale_x_datetime(date_breaks = "4 hour", date_labels = "%H:%M")+
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.position = 'bottom', 
        legend.direction = "horizontal") 
p4

# ggsave(plot = p4, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/ASLO_DOcalcexample_2.png",sep=""),width=5,height=3,dpi=300)

# Value used to transform the data
coeff <- 350

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(head(temp_p2, 32), aes(x=datetime)) +
  geom_line(aes(y=scale(do.obs)), alpha = 0.6) +
  geom_point(aes(y=scale(do.obs)), size=2, alpha = 0.75) +
  
  
  #geom_bar( aes(y=par/coeff), stat="identity", size=.1, fill=priceColor, color="black", alpha=.4) + 
  geom_area(aes(y=scale(par/coeff)), stat="identity", size=.1, fill="goldenrod", alpha=.2) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "DO (mgL)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="PAR")
  ) + 
  theme_bw() +
  scale_x_datetime(date_breaks = "3 hour", date_labels = "%H:%M")+
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "goldenrod", size=13)
  ) 



