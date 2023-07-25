library(lmerTest)
library(lme4)
library(Boruta)
library(mlbench)
library(randomForest)
library(PerformanceAnalytics)
library(MuMIn)
library(car)

set.seed(111)
# load in dataframe: NS_GPP_df, NS_ER_df, Streamdf

# GPP_df 
NSGPP_Weath <-left_join(NS_GPP_df, weatherdf3, 
                         by=c('date'='date', 'shore'='shore'))
summary(NSGPP_Weath)

# remove some silly varibles
NSGPP_Weath<- NSGPP_Weath[,c(-5,-6,-18,-19,-34,-35)]
RF_W<- na.omit(NSGPP_Weath[,c(-1,-2,-4,-5,-6,-7,-8,-32,-33)])

boruta_Weath<- Boruta(RF_W$GPP_mean ~ ., data = RF_W[,c(-1)], doTrace = 2, maxRuns = 500)
print(boruta_Weath)

# RF plot
png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/RF_weather.png', width = 800, height = 800, res = 150)
plot(boruta_Weath, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_Weath$ImpHistory),function(i)
  boruta_Weath$ImpHistory[is.finite(boruta_Weath$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_Weath$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_Weath$ImpHistory), cex.axis = 0.45)
dev.off()

# cor chart then RF
RF_weather_cplot <- chart.Correlation(RF_W, histogram=TRUE, pch=19)

# a priori model
# temp, wind, precip
hist(NSGPP_Weath$airT_mean)
hist(NSGPP_Weath$Windsp_mean)
NSGPP_Weath$Windsp_log <- log(NSGPP_Weath$Windsp_mean)
hist(log(NSGPP_Weath$Windsp_mean))
hist(log(NSGPP_Weath$precipitation))
NSGPP_Weath$precip_log <- log(NSGPP_Weath$precipitation)

hist(log(NSGPP_Weath$snow_water_equivalent))
NSGPP_Weath$SWE_log <-(log(NSGPP_Weath$snow_water_equivalent+1))



# mod 1 cor
RF_W_cplot1 <- chart.Correlation(NSGPP_Weath[,c(3,12,34,35)], histogram=TRUE, pch=19)

Weath_mod1 <- lmer(GPPL_mean ~ scale(airT_mean) + 
                        scale(Windsp_mean) + 
                        scale(precipitation) + 
                        (1|shore), data=NSGPP_Weath)
summary(Weath_mod1)
r.squaredGLMM(Weath_mod1)
vif(Weath_mod1)
qqnorm(resid(Weath_mod1))
qqline(resid(Weath_mod1))

Weath_mod1a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                      scale(airT_mean) + 
                     scale(Windsp_mean) + 
                     scale(precipitation) + 
                     (1|shore), data=NSGPP_Weath)
summary(Weath_mod1a)
r.squaredGLMM(Weath_mod1a)
vif(Weath_mod1a)

qqnorm(resid(Weath_mod1a))
qqline(resid(Weath_mod1a))

Weath_mod2 <- lmer(GPPL_mean ~ scale(airT_mean) + 
                     scale(Windsp_mean) + 
                     (1|shore), data=NSGPP_Weath)
summary(Weath_mod2)
r.squaredGLMM(Weath_mod2)

qqnorm(resid(Weath_mod2))
qqline(resid(Weath_mod2))

Weath_mod2a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1) +
                      scale(airT_mean) + 
                     scale(Windsp_mean) + 
                     (1|shore), data=NSGPP_Weath)
summary(Weath_mod2a)
r.squaredGLMM(Weath_mod2a)
qqnorm(resid(Weath_mod2a))
qqline(resid(Weath_mod2a))




RF_W_cplot2 <- chart.Correlation(NSGPP_Weath[,c(3,9,10,12,29,34)], histogram=TRUE, pch=19)


Weath_mod3 <- lmer(GPPL_mean ~ scale(airT_mean) + scale(Winddr_mean)+
                     scale(precipitation_cumulative) + 
                     scale(SolarR_sd) + 
                     (1|shore), data=NSGPP_Weath)
summary(Weath_mod3)
r.squaredGLMM(Weath_mod3)
vif(Weath_mod3)
qqnorm(resid(Weath_mod3))
qqline(resid(Weath_mod3))


Weath_mod3a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                     scale(airT_mean) + scale(Windsp_max)+
                     scale(precipitation_cumulative) + 
                     scale(SolarR_sd) + 
                     (1|shore), data=NSGPP_Weath)
summary(Weath_mod3a)
r.squaredGLMM(Weath_mod3a)
vif(Weath_mod3a)
qqnorm(resid(Weath_mod3a))
qqline(resid(Weath_mod3a))

AIC(Weath_mod1,Weath_modla, Weath_mod2, Weath_mod2a, Weath_mod3, Weath_mod3a)



Weath_mod2a <- lmer(GPPL_mean ~ scale(airT_mean) + scale(Winddr_mean)+
                     scale(SolarR_sd) + 
                     (1|shore), data=NSGPP_Weath)
summary(Weath_mod2a)
r.squaredGLMM(Weath_mod2a)
vif(Weath_mod2a)

AIC(Weath_modla, Weath_mod1, Weath_mod2, Weath_mod2a)


png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/Weath_modeldiag.png', width = 1000, height = 600, res = 150)
par(mfrow=c(1,2))
qqnorm(resid(Weath_modla))
qqline(resid(Weath_modla))
hist(residuals(Weath_modla))
dev.off()

W_p1 <- ggplot(data = NSGPP_Weath, aes(airT_mean, GPP_mean, color = shore))+
  geom_errorbar(aes(ymin = GPPL_mean, ymax = GPPU_mean, color = shore), alpha = 0.15) +
  geom_errorbarh(aes(xmin = c(airT_mean-airT_sd), 
                     xmax = c(airT_mean+airT_sd)), alpha = 0.15) +
  geom_point(aes(color = shore),alpha = 0.3) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  labs(x="Air temp (C)") +
  geom_smooth(method="lm", se = FALSE) +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.position = 'bottom', 
        legend.direction = "horizontal")

# ggsave(plot = W_p1, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/GPP_W1.png",sep=""),width=6,height=6,dpi=300)

W_p2 <- ggplot(data = NSGPP_Weath, aes(Windsp_mean, GPP_mean, color = shore))+
  geom_errorbar(aes(ymin = GPPL_mean, ymax = GPPU_mean, color = shore), alpha = 0.15) +
  geom_errorbarh(aes(xmin = c(Windsp_mean-Windsp_sd), 
                     xmax = c(Windsp_mean+Windsp_sd)), alpha = 0.15) +
  geom_point(aes(color = shore),alpha = 0.3) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  labs(x="Wind speed") +
  geom_smooth(method="lm", se = FALSE) +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.position = 'bottom', 
        legend.direction = "horizontal")
# ggsave(plot = W_p2, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/GPP_W2.png",sep=""),width=6,height=6,dpi=300)




# best model Weath_mod3a


coef_table <- summary(Weath_mod3a)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(Weath_mod3a),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(airT_mean)"] <- "Air temperature"
  RowNames[RowNames == "scale(Windsp_max)"] <- "Max. wind speed"
  RowNames[RowNames == "scale(precipitation_cumulative)"] <- "Precipitation cumulative"
  RowNames[RowNames == "scale(SolarR_sd)"] <- "Solar radiation variance"
  RowNames[RowNames == "lag(GPPL_mean, 1)"] <- "lag GPP"
})


coeffplotC <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter") +
  theme_minimal()

ggsave(plot = coeffplotC, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_CimM.png",sep=""),width=4.25,height=2,dpi=300)

