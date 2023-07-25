#############
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
NSGPP_Stream <-left_join(NS_GPP_df, Streamdf, 
                         by=c('date'='datetime', 'shore'='shore'))

NSGPP_Stream$Q_log <- log(NSGPP_Stream$dischargeCFS)

hist(NSGPP_Stream$GPP_mean)

RF_stream<- na.omit(NSGPP_Stream[,c(-1,-2,-4,-5,-6,-7,-8,-9,-10,-12)])
str(RF_stream)

hist(RF_stream$GPP_mean)
hist(RF_stream$dischargeCFS)
hist(log(RF_stream$dischargeCFS))
hist(RF_stream$Stream_SPCmean)
hist(log(RF_stream$Stream_SPCmean))
hist(RF_stream$Stream_Wtmean)
hist(RF_stream$stage_ftm)
hist(RF_stream$stage_ftm)
hist(RF_stream$Stream_depth_mean)
hist(log(RF_stream$Stream_depth_mean))
hist(RF_stream$Stream_DOobs_mean)

# Transformed
NSGPP_Stream$Q_log <- log(NSGPP_Stream$dischargeCFS)
NSGPP_Stream$SPC_log <-(log(NSGPP_Stream$Stream_SPCmean))
hist(NSGPP_Stream$Q_log)

## Quick Random Forest
RF_stream<- na.omit(NSGPP_Stream[,c(-1,-2,-4,-5,-6,-7,-8,-9,-10,-12)])
str(RF_stream)

boruta_stream <- Boruta(RF_stream$GPP_mean ~ ., data = RF_stream[,c(-1)], doTrace = 2, maxRuns = 500)
print(boruta_stream)

# RF plot
png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/RF_streamPHY.png', width = 800, height = 800, res = 150)
plot(boruta_stream, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_stream$ImpHistory),function(i)
  boruta_stream$ImpHistory[is.finite(boruta_stream$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_stream$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_stream$ImpHistory), cex.axis = 0.45)
dev.off()

# cor chart then RF
RF_stream_cplot <- chart.Correlation(RF_stream, histogram=TRUE, pch=19)

# a priori model
# temperature, flow, depth
# mod 1 cor
RF_stream_cplot1 <- chart.Correlation(RF_stream[,c(1,5,8,13,16)], histogram=TRUE, pch=19)

streamP_mod1a <- lmer(GPPL_mean ~ scale(Stream_Wtmean) + 
                       scale(Stream_depth_sd) + 
                       scale(Q_log) + 
                       (1|shore), data=NSGPP_Stream)
summary(streamP_mod1a)
r.squaredGLMM(streamP_mod1)
vif(streamP_mod1a)


streamP_mod1 <- lmer(GPPL_mean ~ scale(Stream_Wtmean) + 
                        scale(Q_log) + 
                        (1|shore), data=NSGPP_Stream)
summary(streamP_mod1)
r.squaredGLMM(streamP_mod1)
vif(streamP_mod1)
AIC(streamP_mod1a, streamP_mod1)

# RF suggested model
# mean light
# do mean 
RF_stream_cplot2 <- chart.Correlation(RF_stream[,c(1,7,9,15,16)], histogram=TRUE, pch=19)

streamP_mod2 <- lmer(GPPL_mean ~ scale(Stream_light_mean) + 
                        scale(Stream_DOsat_mean) + 
                        (1|shore), data=NSGPP_Stream)
summary(streamP_mod2)
r.squaredGLMM(streamP_mod2)
vif(streamP_mod2)

AIC(streamP_mod2,streamP_mod1a, streamP_mod1)


## A LOT of things are correlated with flow... could be worth using an SEM
png('/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/StreamPHY_modeldiag.png', width = 1000, height = 600, res = 150)
par(mfrow=c(1,2))
qqnorm(resid(streamP_mod1))
qqline(resid(streamP_mod1))
hist(residuals(streamP_mod1))
dev.off()


SPM_p <- ggplot(data = NSGPP_Stream, aes(lag(GPP_mean,1), GPP_mean, color = shore))+
  geom_abline(intercept = 0, slope = 1,linetype = "dashed", color = "grey25") +
  # geom_errorbar(aes(ymin = GPPL_mean, ymax = GPPU_mean, color = shore), alpha = 0.005) +
  # geom_errorbarh(aes(xmin = c(lag(GPPL_mean,1)), 
  #                    xmax = c(lag(GPPU_mean,1)), alpha = 0.005)) +
  geom_point(aes(color = shore),alpha = 0.4) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  labs(x=expression("Next day GPP"~mmol~O[2]~m^-3~d^-1)) +
  #geom_smooth(method="lm", se = FALSE, alpha = 0.4) +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.position = 'bottom', 
        legend.direction = "horizontal")



SPM_p <- ggplot(data = NSGPP_Stream, aes(Stream_Wtmean, GPP_mean, color = shore))+
  geom_errorbar(aes(ymin = GPPL_mean, ymax = GPPU_mean, color = shore), alpha = 0.15) +
  geom_errorbarh(aes(xmin = c(NSGPP_Stream$Stream_Wtmean-NSGPP_Stream$Stream_Wtstd), 
                     xmax = c(NSGPP_Stream$Stream_Wtmean+NSGPP_Stream$Stream_Wtstd), alpha = 0.15)) +
  geom_point(aes(color = shore),alpha = 0.3) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  labs(x="Water temp (C)") +
  geom_smooth(method="lm", se = FALSE) +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.position = 'bottom', 
        legend.direction = "horizontal")

# ggsave(plot = SPM_p, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/GPPAuto_SPM.png",sep=""),width=6,height=6,dpi=300)

SPM_p2 <- ggplot(data = NSGPP_Stream, aes(Q_log, GPP_mean, color = shore))+
  geom_errorbar(aes(ymin = GPPL_mean, ymax = GPPU_mean, color = shore), alpha = 0.15) +
  geom_point(aes(color = shore),alpha = 0.3) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  labs(x="log(stream flow) (cfms)") +
  #geom_smooth(method="lm", se = FALSE) +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.position = 'bottom', 
        legend.direction = "horizontal")

# ggsave(plot = SPM_p2, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/GPP_SPM2.png",sep=""),width=6,height=6,dpi=300)


SPM_p3 <- ggplot(data = NSGPP_Stream, aes(Stream_depth_sd, GPP_mean, color = shore))+
  geom_errorbar(aes(ymin = GPPL_mean, ymax = GPPU_mean, color = shore), alpha = 0.15) +
  geom_point(aes(color = shore),alpha = 0.3) +
  labs(y=expression("GPP"~mmol~O[2]~m^-3~d^-1)) + 
  labs(x=" Reach depth variance (m)") +
  geom_smooth(method="lm", se = FALSE) +
  scale_color_manual(values=c("#a67d17", "#3283a8")) +
  scale_fill_manual(values = c("#a67d17", "#3283a8")) +
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.position = 'bottom', 
        legend.direction = "horizontal")

# ggsave(plot = SPM_p2, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/GPP_SPM2.png",sep=""),width=6,height=6,dpi=300)


# GPP_df 
NSER_Stream <-left_join(NS_ER_df, Streamdf, 
                         by=c('date'='datetime', 'shore'='shore'))

NSER_Stream$Q_log <- log(NSER_Stream$dischargeCFS)

hist(NSER_Stream$ER_mean)

RF_stream<- na.omit(NSGPP_Stream[,c(-1,-2,-4,-5,-6,-7,-8,-9,-10,-12)])
str(RF_stream)

hist(RF_stream$GPP_mean)
hist(RF_stream$dischargeCFS)
hist(log(RF_stream$dischargeCFS))
hist(RF_stream$Stream_SPCmean)
hist(log(RF_stream$Stream_SPCmean))
hist(RF_stream$Stream_Wtmean)
hist(RF_stream$stage_ftm)
hist(RF_stream$stage_ftm)
hist(RF_stream$Stream_depth_mean)
hist(log(RF_stream$Stream_depth_mean))
hist(RF_stream$Stream_DOobs_mean)

# Transformed
NSGPP_Stream$Q_log <- log(NSGPP_Stream$dischargeCFS)
NSGPP_Stream$SPC_log <-(log(NSGPP_Stream$Stream_SPCmean))
hist(NSGPP_Stream$Q_log)

## Quick Random Forest
RF_stream<- na.omit(NSGPP_Stream[,c(-1,-2,-4,-5,-6,-7,-8,-9,-10,-12)])
str(RF_stream)

boruta_stream <- Boruta(RF_stream$GPP_mean ~ ., data = RF_stream[,c(-1)], doTrace = 2, maxRuns = 500)
print(boruta_stream)


### 
### Let's subset for each shore
###

WGPP<- subset(NS_GPP_df, shore=="west")
WGPP.ts <- WGPP[, c(1,3)]
WGPP.ts1<-ts(WGPP.ts, start = c(2021, 7), end = c(2022, 05), frequency = 12)
plot(WGPP.ts1)
WGPP.ts1_comp <- decompose(WGPP.ts1)


EGPP<- subset(NS_GPP_df, shore=="east")



### Final model testing ##
# ecologically - flow and temp

model1 <- lmer(GPPL_mean ~ lag(GPPL_mean, 1) + 
                 scale(dischargeCFS) +
               scale(Stream_Wtmean)+ (1|shore), data = NSGPP_Stream)
summary(model1)
hist(residuals(model1))

qqnorm(resid(model1))
qqline(resid(model1))
r.squaredGLMM(model1)

model1a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1) + 
                 scale(Q_log) +
                 #scale(log(stage_ftm)) +
                 scale(Stream_Wtmean)+ (1|shore), data = NSGPP_Stream)
summary(model1a)
hist(residuals(model1a))

qqnorm(resid(model1a))
qqline(resid(model1a))
vif(model1a)
r.squaredGLMM(model1a)


model2 <- lmer(GPPL_mean ~  
                 scale(dischargeCFS) +
                 scale(Stream_Wtmean)+ (1|shore), data = NSGPP_Stream)
summary(model2)
hist(residuals(model3))
r.squaredGLMM(model2)

qqnorm(resid(model2))
qqline(resid(model2))

AIC(model1, model1a, model2)


coef_table <- summary(model1a)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(model1a),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(Stream_Wtmean)"] <- "Temperature"
  RowNames[RowNames == "scale(dischargeCFS)"] <- "log(Flow)"
  RowNames[RowNames == "lag(GPPL_mean, 1)"] <- "lag GPP"
})


coeffplot <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter") +
  theme_minimal()

ggsave(plot = coeffplot, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_SPM.png",sep=""),width=4.5,height=1.5,dpi=300)


### Final model testing ##
# ML - stage and streamLight


model3 <- lmer(GPPL_mean ~ lag(GPPL_mean, 1) + 
                 scale(stage_ftm) +
                 scale(Stream_light_mean)+ (1|shore), data = NSGPP_Stream)
summary(model3)
hist(residuals(model3))

qqnorm(resid(model3))
qqline(resid(model3))
r.squaredGLMM(model3)


model4 <- lmer(GPPL_mean ~  
                 scale(stage_ftm) +
                 scale(Stream_light_mean)+ (1|shore), data = NSGPP_Stream)
summary(model4)
hist(residuals(model4))
r.squaredGLMM(model4)

qqnorm(resid(model4))
qqline(resid(model4))

AIC(model1, model1a, model2, model3,model4)



