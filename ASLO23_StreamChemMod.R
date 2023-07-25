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

# read in stream data:

streamchem <- read.csv("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/Covariate_dat/ASLO_StreamWaterChem.csv")
str(streamchem)

SCHEM <- streamchem %>%
  select(Site, Date, Accepted_NO3, Accepted_oPhos, Accepted_NH3, Shimadzu.DOC.mgL)%>%
  mutate(date = as.Date((Date), format ="%Y-%m-%d"))

str(SCHEM)
SCHEM$Accepted_oPhos <- as.numeric(SCHEM$Accepted_oPhos)

SCHEM1 <- SCHEM %>%
  mutate(shore = case_when(
    Site %in% c("BWL", "BWU", "BW0.5m", "BWO") ~ "west",
    Site %in% c("GBL", "GBU", "GB0.5m", "GBO") ~ "east",
    TRUE ~ "unknown"  # Assign "unknown" for unmatched site IDs (optional)
  ))



# GPP_df 
NSGPP_StreamChem <-left_join(NS_GPP_df, SCHEM1, 
                         by=c('date'='date', 'shore'='shore'))
summary(NSGPP_StreamChem)


NSGPP_Stream$Q_log <- log(NSGPP_Stream$dischargeCFS)
hist(NSGPP_Stream$GPP_mean)


# mod 1 cor
RF_stream_chem <- chart.Correlation(NSGPP_StreamChem[,c(3,13,14,15,16)], histogram=TRUE, pch=19)

streamC_mod1 <- lmer(GPPL_mean ~ scale(Accepted_NO3) + 
                        scale(Accepted_oPhos) + 
                        scale(Accepted_NH3) +
                        scale(Shimadzu.DOC.mgL) +
                        (1|shore), data=NSGPP_StreamChem)
summary(streamC_mod1)
r.squaredGLMM(streamC_mod1)
vif(streamC_mod1)
hist(residuals(streamC_mod1))


streamC_mod1a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                        scale(Accepted_NO3) + 
                       scale(Accepted_oPhos) + 
                       scale(Accepted_NH3) +
                       #scale(Shimadzu.DOC.mgL) +
                       (1|shore), data=NSGPP_StreamChem)
summary(streamC_mod1a)
r.squaredGLMM(streamC_mod1a)
vif(streamC_mod1a)
hist(residuals(streamC_mod1a))

qqnorm(resid(streamC_mod1a))
qqline(resid(streamC_mod1a))

streamC_mod2a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                        #scale(Accepted_NO3) + 
                        scale(Accepted_oPhos) + 
                        scale(Accepted_NH3) +
                        scale(Shimadzu.DOC.mgL) +
                        (1|shore), data=NSGPP_StreamChem)
summary(streamC_mod2a)
r.squaredGLMM(streamC_mod2a)
vif(streamC_mod2a)
hist(residuals(streamC_mod2a))




streamC_mod3a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                        #scale(Accepted_NO3) + 
                        #scale(Accepted_oPhos) + 
                        scale(Accepted_NH3) +
                        scale(Shimadzu.DOC.mgL) +
                        (1|shore), data=NSGPP_StreamChem)
summary(streamC_mod3a)
r.squaredGLMM(streamC_mod3a)
vif(streamC_mod3a)
hist(residuals(streamC_mod3a))




streamC_mod4a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                        #scale(Accepted_NO3) + 
                        #scale(Accepted_oPhos) + 
                        scale(Accepted_NH3) +
                        (1|shore), data=NSGPP_StreamChem)
summary(streamC_mod4a)
r.squaredGLMM(streamC_mod4a)
vif(streamC_mod4a)
hist(residuals(streamC_mod4a))



streamC_mod4a <- lmer(GPPL_mean ~ lag(GPPL_mean, 1)+
                        scale(Accepted_NO3) + 
                        #scale(Accepted_oPhos) + 
                        #scale(Accepted_NH3) +
                        (1|shore), data=NSGPP_StreamChem)
summary(streamC_mod4a)
r.squaredGLMM(streamC_mod4a)
vif(streamC_mod4a)
hist(residuals(streamC_mod4a))

AIC(streamC_mod1,streamC_mod1a,streamC_mod2a, streamC_mod3a)


coef_table <- summary(streamC_mod1a)$coefficients

# Create a data frame for plotting
plot_data <- data.frame(
  Term = rownames(coef_table),
  Estimate = fixef(streamC_mod1a),
  Lower = coef_table[, 1],
  Upper = coef_table[, 2]
)

plot_data1<- plot_data[c(-1),]

plot_data2 <- within(plot_data1, {
  RowNames <- rownames(plot_data1)
  RowNames[RowNames == "scale(Accepted_NO3)"] <- "NO3"
  RowNames[RowNames == "scale(Accepted_oPhos)"] <- "PO4"
  RowNames[RowNames == "scale(Accepted_NH3)"] <- "NH3"
  RowNames[RowNames == "scale(Shimadzu.DOC.mgL)"] <- "DOC"
  RowNames[RowNames == "lag(GPPL_mean, 1)"] <- "lag GPP"
})


coeffplotChem <- ggplot(plot_data2, aes(x = Estimate, y = RowNames)) +
  geom_point() +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  
  geom_errorbarh(aes(xmin = Estimate-Upper, xmax = Estimate+Upper), height = 0) +
  labs(x = "Estimate", y = "Parameter") +
  theme_minimal()

ggsave(plot = coeffplotChem, filename = paste("/Users/kellyloria/Documents/UNR/Conferences/ASLO2023/Analysis/figures/COEF_chem.png",sep=""),width=6.5,height=2,dpi=300)





