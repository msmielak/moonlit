
#install.packages("ggpubr")
library(ggpubr)
library(mgcv)
library(lme4)
library(bbmle)
library(glmmTMB)
library(MASS)
library(DHARMa)
library(forecast)
library(car)
library(emmeans)
library(effects)
library(multcomp)
library(MuMIn)
library(broom)
library(broom.mixed)
library(dotwhisker)
library(ggplot2); theme_set(theme_minimal())
library(texreg)
library(xtable)
library(huxtable)
library(lubridate)
library(sjPlot)
library(tidyverse)
library(visreg)
library(nlme)


###############################

#######
### UNE logger
#######


Sys.setenv( tz="Australia/Brisbane" )

UNE <- read.csv("./Datasets/STQ.csv")
UNE$date <- as.POSIXct(UNE$date)




p1 <- ggplot(UNE, aes(x=date))+
  geom_jitter(aes(y=lx), alpha = 0.05)+
  theme_minimal()+
  labs(y="Illuminance (lx)", title = "Logger")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(UNE, aes(x=date))+
  geom_jitter(aes(y=illuminationModel), alpha = 0.05)+
  theme_minimal()+
  labs(x="Date", y="Relative moonlight intensity", title = "Model")

#arrange plots togetherp1
ggarrange(p1, p2, 
          nrow=2)


### plotting measured values against model and moon phase

p3 <-   ggplot(UNE, aes(y=lx, x=illuminationModel))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Relative moonlight intensity", y="Illuminance (lx)", title = "Model")


p4 <- ggplot(UNE, aes(y=lx, x=fraction))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Fraction of moon illuminated", title = "Moon phase")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())

ggarrange(p3, p4)

### testing models
M2_phase <- lm(UNE$fraction ~ poly(UNE$lx, 3))
M2_phase_residuals <- density(residuals(M2_phase))
plot(M2_phase_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")

M2_model <- lm(UNE$illuminationModel ~ poly(UNE$lx, 3))
M2_model_residuals <- density(residuals(M2_model))
plot(M2_model_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")


### Compare error distributions

#errors of first model
x <- M2_model_residuals$x
y <- M2_model_residuals$y

errors1 <- data.frame(x,y)
errors1$model <- "model"

#add 2 data points at -1 and 1 for the logger (this is for data presentation only)
errors1[nrow(errors1) + 1,] = list(x=-1, y=0, model="model")
errors1[nrow(errors1) + 1,] = list(x=1, y=0, model="model")

p5 <- ggplot(errors1, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density", title = "Model")+
  theme(axis.title.x=element_blank())

#errors of second model
x <- M2_phase_residuals$x
y <- M2_phase_residuals$y

errors2 <- data.frame(x,y)
errors2$model <- "moon phase"

errors2[nrow(errors2) + 1,] = list(x=-1, y=0, model="moon phase")
errors2[nrow(errors2) + 1,] = list(x=1, y=0, model="moon phase")




p6 <- ggplot(errors2, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density", title = "Moon phase")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


ggarrange(p5, p6)



#merge them into one df
errors <- rbind(errors1, errors2)




#plot the two
ggplot(errors)+
  geom_line(aes(x=x, y=y, linetype=model))+
  labs(title="Residual distribution - custom logger", x="Residuals", y="Density")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8),  legend.background = element_rect(fill="white"))




######
### Custom logger model testing
#######

#load file for custom logger
customLogger <- read.csv("./Datasets/custom_logger_test.csv")
customLogger$date <- as.POSIXct(customLogger$date)


### plotting model and measurements

p1 <- ggplot(customLogger, aes(x=date))+
  geom_jitter(aes(y=voltage), alpha = 0.1)+
  theme_minimal()+
  labs(y="Voltage (V)", title = "Logger")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(customLogger, aes(x=date))+
  geom_jitter(aes(y=illuminationModel), alpha = 0.1)+
  theme_minimal()+
  labs(x="Date", y="Relative moonlight intensity", title = "Model")

#arrange plots together
ggarrange(p1, p2, 
          nrow=2)


### plotting measured values against model and moon phase

p3 <-   ggplot(customLogger, aes(y=voltage, x=illuminationModel))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Relative moonlight intensity", y="Voltage", title = "Model")
  

p4 <- ggplot(customLogger, aes(y=voltage, x=fraction))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Fraction of moon illuminated", title = "Moon phase")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())

ggarrange(p3, p4)

### testing models
M1_phase <- lm(customLogger$fraction ~ poly(customLogger$lx, 3))
M1_phase_residuals <- density(residuals(M1_phase))
plot(M1_phase_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")

M1_model <- lm(customLogger$illuminationModel ~ poly(customLogger$lx, 3))
M1_model_residuals <- density(residuals(M1_model))
plot(M1_model_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")


### Compare error distributions

#errors of first model
x <- M1_model_residuals$x
y <- M1_model_residuals$y

errors1 <- data.frame(x,y)
errors1$model <- "model"

#add 2 data points at -1 and 1 for the logger (this is for data presentation only)
errors1[nrow(errors1) + 1,] = list(x=-1, y=0, model="model")
errors1[nrow(errors1) + 1,] = list(x=1, y=0, model="model")

p5 <- ggplot(errors1, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density", title = "Model")+
  theme(axis.title.x=element_blank())

#errors of second model
x <- M1_phase_residuals$x
y <- M1_phase_residuals$y

errors2 <- data.frame(x,y)
errors2$model <- "moon phase"

p6 <- ggplot(errors2, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density", title = "Moon phase")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


ggarrange(p5, p6)



#merge them into one df
errors <- rbind(errors1, errors2)




#plot the two
ggplot(errors)+
  geom_line(aes(x=x, y=y, linetype=model))+
  labs(title="Residual distribution - custom logger", x="Residuals", y="Density")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8),  legend.background = element_rect(fill="white"))


######
### SQM logger
######



SQM <- read.csv("./Datasets/Gnosca.csv")
SQM$date <- as.POSIXct(SQM$date)

#SQM$measurement <- 108000*10^(-0.4*SQM$lx)




### plotting model and measurements

p1 <- ggplot(SQM, aes(x=date))+
  geom_jitter(aes(y=lx), alpha = 0.03)+
  theme_minimal()+
  labs(y="Luminance (cd/m²)", title = "Logger")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(SQM, aes(x=date))+
  geom_jitter(aes(y=illuminationModel), alpha = 0.03)+
  theme_minimal()+
  labs(x="Date", y="Relative moonlight intensity", title = "Model")

#arrange plots together
ggarrange(p1, p2, 
          nrow=2)


### plotting measured values against model and moon phase

p3 <-   ggplot(SQM, aes(y=lx, x=illuminationModel))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Relative moonlight intensity", y="Luminance (cd/m²)", title = "Model")


p4 <- ggplot(SQM, aes(y=lx, x=fraction))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Fraction of moon illuminated", title = "Moon phase")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())

ggarrange(p3, p4)

### testing models
M2_phase <- lm(SQM$fraction ~ poly(SQM$lx, 3))
M2_phase_residuals <- density(residuals(M2_phase))
plot(M2_phase_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")

M2_model <- lm(SQM$illuminationModel ~ poly(SQM$lx, 3))
M2_model_residuals <- density(residuals(M2_model))
plot(M2_model_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")


### Compare error distributions

#errors of first model
x <- M2_model_residuals$x
y <- M2_model_residuals$y

errors1 <- data.frame(x,y)
errors1$model <- "model"

#add 2 data points at -1 and 1 for the logger (this is for data presentation only)
errors1[nrow(errors1) + 1,] = list(x=-1, y=0, model="model")
errors1[nrow(errors1) + 1,] = list(x=1, y=0, model="model")

p5 <- ggplot(errors1, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density", title = "Model")+
  theme(axis.title.x=element_blank())

#errors of second model
x <- M2_phase_residuals$x
y <- M2_phase_residuals$y

errors2 <- data.frame(x,y)
errors2$model <- "moon phase"

errors2[nrow(errors2) + 1,] = list(x=-1, y=0, model="moon phase")
errors2[nrow(errors2) + 1,] = list(x=1, y=0, model="moon phase")




p6 <- ggplot(errors2, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density", title = "Moon phase")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())


ggarrange(p5, p6)



#merge them into one df
errors <- rbind(errors1, errors2)




#plot the two
ggplot(errors)+
  geom_line(aes(x=x, y=y, linetype=model))+
  labs(title="Residual distribution - custom logger", x="Residuals", y="Density")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8),  legend.background = element_rect(fill="white"))





#####################################
#####################################
### Case study - Paddysland possum dataset
# 
# #create a subset for PL data only 
# PL <- subset(BTP, Site=="PL")
# PL <- BTP
# 
# date1 <- seq(from=min(PL$date), to=max(PL$date), by="hour")
# 
# PL_moon <- calculateMoonlightIntensity(lat=mean(PL$lat), lon=mean(PL$lon), date=date1, e=0.26)
# 
# #write.csv(PL, "./Datasets/Paddysland_BTP_test_data.csv")
# 

#loading paddysland dataset
PL <- read.csv("./Datasets/Paddysland_BTP_test_data.csv")
PL$date <- as.POSIXct(PL$date)

#extract first and last record
min(PL$date)
max(PL$date)

#calculate time series of nightly detections
PL_daily <- timeAverage(PL, start.date = "2014-12-15 12:00:00", period = "24 hours", statistic = "frequency")

#subset only date and number of detections
PL_daily <- PL_daily[c("date", "BTP")]

#replace NAs with 0
PL_daily[is.na(PL_daily)] <- 0

PL_daily$day <- as.Date(PL_daily$date)

PL_daily$detrend <- PL_daily$day - min(PL_daily$day)

ggplot(PL_daily, aes(x=date, y=BTP))+
  geom_line()+
  geom_smooth()+
  theme_minimal()+
  labs(x="Date", y="Daily detection rate")


#calculating mean illumination and moon phase values
PL_mean <- timeAverage(PL_moon, start.date = "2014-12-15 12:00:00", period = "24 hours", statistic = "mean")

#merging into the daily detections dataset
PL_daily$meanMoonIllumination <- PL_mean$moonlightModel
PL_daily$moonPhase <- PL_mean$moonPhase

# test plots
ggplot(PL_daily, aes(x=moonPhase, y=BTP))+
  geom_jitter()+
  geom_smooth()

ggplot(PL_daily, aes(x=meanMoonIllumination, y=BTP))+
  geom_jitter()+
  geom_smooth()

#######################
### Model building and testing


library(stargazer)
library(gtsummary)
library(texreg)
library(DHARMa)
library(tidyr)


#model for moon phase
m1 <- glmmTMB(data=PL_daily, family ="nbinom1", formula = BTP~moonPhase +(1|detrend))


summary(m1)

#effect plots
plot(allEffects(m1))

#with residuals
plot(effect(mod=m1, term = "moonPhase", partial.residuals=TRUE),
     main="Moon phase",
     xlab="Moon phase",
     ylab="Daily detections",
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     partial.residual=list(pch="+",col="grey"),
     transform= NULL
)

# model for moonlight intensity
m2 <- glmmTMB(data=PL_daily, family ="nbinom1", formula = BTP~meanMoonIllumination+ (1|detrend) )

summary(m2)

#effect plots
plot(allEffects(m2))

#with residuals
plot(effect(mod=m2, term = "meanMoonIllumination", partial.residuals=TRUE),
     main="Illumination",
     xlab="Mean moon illumination",
     ylab="Daily detections",
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     partial.residual=list(pch="+",col="grey"),
     transform= NULL
)


#comparison table
tab_model(m1, m2, transform=NULL)




#############################################
#### Generalised Least Squares model

PL1 <- subset(PL, maxMoonFraction >0.5)
PL1 <- subset(PL, meanMoonIllumination >0.02)

summary(PL1)
hist(PL1$meanMoonFraction)





m.lm <- gls(data=PL1, moonlightDifference~meanMoonIllumination)

summary(m.lm)
plot(m.lm)
plot(allEffects(m.lm))


vf1Fixed <- varFixed(~meanMoonIllumination)
M.gls1<-gls( moonlightDifference~meanMoonIllumination,
            weights=vf1Fixed,data=PL1)

summary(M.gls1)
plot(M.gls1)
plot(allEffects(M.gls1))


vf2 <- varConstPower(form= ~meanMoonIllumination)
M.gls2<-gls( moonlightDifference~meanMoonIllumination,
             weights=vf2,data=PL1)

summary(M.gls2)
plot(M.gls2)
plot(allEffects(M.gls2))

tab_model(M.gls2)


vf3 <- varExp(form =~ meanMoonIllumination)
M.gls3<-gls( moonlightDifference~meanMoonIllumination,
             weights=vf3 ,data=PL1)

summary(M.gls3)

plot(M.gls3)



AICtab(m.lm, M.gls1, M.gls2, M.gls3)
tab_model(M.gls1, M.gls2, M.gls3)


plot(effect(mod=M.gls2, term = "meanMoonIllumination", partial.residuals=TRUE),
     main=NULL,
     xlab="Mean moon illumination",
     ylab="Moonlight preference",
     partial.residual=list(pch="+",col="grey"),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)


#################################
### Testing for preference within each night


gam1 <- gam(moonlightDifference~meanMoonIllumination,
             data=PL1)
summary(gam1)
plot(gam1)


##############




library(ggeffects)
library(car)

# plotting response variable to find its distibution
ggplot(PL1)+
  geom_density(aes(x=moonlightDifference))

ggplot(PL1)+
  geom_density(aes(x=relativeMoonlightDifference))




m3 <- lm(data=PL1, formula = moonlightDifference~
          meanMoonIllumination)


m3b <- lm(data=PL1, formula = relativeMoonlightDifference~
           meanMoonIllumination)

AICtab(m3, m3a, m3b, m4)



plot(m3, which = 1)
summary(m3)
plot(allEffects(m3))

plot(effect(mod=m3, term = "meanMoonIllumination", partial.residuals=TRUE),
     main="Illumination",
     xlab="Mean moon illumination",
     ylab="Preference",
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     partial.residual=list(pch="+",col="grey"),
     transform= NULL
)



ggplot(PL, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point(alpha=0.3)+
  geom_smooth()


m3a <- glmmTMB(data=PL, formula = moonlightDifference~
                 meanMoonIllumination, weights = maxMoonIllumination)


plot(m3a)

summary(m3a)
plot(allEffects(m3a))

plot(effect(mod=m3a, term = "meanMoonIllumination", partial.residuals=TRUE),
     main="Illumination",
     xlab="Moon phase",
     ylab="Preference",
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     partial.residual=list(pch="+",col="grey"),
     transform= NULL
)



ggplot(PL, aes(x=meanMoonIllumination, y=relativeMoonlightDifference))+
  geom_point(alpha=0.3)+
  geom_smooth()


m4 <- lm(data=PL, formula = relativeMoonlightDifference~
                meanMoonIllumination)

summary(m4)
plot(m4)


plot(effect(mod=m4, term = "meanMoonIllumination", partial.residuals=TRUE),
     main="Illumination",
     xlab="Mean moon illumination",
     ylab="Relative preference",
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     partial.residual=list(pch="+",col="grey"),
     transform= NULL
)



ggplot(PL, aes(x=maxMoonFraction, y=relativeMoonlightDifference))+
  geom_point(alpha=0.3)+
  geom_smooth()




m5 <- lm(data=PL, formula = relativeMoonlightDifference~
                maxMoonFraction)

plot(m5)

plot(effect(mod=m5, term = "maxMoonFraction", partial.residuals=TRUE),
     main="Illumination",
     xlab="Moon phase",
     ylab="Relative preference",
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     partial.residual=list(pch="+",col="grey"),
     transform= NULL
)


ggplot(PL, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point(pch="+",col="grey")+
  geom_smooth(method="lm")+
  geom_line(y=0)+
  labs(x="Mean moonlight intensity", y="Moonlight preference")


library(lmtest)

bptest(m3)
bptest(m4)
bptest(m5)


summary(hccm(m3))

coeftest(m3,vcov=hccm)

library(estimatr)

m3_robust <- lm_robust(data=PL, formula = moonlightDifference~
                         meanMoonIllumination)
summary(m3_robust)





#### A linear model with non-constant variances using package lmvar
library(lmvar)

X = model.matrix(~ meanMoonIllumination -1, PL)
fit = lmvar(PL$moonlightDifference, X_mu = X, X_sigma = X, intercept_mu = FALSE, intercept_sigma = FALSE)

summary(fit)
sigma = fitted(fit, mu = FALSE)

plot(PL$moonlightDifference, residuals(fit) / sigma, xlab = "Moonlight intensity", ylab = "z-score")
abline(0, 0, col = "red")

mu = fitted(fit, sigma = FALSE)
plot(PL$moonlightDifference, mu)
abline(0, 0, col = "red")

fit_lm = lm(PL$moonlightDifference ~ meanMoonIllumination - 1, PL)

AIC(fit); AIC(fit_lm)


### Another approach - package sandwich

library(sandwich)
library(lmtest)

var_cov <- vcovHC(m3, tyle="HC4")
coeftest(m3, df = Inf, var_cov)

#################################
### Alternative test - dividing data into quantiles

PL1 <- subset (PL, maxMoonFraction >0.05 & maxMoonFraction <0.25)
ggplot(PL1)+
  geom_density(aes(x=moonlightDifference))

PL2 <- subset (PL, maxMoonFraction >0.25 & maxMoonFraction <0.5)
ggplot(PL2)+
  geom_density(aes(x=moonlightDifference))


PL3 <- subset (PL, maxMoonFraction >0.5 & maxMoonFraction <0.75)
ggplot(PL3)+
  geom_density(aes(x=moonlightDifference))

PL4 <- subset (PL, maxMoonFraction >0.75)
ggplot(PL4)+
  geom_density(aes(x=moonlightDifference))


library(ggridges)

PL$bins <- cut(PL$maxMoonFraction, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"))

PL$bins <- cut(PL$meanMoonIllumination, breaks=c(-0.01, 0.1, 0.2, 0.3, 0.4, 1), labels=c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", ">0.4"))


PL$bins2 <- cut(PL$maxMoonFraction, breaks=c(0, 0.25, 0.5, 0.75, 1), labels=c( "0-25%", "25-50%", "50-75%", "75-100%"))

ggplot(PL, aes(x=moonlightDifference, y=bins))+
  geom_boxplot()

ggplot(PL, aes(x=moonlightDifference, y=bins))+
  geom_density_ridges(scale=2, quantile_lines = TRUE, quantiles=2, alpha=0.5)+
  labs(x="Moonlight preference", y="Mean moonlight intensity")

ggplot(PL, aes(x=moonlightDifference, y=bins2))+
  geom_density_ridges2()


ggplot(PL, aes(x=relativeMoonlightDifference, y=bins))+
  geom_density_ridges2()


ggplot(PL1, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point(alpha=0.3)+
  geom_line(aes(y=0))+
  geom_smooth(method="lm")

ggplot(PL2, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point(alpha=0.3)+
  geom_line(aes(y=0))+
  geom_smooth(method="lm")

ggplot(PL3, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point(alpha=0.3)+
  geom_line(aes(y=0))+
  geom_smooth(method="lm")

ggplot(PL4, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point(alpha=0.3)+
  geom_line(aes(y=0))+
  geom_smooth(method="lm")



PL$Active <- PL$moonlightModel
PL$Mean <- PL$meanMoonIllumination

pd <- gather(PL, group, measure, c(Active, Mean), factor_key=T)

pd1 <- subset (pd, maxMoonFraction <0.25)
pd2 <- subset (pd, maxMoonFraction >0.25 & maxMoonFraction <0.5)
pd3 <- subset (pd, maxMoonFraction >0.5 & maxMoonFraction <0.75)
pd4 <- subset (pd, maxMoonFraction >0.75)

mod1 <- lm(data=pd1, measure~group)
summary(mod1)
#plot(mod1)
plot(effect(mod1, term= "group"),
     main="Measured vs mean value",
     ylab="Illumination"
     #,ylim=c(0,0.25)
) 

mod2 <- lm(data=pd2, measure~group)
summary(mod2)
plot(allEffects(mod2),
     main="Measured vs mean value",
     ylab="Illumination"
     #,ylim=c(0,0.25)
) 

mod3 <- lm(data=pd3, measure~group)
summary(mod3)
plot(allEffects(mod3),
     main="Measured vs mean value",
     ylab="Illumination"
     #,ylim=c(0,0.25)
) 


mod4 <- lm(data=pd4, measure~group)
summary(mod4)
plot(allEffects(mod4),
     main="Measured vs mean value",
     ylab="Illumination"
     #,ylim=c(0,0.25)
) 



#################################
#################################


### binomial regression with pseudo-absence


#Taking paddysland presence records

PL_presence <- PL[, c("date", "BTP", "lat", "lon")]

#generating pseudo absence
timeMin <- min(PL_presence$date)
timeMax <- max(PL_presence$date)
  
#random datapoionts from the same date ranges
random <- as.POSIXct(sample(timeMin:timeMax, 10000), origin = '1970-01-01')
random_moon <- calculateMoonlightIntensity(lat = mean(PL$lat), lon=mean(PL$lon), date = random, e=0.16)
random_moon$active <- 0

#subset night only
random_moon <- subset(random_moon, night==TRUE)

#presence datapoints
PL_moon <- calculateMoonlightIntensity(lat=PL$lat, lon=PL$lon, date=PL$date, e=0.16)
PL_moon$active <- PL$BTP

#combined dataframes
newdata <- rbind(PL_moon, random_moon)

plot(newdata$date, newdata$moonlightModel)


#### Plotting probabilities with confidence intervals for moonlight
logit_moonlight <- glm(active~moonlightModel, family = binomial(link="logit"), data = newdata)
summary(newdata$moonlightModel)

plotdat <- data.frame(moonlightModel=(seq(0,0.86,0.001)))
preddat = predict(logit_moonlight, newdata = plotdat, se.fit=TRUE)
summary(preddat)

with(newdata, plot(moonlightModel, active, type="n", 
                   ylim=c(0, 1),  ylab="Probability of detection", xlab="Modelled relative moonlight intensity", main="Probablity of detection vs moonlight intensity"))
with(preddat, lines(plotdat$moonlightModel, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(plotdat$moonlightModel, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(plotdat$moonlightModel, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))
with(preddat, points(data$moonlightModel, data$active))








tbl_regression(M1_model)

tbl_merge(tbls = list(M1_model, M1_phase, M2_model, M2_phase))
