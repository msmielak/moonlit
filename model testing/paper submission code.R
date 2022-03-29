### This is reproducible code example for the paper
### "Biologically meaningful moonlight measures and their application in ecological research"
### Smielak, M.S (2022)

# load required libraries
library(ggplot2); theme_set(theme_minimal(base_size=10))
library(ggpubr)

### Loading the moonlit package from GitHub repository

#install and load devtools (if needed)
install.packages("devtools")
library(devtools)

#install moonlit library from github repo (if needed)
install_github("msmielak/moonlit")

#load the moonlit library
library(moonlit)



#####
##### Part 1 - testing model against moonlight illuminated measured by 3 independet loggers

#######
### UNE logger
#######

# Set time zone to account for daylight saving
Sys.setenv( tz="Australia/Brisbane" )

#read dataset
#dataset includes date, illumination value and solar altitude (degrees) - records with solar altitude above -11 degrees was removed
UNE <- read.csv("./Datasets/STQ.csv")

#reformat date to POSIX
UNE$date <- as.POSIXct(UNE$date)


# estimate moonlight intensity for each of the records using moonlit package

une_model <- calculateMoonlightIntensity(lat=-30.48, lon=151.64, date=UNE$date, e=0.21)

# merge model estimations into logger dataset

UNE$illuminationModel <- une_model$moonlightModel
UNE$fraction <- une_model$moonPhase


### plotting model and measurements

p1a <- ggplot(UNE, aes(x=date))+
  geom_jitter(aes(y=lx), alpha = 0.05)+
  labs(y="Illuminance (lx)", title = "Logger")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

p2a <- ggplot(UNE, aes(x=date))+
  geom_jitter(aes(y=illuminationModel), alpha = 0.05)+
  labs(x="Date", y="Relative moonlight intensity", title = "Model")+
  theme(axis.text=element_text(size=10),
              axis.title=element_text(size=10))

#arrange plots together
ggarrange(p1a, p2a, 
          nrow=2)


### plotting measured values against model and moon phase

p3a <-   ggplot(UNE, aes(y=lx, x=illuminationModel))+
  geom_jitter(alpha = 0.03)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Relative moonlight intensity", y="Illuminance (lx)", title = "Model")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10))


p4a <- ggplot(UNE, aes(y=lx, x=fraction))+
  geom_jitter(alpha = 0.03)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Fraction of moon illuminated", title = "Moon phase")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
          axis.text=element_text(size=10),
                axis.title=element_text(size=10))

ggarrange(p3a, p4a)

### testing models
M1_phase <- lm(UNE$fraction ~ poly(UNE$lx, 3))
M1_phase_residuals <- density(residuals(M1_phase))
plot(M1_phase_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")

M1_model <- lm(UNE$illuminationModel ~ poly(UNE$lx, 3))
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

p5a <- ggplot(errors1, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density")+
  theme(axis.title.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

#errors of second model
x <- M1_phase_residuals$x
y <- M1_phase_residuals$y

errors2 <- data.frame(x,y)
errors2$model <- "moon phase"

errors2[nrow(errors2) + 1,] = list(x=-1, y=0, model="moon phase")
errors2[nrow(errors2) + 1,] = list(x=1, y=0, model="moon phase")




p6a <- ggplot(errors2, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))


ggarrange(p5a, p6a)



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



# Read dataset for Sky Quality Meter
# it contains date, solar altitude and luminance converted from apparent magnitude according to the equation provided by the manufacturer

SQM <- read.csv("./Datasets/Gnosca.csv")

#convert date to POSIX - need to use a different tz as loggers do not observe daylight saving. Using Lagos instead.
SQM$date <- as.POSIXct(SQM$date, tz="Africa/Lagos")



# estimate moonlight intensity for each of the records using moonlit package

sqm_model <- calculateMoonlightIntensity(lat=46.23, lon=9.02, date=SQM$date, e=0.26)

# merge model estimations into logger dataset

SQM$illuminationModel <- sqm_model$moonlightModel
SQM$fraction <- sqm_model$moonPhase





### plotting model and measurements

p1b <- ggplot(SQM, aes(x=date))+
  geom_jitter(aes(y=lx), alpha = 0.03)+
  theme_minimal()+
  labs(y="Luminance (cd/m²)", title = "Logger")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

p2b <- ggplot(SQM, aes(x=date))+
  geom_jitter(aes(y=illuminationModel), alpha = 0.03)+
  theme_minimal()+
  labs(x="Date", y="Relative moonlight intensity", title = "Model")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10))

#arrange plots together
ggarrange(p1b, p2b, 
          nrow=2)


### plotting measured values against model and moon phase

p3b <-   ggplot(SQM, aes(y=lx, x=illuminationModel))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Relative moonlight intensity", y="Luminance (cd/m²)", title = "Model")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10))


p4b <- ggplot(SQM, aes(y=lx, x=fraction))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Fraction of moon illuminated", title = "Moon phase")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

ggarrange(p3b, p4b)

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

p5b <- ggplot(errors1, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density")+
  theme(axis.title.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

#errors of second model
x <- M2_phase_residuals$x
y <- M2_phase_residuals$y

errors2 <- data.frame(x,y)
errors2$model <- "moon phase"

errors2[nrow(errors2) + 1,] = list(x=-1, y=0, model="moon phase")
errors2[nrow(errors2) + 1,] = list(x=1, y=0, model="moon phase")




p6b <- ggplot(errors2, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))


ggarrange(p5b, p6b)



#merge them into one df
errors <- rbind(errors1, errors2)




#plot the two
ggplot(errors)+
  geom_line(aes(x=x, y=y, linetype=model))+
  labs(title="Residual distribution - custom logger", x="Residuals", y="Density")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8),  legend.background = element_rect(fill="white"))



#####
### Custom built logger
#####
### dataset includes date, voltage (represented as difference from 2.5V) and solar altitude in degrees
### data with solar elevation above -11 degree and when photoresisor is saturated (voltage=0) was removed

#changing system time zone to Brisbane to remove daylight saving
Sys.setenv( tz="Australia/Brisbane" )


# load logger data
customLogger <- read.csv("./Datasets/custom_logger_test.csv")

# convert date to POSIXct
customLogger$date <- as.POSIXct(customLogger$date)

# estimate moonlight intensity for each of the records using moonlit package

cl_model <- calculateMoonlightIntensity(lat=-30.07, lon=152.15, date=customLogger$date, e=0.21)

# merge model estimations into logger dataset

customLogger$illuminationModel <- cl_model$moonlightModel
customLogger$fraction <- cl_model$moonPhase

### plotting model and measurements

p1c <- ggplot(customLogger, aes(x=date))+
  geom_jitter(aes(y=voltage), alpha = 0.1)+
  theme_minimal()+
  labs(y="Voltage (V)", title = "Logger")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

p2c <- ggplot(customLogger, aes(x=date))+
  geom_jitter(aes(y=illuminationModel), alpha = 0.1)+
  theme_minimal()+
  labs(x="Date", y="Relative moonlight intensity", title = "Model")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10))

#arrange plots together
ggarrange(p1c, p2c, 
          nrow=2)


### plotting measured values against model and moon phase

p3c <-   ggplot(customLogger, aes(y=voltage, x=illuminationModel))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Relative moonlight intensity", y="Voltage", title = "Model")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10))


p4c <- ggplot(customLogger, aes(y=voltage, x=fraction))+
  geom_jitter(alpha = 0.02)+
  geom_smooth()+
  theme_minimal()+
  labs(x="Fraction of moon illuminated", title = "Moon phase")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

ggarrange(p3c, p4c)


### testing models
M3_phase <- lm(customLogger$fraction ~ poly(customLogger$voltage, 3))
M3_phase_residuals <- density(residuals(M3_phase))
plot(M3_phase_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")

M3_model <- lm(customLogger$illuminationModel ~ poly(customLogger$voltage, 3))
M3_model_residuals <- density(residuals(M3_model))
plot(M3_model_residuals, xlim = c(-1,1), xlab="Residuals", main = "Custom logger - moon phase")


### Compare error distributions

#errors of first model
x <- M3_model_residuals$x
y <- M3_model_residuals$y

errors1 <- data.frame(x,y)
errors1$model <- "model"

#add 2 data points at -1 and 1 for the logger (this is for data presentation only)
errors1[nrow(errors1) + 1,] = list(x=-1, y=0, model="model")
errors1[nrow(errors1) + 1,] = list(x=1, y=0, model="model")

p5c <- ggplot(errors1, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density")+
  theme(axis.title.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))

#errors of second model
x <- M3_phase_residuals$x
y <- M3_phase_residuals$y

errors2 <- data.frame(x,y)
errors2$model <- "moon phase"

p6c <- ggplot(errors2, aes(x=x, y=y))+
  geom_line()+
  theme_minimal()+
  labs(x="Residual distribution", y="Density")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))


ggarrange(p5c, p6c)


#merge them into one df
errors <- rbind(errors1, errors2)

#plot the two
ggplot(errors)+
  geom_line(aes(x=x, y=y, linetype=model))+
  labs(title="Residual distribution - custom logger", x="Residuals", y="Density")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8),  legend.background = element_rect(fill="white"))


##### Generate plots for publication

ggarrange(p1a, p1b, p1c, p2a, p2b, p2c)

ggarrange(p3a, p4a, p3b, p4b, p3c, p4c, p5a, p6a, p5b, p6b, p5c, p6c, nrow=2, ncol=6)

#####



#####################################
### Case study - Paddysland possum dataset
library(openair) 

#loading paddysland dataset
PL <- read.csv("./Datasets/Paddysland_BTP_test_data.csv")
PL$date <- as.POSIXct(PL$date)

#extract first and last record
min(PL$date)
max(PL$date)


#generate time series of nightly detections
PL_daily <- timeAverage(PL, start.date = "2014-12-15 12:00:00", period = "24 hours", statistic = "frequency")

#subset only date and number of detections
PL_daily <- PL_daily[c("date", "BTP")]

#replace NAs with 0
PL_daily[is.na(PL_daily)] <- 0

#generate day column
PL_daily$day <- as.Date(PL_daily$date)


#test plot
ggplot(PL_daily, aes(x=date, y=BTP))+
  geom_line()+
  geom_smooth()+
  theme_minimal()+
  labs(x="Date", y="Daily detection rate")

#generate a column for detrending (days from start)
PL_daily$detrend <- PL_daily$day - min(PL_daily$day)


#calculating mean illumination and moon phase values
PL_mean <- calculateMoonlightStatistics(lat=-30.07, lon=152.15, date=PL_daily$date, e=0.21, t="15 mins", timezone="Australia/Brisbane")

#merging into the daily detections dataset
PL_daily$meanMoonIllumination <- PL_mean$meanMoonlightIntensity
PL_daily$moonPhase <- PL_mean$meanMoonPhase

# test plots
ggplot(PL_daily, aes(x=moonPhase, y=BTP))+
  geom_jitter()+
  geom_smooth()

ggplot(PL_daily, aes(x=meanMoonIllumination, y=BTP))+
  geom_jitter()+
  geom_smooth()

#######################
### Model building and testing

library(glmmTMB)
library(MuMIn)
library(stargazer)
library(gtsummary)
library(texreg)
library(DHARMa)
library(tidyr)
library(sjPlot)
library(effects)
library(AICcmodavg)


#model for moon phase
m1 <- glmmTMB(data=PL_daily, family ="nbinom1", formula = BTP~moonPhase +(1|detrend))

summary(m1)
plot(m1)

r.squaredGLMM(m1)

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

r.squaredGLMM(m2)

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



############################################
### Testing for preference within each night
#############################################


# calculate moonlight intensity values:
#at detection
PL_illum <- calculateMoonlightIntensity(lat=-30.07, lon=152.15, date=PL$date, e=0.21)

#mean for the night
PL_nightlyMean <- calculateMoonlightStatistics(lat=-30.07, lon=152.15, date=PL$date, e=0.21, t="15 mins", timezone="Australia/Brisbane")

# merge into the dataset
PL$meanMoonIllumination <- PL_nightlyMean$meanMoonlightIntensity
PL$meanMoonFraction <- PL_nightlyMean$meanMoonPhase
PL$maxMoonFraction <- PL_nightlyMean$maxMoonPhase
PL$moonIllumination <- PL_illum$moonlightModel

# calculate "moonlight preference" value as a difference between illumination at a given point and mean for that night
PL$moonlightDifference <- PL$moonIllumination-PL$meanMoonIllumination

# transform moonlight preference to relative values by dividing by mean illumination value
PL$relativeMoonlightDifference <- PL$moonlightDifference/PL$meanMoonIllumination

#Subset data to remove nights with negligible moonlight - cannot show preference

#PL1 <- subset(PL, maxMoonFraction >0.5)
PL1 <- subset(PL, meanMoonIllumination >0.02)


#plot data
ggplot(data=PL1, aes(x=meanMoonIllumination, y=moonlightDifference))+
  geom_point()+
  geom_smooth()


# histogram
hist(PL1$moonlightDifference)

# all records
ggplot(data=PL)+
  geom_density(aes(x=moonlightDifference))

# records with moonlight (>0.02)
ggplot(data=PL1)+
  geom_density(aes(x=moonlightDifference))

lm <- lm(data=PL1, moonlightDifference~meanMoonIllumination)

summary(lm)
plot(allEffects(lm))

### Generalised Least Squares model using gls() function from nlme

library(nlme)


m.lm <- gls(data=PL1, moonlightDifference~meanMoonIllumination)

summary(m.lm)
plot(m.lm)
plot(allEffects(m.lm))

# Comparing difference variance functions

# Fixed variance
vf1Fixed <- varFixed(~meanMoonIllumination)
M.gls1<-gls( moonlightDifference~meanMoonIllumination,
             weights=vf1Fixed,data=PL1)

summary(M.gls1)
plot(M.gls1)
plot(allEffects(M.gls1))

# varConstPower
vf2 <- varConstPower(form= ~meanMoonIllumination)
M.gls2<-gls( moonlightDifference~meanMoonIllumination,
             weights=vf2,data=PL1)

summary(M.gls2)
plot(M.gls2)
plot(allEffects(M.gls2))

tab_model(M.gls2)

# varExp
vf3 <- varExp(form =~ meanMoonIllumination)
M.gls3<-gls( moonlightDifference~meanMoonIllumination,
             weights=vf3 ,data=PL1)

summary(M.gls3)
plot(M.gls3)
plot(allEffects(M.gls3))

tab_model(M.gls3)

# Comparing variance functions


AICtab(m.lm, M.gls1, M.gls2, M.gls3)
tab_model(M.gls1, M.gls2, M.gls3)

### M.gls2 appears to be the best, proceeding using this one


plot(effect(mod=M.gls2, term = "meanMoonIllumination", partial.residuals=TRUE),
     main=NULL,
     xlab="Mean moon illumination",
     ylab="Moonlight preference index",
     partial.residual=list(pch="+",col="grey"),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)


tab_model(M.gls2)





#################################
### dividing data into 5 bins
#####

PL1 <- subset (PL, meanMoonIllumination >0 & meanMoonIllumination <0.1)
ggplot(PL1)+
  geom_density(aes(x=moonlightDifference))

PL2 <- subset (PL, meanMoonIllumination >=0.1 & meanMoonIllumination <0.2)
ggplot(PL2)+
  geom_density(aes(x=moonlightDifference))


PL3 <- subset (PL, meanMoonIllumination >=0.2 & meanMoonIllumination <0.3)
ggplot(PL3)+
  geom_density(aes(x=moonlightDifference))

PL4 <- subset (PL, meanMoonIllumination >=0.3 & meanMoonIllumination <0.4)
ggplot(PL4)+
  geom_density(aes(x=moonlightDifference))

PL5 <- subset (PL, meanMoonIllumination >=0.4)
ggplot(PL5)+
  geom_density(aes(x=moonlightDifference))


### t-tests to check if values are different from 0 for each quantile

t1 <- t.test(PL1$moonlightDifference, mu = 0, alternative = "greater")
t2 <- t.test(PL2$moonlightDifference, mu = 0, alternative = "greater")
t3 <- t.test(PL3$moonlightDifference, mu = 0, alternative = "greater")
t4 <- t.test(PL4$moonlightDifference, mu = 0, alternative = "greater")
t5 <- t.test(PL5$moonlightDifference, mu = 0, alternative = "greater")



labels <- data.frame(
  bins=c(1.6,2.6,3.6,4.6,5.6),
  moonlightDifference=c(-0.3, -0.3, -0.3, -0.3, -0.3),
  mean=c(t1$estimate, t2$estimate, t3$estimate, t4$estimate, t5$estimate),
  p.value=c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value)
)

labels$mean <-  round(labels$mean, digits=3)
labels$mean <- paste("mean  =  ", labels$mean)

labels$p.value <-  round(labels$p.value, digits=3)
labels$p.value <- paste("p = ", labels$p.value)

labels$label <- paste(labels$mean, ", ", labels$p.value)


# Plotting

library(ggridges)

#PL$bins <- cut(PL$maxMoonFraction, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"))

PL$bins <- cut(PL$meanMoonIllumination, breaks=c(-0.01, 0.1, 0.2, 0.3, 0.4, 1), labels=c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", ">0.4"))


#PL$bins2 <- cut(PL$maxMoonFraction, breaks=c(0, 0.25, 0.5, 0.75, 1), labels=c( "0-25%", "25-50%", "50-75%", "75-100%"))

# Boxplots
ggplot(PL, aes(x=moonlightDifference, y=bins))+
  geom_boxplot()

# Ridges
ggplot(PL, aes(x=moonlightDifference, y=bins))+
  geom_density_ridges(scale=2, quantile_lines = TRUE, quantiles=2, alpha=0.5)+
  labs(x="Moonlight preference", y="Mean moonlight intensity")

#with p-values
ggplot(PL, aes(x=moonlightDifference, y=bins))+
  geom_density_ridges(scale=2, quantile_lines = TRUE, quantiles=2, alpha=0.5)+
  labs(x="Moonlight preference", y="Mean moonlight intensity")+
  geom_text(data = labels, aes(x=-0.25, label = label))

