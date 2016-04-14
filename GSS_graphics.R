# Analyses

library(ggplot2)
library(dplyr)
library(plyr)
library(repmis)
library(MASS)
library(Hmisc)

# Distributional diagrams

satjob.freq <- table(z$satjob)

barplot(satjob.freq, 
        col=c("navyblue"),
        xlab = "Job satisfaction categories",
        legend = c("Job satisfaction"),
        ylim = c(0, 15000)
        )

sat.freq <- table(z$happy)

barplot(sat.freq, 
        col=c("navyblue"),
        xlab = "Overall life satisfaction categories",
        legend = c("Life satisfaction"),
        ylim = c(0, 20000)
)

# Histograms

hist(z$hrs1,
        col=c("navyblue"),
        border = "White",
        xlab = "Hours worked last week"
        )   

hist(z$hrs1[z$working_ft==1],
     col=c("navyblue"),
     border = "White",
     xlab = "Hours worked last week (full-time employees)"
)   

####################################
# Graph plots over years           #
####################################


# Prepare age x happiness
z$vhappy1 <- as.numeric(z$vhappy)
t <- z

# Age and year v. happiness: full sample
ggplot(t, aes(x=factor(age), y=vhappy1)) + stat_summary(fun.y="mean", geom="bar", fill="navyblue")

ggplot(t, aes(x=factor(year), y=vhappy1)) + 
  stat_summary(fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.2,0.45)) + 
  theme_bw()              
      
# Age v. happiness (conditional on gender and college education)

## Men
ggplot(t) + 
  stat_summary(data = t[t$sex == 1 & educat == 4,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") +
  expand_limits(y=c(0.25,0.6)) +
  theme_bw()

## Women
ggplot(t) + 
  stat_summary(data = t[t$sex == 2 & educat == 4,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  expand_limits(y=c(0.25,0.6)) +
  theme_bw()

## Combined

ggplot() + 
  stat_summary(data = t[t$sex == 1 & educat == 4,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") + 
  stat_summary(data = t[t$sex == 2 & educat == 4,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  expand_limits(y=c(0.2,0.6)) +
  theme_bw()

## Combined including full-time restriction

ggplot() + 
  stat_summary(data = t[t$sex == 1 & t$educat == 4 & t$working_ft == 1,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") + 
  stat_summary(data = t[t$sex == 2 & t$educat == 4 & t$working_ft == 1,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  expand_limits(y=c(0.2,0.6)) +
  theme_bw()

## Full-time vs. non-full time
ggplot() + 
  stat_summary(data = t[t$working_ft == 1,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Purple") + 
  stat_summary(data = t[t$working_pt == 1,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Darkgreen") + 
  expand_limits(y=c(0.2,0.6)) +
  theme_bw()

## Full-time vs. non-full time: Conditional on female
ggplot() + 
  stat_summary(data = t[t$working_ft == 1 & t$sex == 2,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Purple") + 
  stat_summary(data = t[t$working_pt == 1 & t$sex == 2,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Darkgreen") + 
  expand_limits(y=c(0.2,0.6)) +
  theme_bw()

## Happiness v. work-status: Conditional on gender

ggplot() + 
  stat_summary(data = t[t$sex == 2,], aes(x=factor(wrkstat), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  stat_summary(data = t[t$sex == 1,], aes(x=factor(wrkstat), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.2,0.4)) +
  theme_bw()

## Happiness v. work-status: Conditional on gender and family
t <- z
ggplot() + 
  stat_summary(data = t[t$sex == 2 & t$family == 1,], aes(x=factor(wrkstat), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  stat_summary(data = t[t$sex == 2 & t$family == 0,], aes(x=factor(wrkstat), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.2,0.5)) +
  theme_bw()


ggplot() + 
  stat_summary(data = t[t$sex == 1 & t$family == 1,], aes(x=factor(wrkstat), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  stat_summary(data = t[t$sex == 1 & t$family == 0,], aes(x=factor(wrkstat), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.2,0.5)) +
  theme_bw()

# Commands for printing a graph. !can be deleted!
png(filename="/Users/Unger/Desktop/graphs/male_educ.png")
dev.off()

# Collapse on means


# Regression attempts
attach(z)
y <- lm(vhappy ~ hrs1 + rinc + othinc)
summary(y)

