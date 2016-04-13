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

# Age and year - full sample
ggplot(t, aes(x=factor(age), y=vhappy1)) + stat_summary(fun.y="mean", geom="bar", fill="navyblue")

bp <- ggplot(t, aes(x=factor(age), y=vhappy1))
bp + stat_summary(fun.y="mean", geom="line", fill="navyblue") + expand_limits(y=c(0.4))               
bp + stat_summary(fun.y="mean", geom="point", col="Navyblue") + expand_limits(y=c(0.25,0.4))               

ggplot(t, aes(x=factor(year), y=vhappy1)) + 
  stat_summary(fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.25,0.4))               

# Age v. happiness (men & women seperate)
female_educ <- subset(t, sex==2 & educat == 4)
ggplot(female_educ, aes(x=factor(age), y=vhappy1)) + 
  stat_summary(fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.25,0.6))               


male_educ <- subset(t, sex==1 & educat == 4)
ggplot(male_educ, aes(x=factor(age), y=vhappy1)) + 
  stat_summary(fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(0.25,0.6))        


##

ggplot(t) + 
  stat_summary(data = t[t$sex == 1,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Navyblue") + 
  stat_summary(data = t[t$sex == 2,], aes(x=factor(age), y=vhappy1), 
               fun.y="mean", geom="point", col="Red") + 
  expand_limits(y=c(0.25,0.4)) +
  theme_bw()

##

aggdata <-aggregate(t, by=list(t$sex, t$age), 
                            FUN=mean, na.rm=TRUE)

ggplot(aggdata, aes(x=factor(age)), y=vhappy1) + geom_line()

png(filename="/Users/Unger/Desktop/graphs/male_educ.png")
plot(fit)
dev.off()

# Collapse on means


# Regression attempts
attach(z)
y <- lm(vhappy ~ hrs1 + rinc + othinc)
summary(y)

