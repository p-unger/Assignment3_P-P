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

t <- z

# Age and year v. happiness: full sample
ggplot(t, aes(x=factor(age), y=vhappy1)) + stat_summary(fun.y="mean", geom="bar", fill="navyblue")

ggplot(t, aes(x=factor(year), y=vhappy)) + 
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
  stat_summary(data = t[t$working_ft == 1,], aes(x=factor(age), y=vhappy), 
               fun.y="mean", geom="point", col="Purple") + 
  stat_summary(data = t[t$working_pt == 1,], aes(x=factor(age), y=vhappy), 
               fun.y="mean", geom="point", col="Darkgreen") + 
  expand_limits(y=c(20,60)) +
  theme_bw()

## Full-time vs. non-full time: Conditional on female
ggplot() + 
  stat_summary(data = t[t$working_ft == 1 & t$sex == 2,], aes(x=factor(age), y=vhappy), 
               fun.y="mean", geom="point", col="Purple") + 
  stat_summary(data = t[t$working_pt == 1 & t$sex == 2,], aes(x=factor(age), y=vhappy), 
               fun.y="mean", geom="point", col="Darkgreen") + 
  expand_limits(y=c(20, 60)) +
  theme_bw()

## Happiness v. work-status: Conditional on gender

ggplot() + 
  stat_summary(data = t[t$sex == 2,], aes(x=factor(wrkstat), y=vhappy), 
               fun.y="mean", geom="point", col="Red") + 
  stat_summary(data = t[t$sex == 1,], aes(x=factor(wrkstat), y=vhappy), 
               fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(20,40)) +
  theme_bw()

## Happiness v. work-status: Conditional on gender and family
t <- z
ggplot() + 
  stat_summary(data = t[t$sex == 2 & t$family == 1,], aes(x=factor(wrkstat), y=vhappy), 
               fun.y="mean", geom="point", col="Red") + 
  stat_summary(data = t[t$sex == 2 & t$family == 0,], aes(x=factor(wrkstat), y=vhappy), 
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

# Spouse's work conditions
table(t$spwrksta)
t$spouse_work <- NULL
t$spouse_work[t$spwrksta==1] <- 1
t$spouse_work[t$spwrksta==2] <- 2
t$spouse_work[t$spwrksta==7] <- 7

ggplot() + 
  stat_summary(data = t[t$ujobsat1 == 0,], aes(x=factor(sex), y=vhappy), 
               fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(20,60)) +
  theme_bw()

t$hardwork <- NULL
t$hardwork[t$hrs1 > 50] <- 1
t$hardwork[t$hrs1 < 50] <- 0
t$hardwork <- as.numeric(t$hardwork)
table(t$hardwork)

describe(t$hrs2)

ggplot() + 
  stat_summary(data = t[t$sex == 1 &  t$hardwork == 1 & t$working_ft == 1 & t$kid == 1,], aes(x=factor(spouse_work), y=vhappy), 
               fun.y="mean", geom="point", col="Red") + 
  stat_summary(data = t[t$sex == 1  & t$hardwork == 1 & t$working_ft == 1 & t$kid == 0,], aes(x=factor(spouse_work), y=vhappy), 
               fun.y="mean", geom="point", col="Navyblue") + 
  expand_limits(y=c(20,60)) +
  theme_bw()

# Commands for printing a graph. !can be deleted!
png(filename="/Users/Unger/Desktop/graphs/male_educ.png")
dev.off()

# Collapse on means

table(t$career[year == 2010])


# Regression attempts
attach(z)
y <- lm(vhappy ~ hrs1 + rinc + othinc)
summary(y)

table(z$happy)
t$vhappy <- z$happy==3
t$vhappy <- as.numeric(z$vhappy)

t <- z
t$ujobsat1 <- t$satjob == 1 | t$satjob == 2 | t$satjob == 3
t$ujobsat2 <- t$satjob == 1 | t$satjob == 2 

t$ujobsat1 <- as.numeric(t$ujobsat1)
table(t$ujobsat1)

t$d_sex <- t$sex == 1
t$d_sex <- as.numeric(t$d_sex)
t$ujobsat1 <- as.numeric(t$ujobsat1)

R1 <- lm(ujobsat1 ~ d_sex + age + agesq + hrs1 + def_lrinc + def_lothinc + family + 
           as.factor(year) + as.factor(occ80),
          data = subset(t, vhappy == 1 & working_ft == 1))
summary(R1)

table(t$ujobsat1)
table(t$vhappy)

R3 <- lm(vhappy ~ d_sex + family + hrs1 +  def_linc + def_othinc + as.factor(occ80), 
         data = subset(t, ujobsat1 == 0 & year >= 1977 & year < 2011))
summary(R3)

R2 <- lm(vhappy ~ d_sex,
         data = subset(t, working_ft == 1 & educat == 4 ))
summary(R2)


# Potential graph for showing correlation between job and overall happiness
t$jobsat1 <- as.numeric(t$satjob == 1)
t$jobsat2 <- as.numeric(t$satjob == 2)
t$jobsat3 <- as.numeric(t$satjob == 3)
t$jobsat4 <- as.numeric(t$satjob == 4)

l <- lm(vhappy ~ jobsat2 + jobsat3 + jobsat4, data = t)
summary(l)

# Base regression
R1 <- lm(ujobsat1 ~ career*married + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), 
         data = subset(z, sex==2 & educat == 4))
summary(M1a)

