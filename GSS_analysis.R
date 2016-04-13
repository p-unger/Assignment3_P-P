##################################################
# ANALYSIS OF GSS DATA #
##################################################

# set your working directory.
setwd("~/R_data/Assignment3_P-P") # Staender
setwd("~/Documents/CDA/collaborative_projects/Assignment3_P-P") # Unger


# Load packages
library(reshape)
library(gmodels)
library(car)
library(plyr)
library(Hmisc)
library(ggplot2)

# Load GSS-data file
# !Should be sourced!
load( "GSS.fin.rda" )

z <- GSS.fin.df
rm( GSS.fin.df )
gc()

##SUM STATS
attach(z)

describe(z$year[sex==2 & educat==4]) 
describe(z$vhappy[sex==2 & educat==4]) 
describe(z$happy[sex==2 & educat==4]) 
describe(z$career[sex==2 & educat==4]) 
describe(z$married[sex==2 & educat==4]) 
describe(z$family[sex==2 & educat==4]) 
describe(z$careermarried[sex==2 & educat==4]) 
describe(z$careerfamily[sex==2 & educat==4])
describe(z$age[sex==2 & educat==4])

table(race[sex==2 & educat==4])
table(cohort[sex==2 & educat==4])


table(z$career, z$sex)
table(z$careerkid, z$sex)
table(z$careerfamily, z$sex)
table(z$careermarried, z$sex)
table(z$keepinghousekid, z$sex)


# Scatterplot for restricted sample
#car::scatterplotMatrix()

####################
# Plots to compare male and female overall happiness #
####################

ggplot() + 
  stat_summary(data = z[z$sex == 1,], aes(x=factor(career), y=vhappy), 
               fun.y="mean", geom="point", col="Navyblue") + 
  stat_summary(data = z[z$sex == 2,], aes(x=factor(career), y=vhappy), 
               fun.y="mean", geom="point", col="Red") + 
  expand_limits(y=c(0.25,0.4)) +
  theme_bw()

z$meanhap <- NA
z$meanhap[z$family==0 & z$career==0] <- "No career, no family"
z$meanhap[z$family==0 & z$career==1] <- "Career, no family"
z$meanhap[z$family==1 & z$career==0] <- "No career, family"
z$meanhap[z$family==1 & z$career==1] <- "Career, family"

table(z$meanhap)
describe(z$meanhap)

ggplot() + 
  stat_summary(data = z[z$sex == 1,], aes(x=factor(meanhap), y=vhappy), 
               fun.y="mean", geom="point", col="Navyblue") +
  stat_summary(data = z[z$sex == 2,], aes(x=factor(meanhap), y=vhappy), 
             fun.y="mean", geom="point", col="Red")
  theme_bw()
  
  
  
##############
# Regression # 
##############
  
#controls: age agesq as.factor(year) as.factor(race) as.factor(bdec)
# changing variables into numeric
z$working_ft <- as.numeric(z$working_ft)
z$working_pt <- as.numeric(z$working_pt)

#### M1 #### FOR WOMEN: Career-Married interaction effect on being very happy (Subset for college educated women)

  
M1a <- lm(vhappy ~ career*married + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), 
         data = subset(z, sex==2 & educat == 4))
summary(M1a)

M1b <- lm(vhappy ~ career*married + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), 
         data = subset(z, sex==2 & educat == 4 & working_ft == 1))
summary(M1b)

M1c <- lm(vhappy ~ career*married + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), 
          data = subset(z, sex==2 & educat == 4 & working_pt == 1))
summary(M1c)

table(z$working_ft, z$career)
table(z$working_pt, z$career)

require("interplot")

interM1a <- interplot(M1a, var1 = "married", var2 = "career") +
  ggtitle("Working") +
  xlab("career") + 
  ylab("effect of marriage on life satisfaction")

interM1b <- interplot(M1b, var1 = "married", var2 = "career") +
  ggtitle("Working Full-Time") +
  xlab("career")

interM1c <- interplot(M1c, var1 = "married", var2 = "career") +
  ggtitle("Working Part-Time") +
  xlab("career")

require("gridExtra")

grid.arrange(interM1a, interM1b, interM1c, ncol = 3) +
  ggtitle("The interaction effect of having a career on life satisfaction (College educated women)")



###################
### I have only worked until here...
###################





#### M2 #### FOR MEN: Career-Married interaction effect on being very happy (Subset for college educated men)
M2 <- lm(vhappy ~ career*married + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), 
          data = subset(z, sex==1 & educat == 4))
summary(M1b)

interM1b <- interplot(M1b, var1 = "married", var2 = "career") +
  ggtitle("This is a title") +
  xlab("career") + 
  ylab("marriage and satisfaction")

# M1 subset for college educated men
M2 <- lm(happy ~ career*married + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), 
         data = subset(z, sex==2 & educat == 4))
summary(M2)

interM2 <- interplot(M2, var1 = "married", var2 = "career") +
  ggtitle("This is a title") +
  xlab("career") + 
  ylab("marriage and satisfaction")

require("gridExtra")

grid.arrange(interM1, interM1b, interM2, nrow = 2)

n <- subset(z, sex==2 & educat == 4 & age>=40)

M3 <- lm(vhappy ~ career + married + careermarried + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = n)

M4 <- lm(vhappy ~ career + family + careerfamily + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = m)
M5 <- lm(happy ~ career + family + careerfamily + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = m)
M6 <- lm(happy ~ career + family + careerfamily + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = n)

load("~/R_data/Assignment3_P-P/CPS.rda")
write.csv(CPS.df, "CPS.csv")

##########
##Table 2 - focus on married women or women with family - allow to better control for income - use spousal income
##########

t <- subset(z, sex==2 & educat == 4 & married==1)

#define categories for husband's income
table(z$othinc)

z$othinccat <- trunc(z$othinc/5000)
z$othinccat[is.na(z$othinc)] <- -1

table(z$othinccat)

M7 <- lm(vhappy ~ career + age + agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = t)
M8 <- lm(vhappy ~ career + as.factor(othinccat) + age agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = t)
M9 <- lm(vhappy ~ career + keepinghouse + as.factor(othinccat) + age agesq + as.factor(year) + as.factor(race) + as.factor(bdec), data = t)

xi:reg vhappy  career keepinghouse $controls i.othinccat if sex==2 & educat==4  & married==1
outreg career keepinghouse using table2,bdec(3) br se append

reg vhappy  career $controls if sex==2 & educat==4 & family==1
outreg career using table2,bdec(3) br se append
xi:reg vhappy  career $controls i.othinccat if sex==2 & educat==4 & family==1
outreg career using table2,bdec(3) br se append
xi:reg vhappy  career keepinghouse $controls i.othinccat if sex==2 & educat==4  & family==1
outreg career keepinghouse using table2,bdec(3) br se append