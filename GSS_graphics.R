# Analyses

library(ggplot2)
library(dplyr)
library(repmis)
library(MASS)

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

# Regression attempts
attach(z)
y <- lm(vhappy ~ hrs1 + rinc + othinc)
summary(y)

