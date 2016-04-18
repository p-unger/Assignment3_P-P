############################################
# Final dataset: Combine and transform     #
############################################

# set your working directory.
#setwd("~/R_data/Assignment3_P-P") # Staender
# setwd("~/Documents/CDA/collaborative_projects/Assignment3_P-P") # Unger


# Load packages
library(reshape)
library(gmodels)
library(car)
library(plyr)
library(Hmisc)

# Load GSS-data file
# load( "data/GSS.CS.rda" )

# copy to a different object
z <- GSS.CS.df

# remove the original from RAM
rm( GSS.CS.df )

# clear up memory
gc()

##########################
# Generate new variables #
##########################

# fix z as reference dataset
attach(z)

# generate work status dummies
z$working_ft <- wrkstat == 1
z$working_pt <- wrkstat == 2
z$working <- wrkstat == 1 | wrkstat == 2
z$keepinghouse <- wrkstat == 7


# generate married dummies
z$married <- marital==1
z$nevermarried <- marital==5

# generate no-kids dummy
z$nokid <- childs==0

# generate other variables
z$agesq <- age*age
z$white <- race==1
z$black <- race==2

# gen rinc77
z$rinc77 <- NA
z$rinc77[z$rincom77==1] <- 500
z$rinc77[z$rincom77==2] <- 2000
z$rinc77[z$rincom77==3] <- 3500
z$rinc77[z$rincom77==4] <- 4500
z$rinc77[z$rincom77==5] <- 5500
z$rinc77[z$rincom77==6] <- 6500
z$rinc77[z$rincom77==7] <- 7500
z$rinc77[z$rincom77==8] <- 9000
z$rinc77[z$rincom77==9] <- 11250
z$rinc77[z$rincom77==10] <- 13750
z$rinc77[z$rincom77==11] <- 16250
z$rinc77[z$rincom77==12] <- 18750
z$rinc77[z$rincom77==13] <- 21500
z$rinc77[z$rincom77==14] <- 23750
z$rinc77[z$rincom77==15] <- 37500
z$rinc77[z$rincom77==16] <- 70000

# gen rinc82
z$rinc82 <- NA
z$rinc82[z$rincom82==1] <- 500
z$rinc82[z$rincom82==2] <- 2000
z$rinc82[z$rincom82==3] <- 3500
z$rinc82[z$rincom82==4] <- 4500
z$rinc82[z$rincom82==5] <- 5500
z$rinc82[z$rincom82==6] <- 6500
z$rinc82[z$rincom82==7] <- 7500
z$rinc82[z$rincom82==8] <- 9000
z$rinc82[z$rincom82==9] <- 11250
z$rinc82[z$rincom82==10] <- 13750
z$rinc82[z$rincom82==11] <- 16250
z$rinc82[z$rincom82==12] <- 18750
z$rinc82[z$rincom82==13] <- 21500
z$rinc82[z$rincom82==14] <- 23750
z$rinc82[z$rincom82==15] <- 30000
z$rinc82[z$rincom82==16] <- 42500
z$rinc82[z$rincom82==17] <- 70000

# gen rinc86
z$rinc86 <- NA
z$rinc86[z$rincom86==1] <- 500
z$rinc86[z$rincom86==2] <- 2000
z$rinc86[z$rincom86==3] <- 3500
z$rinc86[z$rincom86==4] <- 4500
z$rinc86[z$rincom86==5] <- 5500
z$rinc86[z$rincom86==6] <- 6500
z$rinc86[z$rincom86==7] <- 7500
z$rinc86[z$rincom86==8] <- 9000
z$rinc86[z$rincom86==9] <- 11250
z$rinc86[z$rincom86==10] <- 13750
z$rinc86[z$rincom86==11] <- 16250
z$rinc86[z$rincom86==12] <- 18750
z$rinc86[z$rincom86==13] <- 21500
z$rinc86[z$rincom86==14] <- 23750
z$rinc86[z$rincom86==15] <- 27500
z$rinc86[z$rincom86==16] <- 32500
z$rinc86[z$rincom86==17] <- 37500
z$rinc86[z$rincom86==18] <- 45000
z$rinc86[z$rincom86==19] <- 55000
z$rinc86[z$rincom86==20] <- 70000

# gen rinc91
z$rinc91 <- NA
z$rinc91[z$rincom91==1] <- 500
z$rinc91[z$rincom91==2] <- 2000
z$rinc91[z$rincom91==3] <- 3500
z$rinc91[z$rincom91==4] <- 4500
z$rinc91[z$rincom91==5] <- 5500
z$rinc91[z$rincom91==6] <- 6500
z$rinc91[z$rincom91==7] <- 7500
z$rinc91[z$rincom91==8] <- 9000
z$rinc91[z$rincom91==9] <- 11250
z$rinc91[z$rincom91==10] <- 13750
z$rinc91[z$rincom91==11] <- 16250
z$rinc91[z$rincom91==12] <- 18750
z$rinc91[z$rincom91==13] <- 21500
z$rinc91[z$rincom91==14] <- 23750
z$rinc91[z$rincom91==15] <- 27500
z$rinc91[z$rincom91==16] <- 32500
z$rinc91[z$rincom91==17] <- 37500
z$rinc91[z$rincom91==18] <- 45000
z$rinc91[z$rincom91==19] <- 55000
z$rinc91[z$rincom91==20] <- 67500
z$rinc91[z$rincom91==21] <- 90000

# gen rinc98
z$rinc98 <- NA
z$rinc98[z$rincom98==1] <- 500
z$rinc98[z$rincom98==2] <- 2000
z$rinc98[z$rincom98==3] <- 3500
z$rinc98[z$rincom98==4] <- 4500
z$rinc98[z$rincom98==5] <- 5500
z$rinc98[z$rincom98==6] <- 6500
z$rinc98[z$rincom98==7] <- 7500
z$rinc98[z$rincom98==8] <- 9000
z$rinc98[z$rincom98==9] <- 11250
z$rinc98[z$rincom98==10] <- 13750
z$rinc98[z$rincom98==11] <- 16250
z$rinc98[z$rincom98==12] <- 18750
z$rinc98[z$rincom98==13] <- 21500
z$rinc98[z$rincom98==14] <- 23750
z$rinc98[z$rincom98==15] <- 27500
z$rinc98[z$rincom98==16] <- 32500
z$rinc98[z$rincom98==17] <- 37500
z$rinc98[z$rincom98==18] <- 45000
z$rinc98[z$rincom98==19] <- 55000
z$rinc98[z$rincom98==20] <- 67500
z$rinc98[z$rincom98==21] <- 82500
z$rinc98[z$rincom98==22] <- 100000
z$rinc98[z$rincom98==23] <- 120000

# gen rinc06
z$rinc06 <- NA
z$rinc06[z$rincom06==1] <- 500
z$rinc06[z$rincom06==2] <- 2000
z$rinc06[z$rincom06==3] <- 3500
z$rinc06[z$rincom06==4] <- 4500
z$rinc06[z$rincom06==5] <- 5500
z$rinc06[z$rincom06==6] <- 6500
z$rinc06[z$rincom06==7] <- 7500
z$rinc06[z$rincom06==8] <- 9000
z$rinc06[z$rincom06==9] <- 11250
z$rinc06[z$rincom06==10] <- 13750
z$rinc06[z$rincom06==11] <- 16250
z$rinc06[z$rincom06==12] <- 18750
z$rinc06[z$rincom06==13] <- 21500
z$rinc06[z$rincom06==14] <- 23750
z$rinc06[z$rincom06==15] <- 27500
z$rinc06[z$rincom06==16] <- 32500
z$rinc06[z$rincom06==17] <- 37500
z$rinc06[z$rincom06==18] <- 45000
z$rinc06[z$rincom06==19] <- 55000
z$rinc06[z$rincom06==20] <- 67500
z$rinc06[z$rincom06==21] <- 82500
z$rinc06[z$rincom06==22] <- 100000
z$rinc06[z$rincom06==23] <- 120000
z$rinc06[z$rincom06==24] <- 140000
z$rinc06[z$rincom06==25] <- 160000

## FAMILY INCOME -family income questioned is modelled same way as r's income question

# gen inc77
z$inc77 <- NA
z$inc77[z$income77==1] <- 500
z$inc77[z$income77==2] <- 2000
z$inc77[z$income77==3] <- 3500
z$inc77[z$income77==4] <- 4500
z$inc77[z$income77==5] <- 5500
z$inc77[z$income77==6] <- 6500
z$inc77[z$income77==7] <- 7500
z$inc77[z$income77==8] <- 9000
z$inc77[z$income77==9] <- 11250
z$inc77[z$income77==10] <- 13750
z$inc77[z$income77==11] <- 16250
z$inc77[z$income77==12] <- 18750
z$inc77[z$income77==13] <- 21500
z$inc77[z$income77==14] <- 23750
z$inc77[z$income77==15] <- 37500
z$inc77[z$income77==16] <- 70000

## gen inc82
z$inc82 <- NA
z$inc82[z$income82==1] <- 500
z$inc82[z$income82==2] <- 2000
z$inc82[z$income82==3] <- 3500
z$inc82[z$income82==4] <- 4500
z$inc82[z$income82==5] <- 5500
z$inc82[z$income82==6] <- 6500
z$inc82[z$income82==7] <- 7500
z$inc82[z$income82==8] <- 9000
z$inc82[z$income82==9] <- 11250
z$inc82[z$income82==10] <- 13750
z$inc82[z$income82==11] <- 16250
z$inc82[z$income82==12] <- 18750
z$inc82[z$income82==13] <- 21500
z$inc82[z$income82==14] <- 23750
z$inc82[z$income82==15] <- 30000
z$inc82[z$income82==16] <- 42500
z$inc82[z$income82==17] <- 70000

# gen inc86
z$inc86 <- NA
z$inc86[z$income86==1] <- 500
z$inc86[z$income86==2] <- 2000
z$inc86[z$income86==3] <- 3500
z$inc86[z$income86==4] <- 4500
z$inc86[z$income86==5] <- 5500
z$inc86[z$income86==6] <- 6500
z$inc86[z$income86==7] <- 7500
z$inc86[z$income86==8] <- 9000
z$inc86[z$income86==9] <- 11250
z$inc86[z$income86==10] <- 13750
z$inc86[z$income86==11] <- 16250
z$inc86[z$income86==12] <- 18750
z$inc86[z$income86==13] <- 21500
z$inc86[z$income86==14] <- 23750
z$inc86[z$income86==15] <- 27500
z$inc86[z$income86==16] <- 32500
z$inc86[z$income86==17] <- 37500
z$inc86[z$income86==18] <- 45000
z$inc86[z$income86==19] <- 55000
z$inc86[z$income86==20] <- 70000

# gen inc91
z$inc91 <- NA
z$inc91[z$income91==1] <- 500
z$inc91[z$income91==2] <- 2000
z$inc91[z$income91==3] <- 3500
z$inc91[z$income91==4] <- 4500
z$inc91[z$income91==5] <- 5500
z$inc91[z$income91==6] <- 6500
z$inc91[z$income91==7] <- 7500
z$inc91[z$income91==8] <- 9000
z$inc91[z$income91==9] <- 11250
z$inc91[z$income91==10] <- 13750
z$inc91[z$income91==11] <- 16250
z$inc91[z$income91==12] <- 18750
z$inc91[z$income91==13] <- 21500
z$inc91[z$income91==14] <- 23750
z$inc91[z$income91==15] <- 27500
z$inc91[z$income91==16] <- 32500
z$inc91[z$income91==17] <- 37500
z$inc91[z$income91==18] <- 45000
z$inc91[z$income91==19] <- 55000
z$inc91[z$income91==20] <- 67500
z$inc91[z$income91==21] <- 90000

# gen inc98
z$inc98 <- NA
z$inc98[z$income98==1] <- 500
z$inc98[z$income98==2] <- 2000
z$inc98[z$income98==3] <- 3500
z$inc98[z$income98==4] <- 4500
z$inc98[z$income98==5] <- 5500
z$inc98[z$income98==6] <- 6500
z$inc98[z$income98==7] <- 7500
z$inc98[z$income98==8] <- 9000
z$inc98[z$income98==9] <- 11250
z$inc98[z$income98==10] <- 13750
z$inc98[z$income98==11] <- 16250
z$inc98[z$income98==12] <- 18750
z$inc98[z$income98==13] <- 21500
z$inc98[z$income98==14] <- 23750
z$inc98[z$income98==15] <- 27500
z$inc98[z$income98==16] <- 32500
z$inc98[z$income98==17] <- 37500
z$inc98[z$income98==18] <- 45000
z$inc98[z$income98==19] <- 55000
z$inc98[z$income98==20] <- 67500
z$inc98[z$income98==21] <- 82500
z$inc98[z$income98==22] <- 100000
z$inc98[z$income98==23] <- 120000

# gen inc06
z$inc06 <- NA
z$inc06[z$income06==1] <- 500
z$inc06[z$income06==2] <- 2000
z$inc06[z$income06==3] <- 3500
z$inc06[z$income06==4] <- 4500
z$inc06[z$income06==5] <- 5500
z$inc06[z$income06==6] <- 6500
z$inc06[z$income06==7] <- 7500
z$inc06[z$income06==8] <- 9000
z$inc06[z$income06==9] <- 11250
z$inc06[z$income06==10] <- 13750
z$inc06[z$income06==11] <- 16250
z$inc06[z$income06==12] <- 18750
z$inc06[z$income06==13] <- 21500
z$inc06[z$income06==14] <- 23750
z$inc06[z$income06==15] <- 27500
z$inc06[z$income06==16] <- 32500
z$inc06[z$income06==17] <- 37500
z$inc06[z$income06==18] <- 45000
z$inc06[z$income06==19] <- 55000
z$inc06[z$income06==20] <- 67500
z$inc06[z$income06==21] <- 82500
z$inc06[z$income06==22] <- 100000
z$inc06[z$income06==23] <- 120000
z$inc06[z$income06==24] <- 140000
z$inc06[z$income06==25] <- 160000

# Create single income variables

z <- transform(z, rinc = rowMeans(z[, c("rinc77","rinc82", "rinc86", "rinc91", "rinc98", "rinc06")], na.rm = TRUE))
z <- transform(z, inc = rowMeans(z[, c("inc77","inc82", "inc86", "inc91", "inc98", "inc06")], na.rm = TRUE))

# Merge PCE and deflator
z <- merge(z, PCE, by = "year")
rm(PCE)

# Create otherincome variable
z$othinc <- z$inc - z$rinc
z$othinc[z$othinc<0] <- 0

# deflate income variables with the PCE-deflator
z$def_rinc <- z$rinc / z$PCEPI * 100
z$def_inc <- z$inc / z$PCEPI * 100
z$def_othinc <- z$othinc / z$PCEPI * 100

# satisfaction questions
z$vhappy <- z$happy==1
z$vhappy <- as.numeric(z$vhappy)

# 4 education categories

z$educat <- NA
z$educat[z$educ<12] <- 1
z$educat[z$educ==12] <- 2
z$educat[z$educ>12 & z$educ<16] <- 3
z$educat[z$educ>=16] <- 4

###################
# Restrict Sample #
###################

# Keep only people that are either working, or keeping house
z <- subset(z, z$working==1 | z$keepinghouse == 1)

# Keep if age>=25 & age<=54
z <- subset(z, z$age>=25 & z$age <= 54)

# One of these are redundant !!!
z$def_linc <- log(z$def_inc)
z$def_lrinc <- log(z$def_rinc)
z$def_lothinc <- log(z$def_othinc)

z$linc <- log(z$def_inc)
z$lrinc <- log(z$def_rinc)
z$lothinc <- log(z$def_othinc)

# Cohorts
z$by <- z$year-z$age

# drop cohort
z$cohort <- NULL

z$cohort <- NA
z$cohort[z$by>=1898 & z$by<=1923] <- 2
z$cohort[z$by>=1924 & z$by<=1943] <- 3
z$cohort[z$by>=1944 & z$by<=1957] <- 4
z$cohort[z$by>=1958 & z$by<=1973] <- 5
z$cohort[z$by>=1974 & z$by<=1991] <- 6

z$cohortb <- NA
z$cohortb[z$cohort<=3] <- 3
z$cohortb[z$cohort==4] <- 4
z$cohortb[z$cohort>=5] <- 5

##################################################
# merge men's earnings pctile from the March CPS #
##################################################

z$age_g <- trunc(z$age*2/10)
table(z$age_g)

# merge p25 and p50 from Bertrand data frame (must be open)

vars <- c("p25", "p50", "age_g", "educat", "year")
df.p25 <- CPS.df[vars]
temp <- unique(df.p25)
temp$year <- as.numeric(as.character(temp$year))
d <- merge(z, temp, by = c('age_g', 'educat', 'year'), all.x = TRUE, all.y = FALSE)
z <- d

rm(CPS.df, d, df.p25, temp, vars)

#gen career
##########################
z$career <- z$rinc > z$p25
z$career <- as.numeric(z$career)
z$career[is.na(z$career)] <- 0

## Generate Interaction Variables

z$kid <- z$nokid==0

z$kid <- as.numeric(z$kid)
z$married <- as.numeric(z$married)
z$keepinghouse <- as.numeric(z$keepinghouse)

z$family <- z$married*z$kid
z$careerkid <- z$career*z$kid
z$careermarried <- z$career*z$married
z$careerfamily <- z$career*z$family
z$keepinghousekid <- z$keepinghouse*z$kid


#################################################
##invert scale of happiness and job satisfaction questions
#################################################

#invert life satisfaction
z$happyb <- NA
z$happyb[z$happy==3] <- 1
z$happyb[z$happy==2] <- 2
z$happyb[z$happy==1] <- 3

z$happy <- NULL
z <- rename(z, c(happyb="happy"))

#invert job satisfaction
z$satjobb <- NA
z$satjobb[z$satjob==4] <- 1
z$satjobb[z$satjob==3] <- 2
z$satjobb[z$satjob==2] <- 3
z$satjobb[z$satjob==1] <- 4

z$satjob <- NULL
z <- rename(z, c(satjobb="satjob"))

# Invert family satisfaction
z$satfamb <- NA
z$satfamb[z$satfam==7] <- 1
z$satfamb[z$satfam==6] <- 2
z$satfamb[z$satfam==5] <- 3
z$satfamb[z$satfam==4] <- 4
z$satfamb[z$satfam==3] <- 5
z$satfamb[z$satfam==2] <- 6
z$satfamb[z$satfam==1] <- 7

z$satfam <- NULL
z <- rename(z, c(satfamb="satfam"))

################################
# Additional variable creation #
################################

# generate bdec
z$bdec <- trunc(z$by/10)
table(z$bdec)

# Generate alternative scaling of vhappy variable
z$vhappyb <- z$vhappy*100

# Spouse's work conditions
z$spouse_work <- NULL
z$spouse_work[z$spwrksta==1] <- 1
z$spouse_work[z$spwrksta==2] <- 2
z$spouse_work[z$spwrksta==7] <- 7

# Job-satisfaction dummies
z$vjobsat1 <- z$satjob == 4 
z$vjobsat2 <- z$satjob == 3 | z$satjob == 4
z$vjobsat1 <- as.numeric(z$vjobsat1)
z$vjobsat2 <- as.numeric(z$vjobsat2)


# Gender dummy for regressions
z$d_sex <- z$sex == 1
z$d_sex <- as.numeric(z$d_sex)


# limiting the data set to years where career variable is defined
# z = z[z$year >= 1977 & z$year < 2012,]

# Save Dataset for Analysis !!
save( z , file = "data/data_final.rda" )

