#################################################################################
# Data Set Preperation #
#################################################################################

# set your working directory.
setwd("~/R_data/GSS")

#install.packages("Hmisc")
library(Hmisc)
library(gmodels)


# now the r data frame can be loaded directly
# from your local hard drive.  this is much faster.
load( "GSS.CS.rda" )

# copy to a different object
z <- GSS.CS.df

# remove the original from RAM
rm( GSS.CS.df )

# clear up memory
gc()


#############
#consider age 25 to 54
#############


#keep if age>=25 & age<=54


# based on variable values
y <- z[ which(z$age>=25 & z$age<=54), ]
z <- y



#tab year
frq_year <- table(z$year) 
frq_year


#tab wrkstat
frq_wrkstat <- table(z$wrkstat) 
frq_wrkstat

#tab wrkstat
frq_marital <- table(z$marital) 
frq_marital

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
y$nokid <- childs==0

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

# tab year if rinc77~=.

frq_rinc77 <- table(z$rinc77) 
frq_rinc77
summary(z$rinc77)


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

#FAMILY INCOME -family income questioned is modelled same way as r's income question
#...
####


#creating a single variable
egen rinc=rowmean(rinc77 rinc82 rinc86 rinc91 rinc98 rinc06)
egen inc=rowmean(inc77 inc82 inc86 inc91 inc98 inc06)

*deflating
sort year
merge year using ../../CPI/cpi.dta
tab _merge
keep if _merge==3


replace rinc=rinc/def99
replace inc=inc/def99
gen othinc=inc-rinc
sum othinc,d
replace othinc=0 if othinc<0

*satisfaction questions

gen vhappy=happy==1
replace vhappy=. if happy==.d|happy==.i|happy==.n



