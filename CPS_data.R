#################################################
# CPS - download and income percentiles by year #
#################################################

# Setting working directory
setwd("~/Desktop/R_ass3")

# Install required packages from Adsfree.com by anthony joseph damico
install.packages( c( "MonetDB.R" , "MonetDBLite" , "devtools" , "survey" , "SAScii" , "descr" , "downloader" , "digest" , "haven" , "devtools" ) , repos=c("http://dev.monetdb.org/Assets/R/", "http://cran.rstudio.com/") )

# Load downloader package
library(downloader)
library(devtools)
cps.years.to.download <- c( 2015 , 2014 , 2014.58 , 2014.38 , 2013:1998 )
source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Current%20Population%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )
