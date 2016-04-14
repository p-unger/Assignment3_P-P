################################################################
#        Download and import Bertrand (2013) files into R      #
################################################################

# set your working directory.
# setwd("~/R_data/GSS")

# set the number of digits shown in all output
options( digits = 8 )

# Loading packages
library(foreign) # load foreign package (converts data files into R)

# create new character variables containing the full filepath of the file on norc's website
# that needs to be downloaded and imported into r for analysis
CPS.location <-	"https://www.aeaweb.org/aer/data/may2013/P2013_4384_data.zip"


# create a temporary file and a temporary directory for downloading file to the local drive
tf <- tempfile() ; td <- tempdir()


# download the file using the filepath specified
download.file( 
  CPS.location, # download the file stored in the location designated above
  tf, # save the file as the temporary file assigned above
  mode = "wb" # download this as a binary file type
)

# the variable 'tf' now contains the full file path on the local computer to the specified file

# store the file path on the local disk to the extracted file (previously inside the zipped file)
# inside a new character string object 'fn'
fn <- 
  unzip( 
    # unzip the contents of the temporary file
    tf , 
    # ..into the the temporary directory (also assigned above)
    exdir = td , 
    # overwrite the contents of the temporary directory
    # in case there's anything already in there
    overwrite = T
  )

# print the temporary location of the spss (.sav) file to the screen
print( fn[ grep( "dta$" , fn ) ] )


# convert the spss (.sav) file saved on the local disk (at 'fn') into an r data frame
CPS.df <- 
  read.dta( 
    fn[ grep( "GSS_aer.dta" , fn ) ] , 
  )

# copy to a different object
z <- CPS.df

# remove the original from RAM
rm( CPS.df )

# clear up memory
gc()

# repeat
CPS.df <- z

# repeat
rm( z )

# clear up memory
gc()


# save the cross-sectional cumulative gss r data frame inside an r data file (.rda)
save( CPS.df , file = "CPS_Bertrand.rda" )

rm(CPS.location, fn, td, tf)

# summary(CPS.df$year)
