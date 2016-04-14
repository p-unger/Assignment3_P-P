################################################################
#                Download and import of the GSS into R         #
################################################################

# The instruction on how to download and import the GSS is taken from http://asdfree.com
# by anthony joseph damico (ajdamico@gmail.com)


#################################################################################
# Analyze the General Social Survey cross-sectional cumulative data file with R #
#################################################################################


# set your working directory.
# setwd("~/R_data/GSS")

# set the number of digits shown in all output
options( digits = 8 )

# Loading packages
library(foreign) # load foreign package (converts data files into R)


###############################################
# DATA LOADING COMPONENT - ONLY RUN THIS ONCE #
###############################################

# create new character variables containing the full filepath of the file on norc's website
# that needs to be downloaded and imported into r for analysis
GSS.CS.file.location <-	"http://gss.norc.org/documents/spss/GSS_spss.zip"


# create a temporary file and a temporary directory for downloading file to the local drive
tf <- tempfile() ; td <- tempdir()


# download the file using the filepath specified
download.file( 
  GSS.CS.file.location, # download the file stored in the location designated above
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
print( fn[ grep( "sav$" , fn ) ] )
	

# these two steps take a while.  but once saved as a .rda, future loading becomes fast forever after #


# convert the spss (.sav) file saved on the local disk (at 'fn') into an r data frame
GSS.CS.df <- 
	read.spss( 
		fn[ grep( "sav$" , fn ) ] , 
		to.data.frame = TRUE , 
		use.value.labels = FALSE 
	)

# copy to a different object
z <- GSS.CS.df

# remove the original from RAM
rm( GSS.CS.df )

# clear up memory
gc()

# repeat
GSS.CS.df <- z

# repeat
rm( z )

# i have no idea why this works.
gc()
# but if you don't do this on a 3gb ram machine
# you will run out of memory.  go figure.

rm(fn, GSS.CS.file.location, td, tf)

# save the cross-sectional cumulative gss r data frame inside an r data file (.rda)
save( GSS.CS.df , file = "GSS.CS.rda")


