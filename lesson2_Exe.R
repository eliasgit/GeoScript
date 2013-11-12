
# ELIAS GEBREMESKEL
# date 08.11.2013
#################################################################################
####### writing leap year R script
setwd("M:/Geo_script")
library(rasta)
is.leap <- function (x){
                if (x%%400 == 0){
                output <- TRUE
              }
            else if (x%%100 == 0){
              output <- FALSE
            }
          else if(x%%4 == 0){
            output <- TRUE 
          } 
          else{
            output <- FALSE
          }
     return(output)
}
is.leap(2000) ###### is TRUE
  
