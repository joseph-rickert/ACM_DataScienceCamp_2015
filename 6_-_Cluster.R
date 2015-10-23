
  In this script we will look at Kmeans and hierarchical clustering using the weather data.

set.seed(42)
library(rattle)
library(fpc)  	                       # for the plotcluster function
library(Hmisc)                         # for varclus function

## Weather Data

data(weather)
#
head(weather,2)                          # Look at the data

## KMEANS with plot and statistics

# For thsi exercise, we remove the date, location, Risk, Rain Today and Rain Tomorrow columns.
# Note that the K means algorithm requires numeric variables


weather <- weather[,-c(1,2,22,23,24)]           # Drop the first two columns

numvars <- lapply(weather,is.numeric)			      # Find numeric variables in data set
numdata <- na.omit(weather[,numvars==TRUE])
head(numdata,2)
#
km <- kmeans(x=numdata, centers=3)             #  Compute kmeans with 3 clusters
  
#Generate a scatter plots of the variables colored by clusters

palette()               # obtain the current palette
palette(c("blue","red", "green","orange"))    # six color rainbow



cPlot <- function(vars=sample(1:15,5)){
  plot(numdata[vars], col=km$cluster, pch=16)
  }

cPlot(1:5)
cPlot()
cPlot()

palette("default")      # reset back to the default
