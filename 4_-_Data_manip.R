IBM.stock  <- read.csv("IBM.stock.csv")

## Augment the data frame
#Add a new variable, Volatility, to a data frame, The first uses "$" to index into the data frame, The second uses the within function

IBM.stock$Volatility<- (IBM.stock$High - IBM.stock$Low)/IBM.stock$Open
head(IBM.stock)

## Prune the data frame
#We show two was to prune the data frame so that it only include prices after January 1, 2000. Note that both methods use row, column indexing into the data frame IBM.stock[row,column]. The first thing we do is check to see what data types the varables are. Noticing that Date is a factor, we willmake it a date as we build a new data frame.
sapply(IBM.stock,class)  		# Note that Date is a factor

IBM.stock.2000 <- IBM.stock[as.Date(IBM.stock$Date) > as.Date('2000-01-01'),]
head(IBM.stock.2000)
tail(IBM.stock.2000)

## Aggregate data
#Here we will aggregate daily observations to form a monthly series. First we will create new year and month variables by extracting the relevant information from the Date variable

IBM.stock.2000$Month <- substr(IBM.stock.2000$Date,6,7)  		# Add variable Month to data frame
IBM.stock.2000$Year  <- substr(IBM.stock.2000$Date,1,4)			# Add variable Year to the data frame
head(IBM.stock.2000)

#Make a new data frame to hold the aggregated monthly prices.

IBM.stock.month <- aggregate(.~Month+Year,data=IBM.stock.2000,mean) # The dot in the formula stands for everything
head(IBM.stock.month)

# What did aggregate do?
L1 <-  as.integer(IBM.stock.2000$Month)==1
L2 <- as.integer(IBM.stock.2000$Year)==2000
df <- IBM.stock.2000[L1 & L2,]
mean(df$Open)

#Make a date variable in IBM.stock.month and assign everything to the first of the month

IBM.stock.month$Date <- as.Date(paste(IBM.stock.month$Year,IBM.stock.month$Month,'01',sep='-'))
head(IBM.stock.month,3)

#Now sort the data frame to get the latest data first
IBM.stock.month <- IBM.stock.month[with(IBM.stock.month,order(-as.integer(Year),Month)),]
head(IBM.stock.month,3)

## Merge data
#We will merge the the data frame containing the aggregated monthly stock prices from the year 2000 to the present with a new data frame containing dividend data. First we get the dividend data.

url2 <- "http://ichart.finance.yahoo.com/table.csv?s=IBM&a=00&b=2&c=1962&d=11&e=22&f=2011&g=v&ignore=.csv"
IBM.div <- read.table(url2,header=TRUE,sep=",")
#write.csv(IBM.div,"IBM.div.csv",row.names=FALSE)
head(IBM.div)
#
class(IBM.stock.month$Date)
class(IBM.div$Date)

#Make the IBM.div date into a proper date object
IBM.div$Date <- as.Date(IBM.div$Date)
class(IBM.div$Date)

#Next we write a function to Create a column for the merge picking the last dividend dispersed.

fcn <- function(x){
  as.character(IBM.div$Date[min(which(IBM.div$Date < x))])
}
IBM.stock.month$divDate <- sapply(IBM.stock.month$Date,fcn)
IBM.stock.month$divDate <- as.Date(IBM.stock.month$divDate)
head(IBM.stock.month)

#Do the merge

IBM <- merge(IBM.stock.month,IBM.div,by.x='divDate',by.y='Date')
head(IBM,2)

## Is there an easirer way to merge?
#Lets try using the join function from the plyr package.

library(plyr)
head(IBM.stock.month,3)
head(IBM.div,3)
names(IBM.div)[1] <- "divDate"
#
IBM2 <- join(IBM.stock.month,IBM.div,by='divDate')
head(IBM2,3)


## Comparing data frames
library(compare)
# Sort both data frames to compare them.   
IBMs <- IBM[order(-as.integer(as.Date(IBM$Date))),]
IBM2s <- IBM2[order(-as.integer(as.Date(IBM2$Date))),]
comparison <- compare(IBMs,IBM2s,allowAll=TRUE)
comparison$result

## Reshaping a data frame
#Finaly, let's look at using the reshape function to make "wide" and "long" versions of the data set.

IBM.wide <- reshape(IBM[,c("Year","Month","Close")],idvar="Year",timevar="Month",direction="wide")
IBM.long <- reshape(IBM.wide,idvar="Year",timevar="Month",direction="long")
head(IBM,3)
head(IBM.wide,3)
head(IBM.long, 3)

## Hadley Wickham's tidy data
# Over the past few years Hadley Wickham has written a large nummer of R packages 
# aimed at facilitating data wrangling with R while simplyfing and standardizing 
# syntax. 
library(reshape2)
ibm <- IBM[,c("Year","Month","Close")]
head(ibm)
ibm.wide <- dcast(ibm, Year  ~ Month, value.var ="Close")
head(ibm.wide,3)
ibm.long <- melt(ibm.wide, id.vars="Year",
                           variable.name = "Month",
                           value.name="Close")
head(ibm.long,3)

