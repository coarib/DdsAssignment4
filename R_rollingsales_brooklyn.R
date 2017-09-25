# Author: Benjamin Reddy
# Taken from pages 49-50 of O'Neil and Schutt

#require(gdata)
#require(plyr) #Added by Monnie McGee
#install the gdata and plyr packages and load in to R.
library(plyr)
library(gdata)
#setwd("C:/MSDS 6306-FALL2016/404/Live session 06")
# setwd("G://onlineSchool//SMU//MSDS6306//lectureNotes//week5")
## You need a perl interpreter to do this on Windows.
## It's automatic in Mac
#bk <- read.xls(".\\Data\\rollingsales_brooklyn.xls",pattern="BOROUGH",perl = "C:\\Strawberry\\perl\\bin\\perl.exe")

# So, save the file as a csv and use read.csv instead
bk <- read.csv(".\\Data\\rollingsales_brooklyn.csv",skip=4,header=TRUE)


## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))

names(bk) <- tolower(names(bk)) # make all variable names lower case

## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))

## clean/format the data with regular expressions
#bk$land.square.feet <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))

bk$year.built <- as.numeric(as.character(bk$year.built))



## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]

writeLines("\nsummary(bk):\n")
print(summary(bk))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n,breaks=seq(0,340000000,1000000)) 
detach(bk)


## Check the data 
writeLines("\nhead(bk.sale)\n")
print(head(bk.sale))

writeLines("\nsummary(bk.sale)\n")
print(summary(bk.sale))

writeLines("\nstr(bk.sale)\n")
str(bk.sale) # Very handy function!

plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log10(bk.sale$gross.sqft),log10(bk.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
dim(bk.homes)


# complete plot() with log10 of bk.homes$gross.sqft,bk.homes$sale.price.n
#   as above "bk.sale"
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))

writeLines("\nsummary(bk.homes) with sale price < 100000\n")
print(summary(bk.homes[which(bk.homes$sale.price.n<100000),]))

## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log10(bk.homes$sale.price.n) <=5) + 0

# find out homes that meets bk.homes$outliers==0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]

writeLines("\nsummary(bk.homes) after outliers were removed:\n")
print(summary(bk.homes))

plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))

attach(bk.homes)
hist(sale.price.n,breaks = 200) 
detach(bk.homes)
