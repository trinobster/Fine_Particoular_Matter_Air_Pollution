
# Case Study - Fine Particular Matter Air Pollution in the United States
# We want to answer the very basic question: 
# are the levels of PM2.5 on average lower in 2012 than they were in 1999?

# PM2.5 values are measured in µg/m^3

## --------------------------------------------------------------------
## 0) First of all, we setup the working directory and load few libraries:

pathToDirectory <- readline(prompt="\n\nWelcome, please enter the path to the directory where you put Costanti.R and MyUtils.R\nAlso, data should be put in this path in a directory named: dati \n")
setwd(pathToDirectory)

source("MyUtils.R") # this file contains utility functions
source("Costanti.R")
library(ISLR)
library(sqldf)

## --------------------------------------------------------------------
## 1) Opening up and exploring the data files

# Here is the data frame for 1999 data:
pm0 = readFile(paste(path, dati1999, sep = "_"))
# By default, # is the symbol for comments in the dataframe so first 2 lines will be ignored
# We set header = FALSE so name columns are generic: V1, V2, ...
# The separation character is |

# How many dimensions you got?
dim(pm0) # there are 117421 rows and 28 columns

# Let's take a look at the first few rows.
head(pm0)

# Get the column names, so that we don't have to refer to them by V1 V2...
nameList = getNames(paste(path, dati1999, sep = "_"))
colnames(pm0) = nameList

# Take a look at PM2.5 column, calling it x0.
# Run class, str, summary functions to see what it is, and write down some considerations on the values that you see.
x0 = visualizeSamples(pm0)
# The class is numeric; x0 reports the particle mass as we can see from this compact display of the internal structure;
# there are missing values and 117421 totally measures.
# From the summary we can notice that the minimum value is 0.00 and there are 13217 missing values on 117421.
m0 = mean(x0, na.rm = T)# Mean value is 13.74
median0 = median(x0, na.rm = T) # median is 11.5
sd0 = sd(x0, na.rm = T) # standard deviation is 9.41

# What having more than 13K missing values means, in terms of the proportion on the entire data set?
( getMissingValue(x0)[[1]] / length(x0) ) * 100 # 11.26% of dataset is NA

## --------------------------------------------------------------------
## 2) Are missing data a problem?

# We try to answer to this question, making a list of matrix in which 
# we can see total missing observations relative to each State/County/Site:
pm0MISS = pm0[is.na(pm0$Sample.Value),] # Here we have pm0 where PM2.5 samples are missing
colnames(pm0MISS) = nameList
attrList = list(pm0MISS$Site.ID, pm0MISS$State.Code, pm0MISS$County.Code)
myList = list()
attrName = c("Site.ID", "State.Code","County.Code")

for(j in 1 : length(attrList)){
  mat = analyseAttr(pm0MISS, attrList[j], attrName[j], pm0) #splitta in funzione di quell'attributo
  myList[[length(myList) + 1]] = mat
  write.table(myList[[j]], file = paste(attrName[j], "mat.txt", sep = "_"), 
              sep = "\t", row.names = FALSE, col.names = FALSE)
  myList[[j]] = myList[[j]][myList[[j]][, "Perc"] >= 30, ] 
  # We extract some interesting values and save them in myList
}


# Here is the data frame for 2012 data:
pm1 = readFile(paste(path, dati2012, sep = "_"))

# How many observations exactly we have here?
dim(pm1) # This file contains 1304287 observations

# Give the names for this data frame to be the same as the other one.
colnames(pm1) = nameList

# As before, take a quick check on what we have here in terms of PM2.5 values (call it x1), using some summary functions.
x1 = visualizeSamples(pm1)
# The class is numeric, there are 1304287 values and the missing value percentage is:
( getMissingValue(x1)[[1]] / length(x1) ) * 100 # only 5.61 % this time. 
# But we can notice that the minimum is -10.00, a negative value. Mean value is 9.14
m1 = mean(x1, na.rm = T) # mean is 9.14
median1 = median(x1, na.rm = T) # median is 7.63
sd1 = sd(x1, na.rm = T) # standard deviation is 8.56
# Now let's do a quick comparison of the 2012 and the 1999 data: do we have good news for public health?
m1 < m0 # Observing mean values looks like we have good news
median1 < median0
sd1 < sd0

# [PLOT 1] Take a look at a plot, a visual representation of this data: which graph would you choose?
plot(x0, main = title1999, ylab = sample, type = "p")
# [PLOT 2]
plot(x1, main = title2012, ylab = sample, type = "p")

# [PLOT 3] We plotted 2012 and 1999 on the same window:
plot(x1, main = paste(title1999, title2012, sep = " and "), ylab = sample, type = "l", col = "blue")
lines(x0, col = "green")
legend('top', c(title2012, title1999), col = c("blue", "green"), lty = 1:2)

# Now we want to plot mean values for every month and both years in a Barplot
# Graph for every month in 1999:
monthCol0 = extractMonths(pm0$Date)
lista0 <- split(cbind(pm0,monthCol0), monthCol0) #splitta per mese i dati del 99

vectorValue <- c()
for (i in 1 : 12){
  vectorValue[i] <- mean(lista0[[i]]$Sample.Value, na.rm = T) #calcola i valori medi per ogni mese
}

my_colors <- c("orange", "yellow")
names(vectorValue) <- c("J", "F", "M", "A", "M", "J", "Ju", "A", "S", "O", "N", "D")
barplot(vectorValue, main = "1999 MONTHS' MEANS BARPLOT", xlab="MONTHS", ylab="VALUES MEAN")
abline(h = m0)

# Graph for every month in 2012: (we observed that in 2012 we only have 10 observations months)
monthCol1 = extractMonths(pm1$Date)
lista1 <- split(cbind(pm1,monthCol1), monthCol1)

vectorValue1 <- c()
for (i in 1 : 10){
  # For every month in 2012, we put its mean value in this vector
  vectorValue1[i] <- mean(lista1[[i]]$Sample.Value, na.rm = T)
}
my_colors <- c("red", "blue")
barplot(vectorValue1, main = "1999 MONTHS' MEANS BARPLOT", xlab="MONTHS", ylab="VALUES MEAN")
abline(h = m1)

# Now we create a matrix(12, 2) to fill with mean values founded for every month:
# column 1 represents year 1999, while column 2 represents year 2012
my_new_vector <- matrix(nrow = 12, ncol = 2) 
for (i in 1 : 12) {
  if (i > 10){
    my_new_vector[i, 1] <- vectorValue[i]
    my_new_vector[i, 2] <- 0
  }
  else{
    my_new_vector[i, 1] <- vectorValue[i]
    my_new_vector[i, 2] <- vectorValue1[i]
  }
}
my_data <- structure(list(my_new_vector[1, ], my_new_vector[2, ], my_new_vector[3, ], my_new_vector[4, ], 
                          my_new_vector[5, ], my_new_vector[6, ], my_new_vector[7, ], my_new_vector[8, ],
                          my_new_vector[9, ], my_new_vector[10, ], my_new_vector[11, ], my_new_vector[12, ]), 
                     .Names = month.abb, 
                     class = "data.frame", row.names = c(NA, -2L))
attach(my_data)
print(my_data)

# [PLOT 4] Finally we can obtain a barplot for every mean value in every month:
barplot(as.matrix(my_data), main = "1999 & 2012 Means' Barchart", ylab = "Means [µg/m^3]", cex.lab = 1.5, cex.main = 1.4, beside = TRUE, col = my_colors)
abline(h = m0, lwd = 3, col = "orange")
abline(h = m1, lwd = 3, col = "green")
legend("bottom", inset = .02, c("1999","2012"), cex = 0.65, fill = my_colors, bg = 'lightyellow')
legend("bottom", inset = .22, cex = 0.65, bg = 'lightyellow', legend = c("Mean 1999", "Mean 2012"), col = c("orange", "green"), lwd = 3)

# From this last plot, we can say that, in general, we had an improvement in 2012 for public health

# What can you say in terms of outliers and extreme values?
# There are more outliers in the 2012 dataset than in the 1999 one:
# in fact the maximum value for x1 is 909.00 µg/m^3, but is far away 
# from the mean one (9.14 µg/m^3).

# Calculating inner fences for x1 with the help of a function:
pmOut1_min = calcInnerFences(x1, pm1, 1.5)
dim(pmOut1_min) # there are 47160 observations, the 3.62% of 2012 dataset (pm1)
# A point that falls outside the data set's inner fences is classified as a minor outlier, 
# while one that falls outside the outer fences is classified as a major outlier. 
pmOut1_maj = calcInnerFences(x1, pm1, 3)
dim(pmOut1_maj) # there are 9739 observations which are major outliers (0.74%)

# Calculating inner fences for x0 with the help of a function:
pmOut0_min = calcInnerFences(x0, pm0, 1.5)
dim(pmOut0_min) # there are 3605 observations, the 3.07% of 1999 dataset
pmOut0_maj = calcInnerFences(x0, pm0, 3)
dim(pmOut0_maj) # there are 634 observations which are major outliers (0.54%)

# [PLOT 5] Run a boxplot on x0 and x1:
par(mfrow = c(1,1))
# lmts per plottare con la stessa scala. quel casino per calcolare il range senza outliers
lmts <- range(x0[!x0 %in% boxplot.stats(x0)$out], x1[!x1 %in% boxplot.stats(x1)$out], na.rm = T)
boxplot(x0, x1, horizontal = F, main = "1999 vs 2012", names = c("1999","2012"), ylab = sample, ylim=lmts+c(-4,200))
#boxplot(x1, horizontal = F, main = title2012, xlab = sample, ylim=lmts)

# [PLOT 6] here are the same without outliers:
par(mfrow = c(1,1))
boxplot(x0, x1, outline = F, horizontal = F, main = "1999 vs 2012", names = c("1999","2012"), ylab = sample, ylim=lmts)
#boxplot(x1, outline = F, horizontal = F, main = title2012, ylab = sample, ylim=lmts)

# We have asymmetric distributions, so we also calculate the mode:
moda1 = Mode(x1) # the most common value in pm1 is 6
moda0 = Mode(x0) # in pm0 is 7

# Find a way to fix this box plot: which transformation would you apply 
# to the two variables in order to even out the box plot a little bit?
# We could use the logarithm to fix the right skew (here R warns us that logarithm has endless bound and of course it can't be well represented in a boxplot)

# [PLOT 7]
x0_<-log(x0, base = exp(1))
x1_<-log(x1, base = exp(1))
lmtsLog <- range(x0_[!x0_ %in% boxplot.stats(x0_)$out], x1_[!x1_ %in% boxplot.stats(x1_)$out], na.rm = T)

boxplot(x0_, x1_, horizontal = F, main =  "1999 vs 2012 with log tranformation",names = c("1999","2012"), ylab = logsample, ylim=lmtsLog+c(-4,3))
#boxplot(log(x1, base = exp(1)), horizontal = T, main = title2012, xlab = logsample)

# [PLOT 8] here are the same without outliers:
boxplot(x0_, x1_, outline = F, horizontal = F,main =  "1999 vs 2012 with log tranformation",names = c("1999","2012"), ylab = logsample, ylim=lmtsLog)
#boxplot(log(x1, base = exp(1)), outline = F, horizontal = T, main = title2012, xlab = logsample)

# Which main difference do you notice between the two data sets?
# With this boxplot can compare the two graphs with greater ease than in plot 3
# and we can observe that the 1999 boxplot is a bit shifted to the right, consequently every quartile, 
# median included. For this reason we can say that we record a slight increase of the PM2.5 mass.

# What do you notice in the summary of 2012 data that is really strange?
# The minimum value is negative, which it's strange because the measure concernes the particle 
# mass and it can't be negative.

## --------------------------------------------------------------------
## 3) Why are there negative values?

# How many negative values do I have? What is the proportion on the entire 2012 data set?

pm1Neg = pm1[pm1$Sample.Value < 0 & !is.na(pm1$Sample.Value), ]

# The positive value without the missing values are:
pm1Pos = pm1[pm1$Sample.Value >= 0 & !is.na(pm1$Sample.Value), ]

dim(pm1Neg) # There are 26.474 in the 2012 data set over 1.304.287 totally observations; so the proportion is:
( dim(pm1Neg)[1] / dim(pm1)[1] ) * 100 # about 2.02%

dim(pm1Pos) 
# There are 1.204.680 positive values without missing values in the 2012 data set
# over 1.304.287 totally observations; so the proportion is:
( dim(pm1Pos)[1] / dim(pm1)[1] ) * 100 # about 92.37% (In fact 100-5,61(missing value in 2012)-2,02 = 92,37%)

# It may be interesting to see whether there are negative values at certain times of the year, 
# or maybe the only negative values occur at certain locations or times.
# First of all, convert the Date column from integer to date format:
Date1 = correctDate(pm1$Date)

# [PLOT 9] Take a look at the histogram of the dates by month to see where the collection occurs:
hist(Date1, "months", main = titleHist, xlab = data2012)
# What can you observe from the graph?
# We can notice that a lot of observations has been taken in the period between december
# and june, less between june and september and almost none from september to november

# [PLOT 10] Take now a look at the histogram of the dates where the negative values occur
DateNeg = correctDate(pm1Neg$Date)
hist(DateNeg, "months", main = titlenegHist, xlab = data2012)

DatePos = correctDate(pm1Pos$Date)
hist(DatePos, "months", main = titleposHist, xlab = data2012) #It's very similar at the Date1 hist
# What can you observe from the graph?
# Looks like during winter and spring, between december and june, the most of negative sample values occur
# while during summer, between june and september, the frequency of negative sample values measured is lower


dim(pm1)
x0 = visualizeSamples(pm1) #Min, 1st Qu, Median, Mean, 3rd Qu, Max of pm1
#x0withneg = visualizeSamples(pm1Neg)
x0pos = visualizeSamples(pm1Pos) #Min, 1st Qu, Median, Mean, 3rd Qu, Max of pm1Pos
m0pos = mean(x0pos, na.rm = T)# Mean value is 9.38
median0pos = median(x0pos, na.rm = T) # median is 7.9
sd0pos = sd(x0pos, na.rm = T) # standard deviation is 8.48




# One issue with PM2.5 is that many areas of the country it tends to be very low in the winter and high in the summer. 
# So typically when pollution values are high, they're easier to measure and when they're low, they're harder to measure. 
# So maybe some of the negative values are just kind of a measurement error when the values tend to be very low.
# But given that it's only 2% of the data, we're not going to spend too much time worrying about it at the moment.


## --------------------------------------------------------------------
## 4) Exploring change at one monitor

# Pick New York state (state code = 36) and try to find a monitor in that state to see whether 
# or not the pollution levels have gone down.
# How can you find which county / monitor ID numbers combinations exist in both data sets?

attach(pm0)
pm0NY = pm0[State.Code == 36, ]

attach(pm1)
pm1NY = pm1[State.Code == 36, ]

#####nuovo codice ##########


countySite0 = paste(pm0NY$County.Code, pm0NY$Site.ID) 
countySite1 = paste(pm1NY$County.Code, pm1NY$Site.ID) 
pm0NY <- cbind(pm0NY, countySite0)
pm1NY <- cbind(pm1NY, countySite1)
intersection <- intersect(countySite0, countySite1) 


library('plyr') # library for "count"
freq0 <- count(pm0NY, 'pm0NY$countySite0')
names(freq0)[1] <- 'countySite'
freq1 <- count(pm1NY, 'pm1NY$countySite1')
names(freq1)[1] <- 'countySite'


index0 <- c()
index1 <- c()
for (i in 1:length(intersection)) {
  ind0 <- match(intersection[i], freq0$countySite)
  index0 <- append(index0, ind0)
  
  ind1 <- match(intersection[i], freq1$countySite)
  index1 <- append(index1, ind1)
}

freq00 <- freq0[index0,]
freq11 <- freq1[index1,]

########
#index in pm1NY
#index1 <- match(intersection, countySite1)

#length(intersection) #10

pm0NYcombSplit <- split(pm0NY, pm0NY$countySite0)
pm1NYcombSplit <- split(pm1NY, pm1NY$countySite1)

# altro modo per contare
dim0 <- lapply(index0, function(x) dim(pm0NYcombSplit[[x]])[1])
dim1 <- lapply(index1, function(x) dim(pm1NYcombSplit[[x]])[1])

dim0 <- unlist(dim0)
dim1 <- unlist(dim1)
#da qui basterebbe risalire all'indice in index e verificare in freq0...ma e' un giro inutile
#######fine codice nuovo ######


# We create an empty data frame with pm0NY dimensions (for now)
v = rep(0, dim(pm0NY)[1])
pm = data.frame(v)
for(i in 2 : dim(pm0NY)[2]){
  pm = cbind(pm, v)
}

# We setup names for the columns in pm
colnames(pm0NY) <- gsub("\\.","_",colnames(pm0NY)) # this row lets sqldf package to be used
colnames(pm1NY) <- gsub("\\.","_",colnames(pm1NY))

# In pm0Split we have a list of dataframes splitted by equal values od Site_ID
pm0Split = split(pm0NY, pm0NY$Site_ID)
counter = 1
attach(pm1NY)

# Now we will put values in pm
for(i in 1 : length(pm0Split)){       # we take every dataframe from pm0Split
  cc0 = pm0Split[[i]]$Site_ID[1] # cc0 is the Site_ID of dataframe which position is "i" in the list
  
  # With a query from sqldf library, we take every line in pm1NY which has the same Site_Id of cc0
  res = sqldf(strwrap(sprintf("select * from pm1NY where Site_ID = %s", cc0)))
  
  if(dim(res)[1] != 0){ # if the query result is not empty
    
    for(j in 1 : dim(res)[1]){
      pm[counter, ] = res[j, ] # we put the lines in pm
      counter = counter + 1
    }
  }
}

pm = head(pm, counter - 1)

# How many of them are there?
dim(pm)[1] # There are 421 SITE ID observations in 1999 data that match in 2012's, for State Code = New York

# Considering only New York state, 
# what we want to do is to split this data frame by the kind of monitor. 
# So, split it into separate data frames by each monitor and then count 
# how many observations there are in each of them, in each period (1999 and 2012).

# For 1999 data, considering New York state we already splitted considering the Site_ID:
summary(pm0Split)
obs0 = countObs(pm0Split)
obs0

# For 2012 data, considering New York state we split dataframe cosidering Site_ID:
pm1Split = split(pm1NY, pm1NY$Site_ID)
summary(pm1Split)
obs1 = countObs(pm1Split)
obs1

# For the combination of 1999 and 2012 data, cosidering New York state and splitting with SIte_ID:
colnames(pm) = nameList
pmSplit = split(pm, pm$Site.ID)
summary(pmSplit)
obs = countObs(pmSplit)
obs

colnames(pm0NY) <- gsub("\\_",".",colnames(pm0NY)) # this row lets sqldf package to be used
colnames(pm1NY) <- gsub("\\_",".",colnames(pm1NY))
# Call a new data-frame pm1sub, as a subset of pm1 with Country code 63 and site ID 2008 
# (always related to New York state). 
pm1sub = subset(pm1NY, County.Code == 63 & Site.ID == 2008)

# Then do the same thing for the 1999 data, and call it pm0sub.
pm0sub = subset(pm0NY, County.Code == 63 & Site.ID == 2008)

# Take a quick look at the dimensions of these two new data sets.
dim(pm0sub) # we have 122 rows
dim(pm1sub) # we have 30 rows, so in 2012 have been collected less observations than in 1999

# Plot the PM2.5 data as a function of time: 
# on the x axis is going to be the date and on the y axis is going to be the level PM2.5.

# We correct the date:
pm1sub$Date = as.Date(as.character(pm1sub$Date), "%Y%m%d")
pm0sub$Date = as.Date(as.character(pm0sub$Date), "%Y%m%d")

# For 2012, you can see that the data are bouncing around all over the place, 
# somewhere between 4 and 14 micrograms per meter cubed:
plot(pm1sub$Date, pm1sub$Sample.Value, main = main2012, xlab = titleDate2012, ylab = titleSample)

# For 1999, first of all you'll notice that the data are only actually recorded 
# starting in July, through the end of the year. 
# So only about a half of the year of data is collected there, 
# and you can see that they range from roughly five micrograms per meter cubed to about 40.
plot(pm0sub$Date, pm0sub$Sample.Value, main = main1999, xlab = titleDate1999, ylab = titleSample)

# Of course it's a little hard to look at the plots separately, 
# so put both 1999 and 2012 on the same panel. 
# (Hint: you need to use par function for that). 
# Please, put a little line where the median is, for each year. 
# In order not to be misleading, also please put the two plots on the same range.

par(mfrow = c(1, 2))
title(main, outer = T)

#ymin <- min(pm1sub$Sample.Value, pm0sub$Sample.Value, na.rm = TRUE)
#ymax <- max(pm1sub$Sample.Value, pm0sub$Sample.Value, na.rm = TRUE)
lmts <- range(pm0sub$Sample.Value, pm1sub$Sample.Value, na.rm = T)+c(0,6)


# [PLOT 12] 1999 sample values for State Code 36, County Code 63 and Site ID 2008
plot(pm0sub$Date, pm0sub$Sample.Value,
     main = main1999,
     xlab = titleDate1999, ylab = titleSample, ylim =lmts)
v = rep(median(pm0sub$Sample.Value, na.rm = T), length(pm0sub$Date))
lines(pm0sub$Date, v, col = "red") # median
abline(h = mean(pm0sub$Sample.Value, na.rm = TRUE), col = "green") # mean
abline(h = sd(pm0sub$Sample.Value, na.rm = TRUE), col = "grey") # standard deviation
legend("topright", c("Median","Mean", "Standard Deviation"),lty = c(1,1), lwd = c(2.5,2.5), col = c("red", "green", "grey"), cex = 0.65, bty = 'n')

# [PLOT 11] 2012 sample values for State Code 36, County Code 63 and Site ID 2008
plot(pm1sub$Date, pm1sub$Sample.Value,
     main = main2012,
     xlab = titleDate2012, ylab = titleSample, ylim = lmts)
v = rep(median(pm1sub$Sample.Value, na.rm = T), length(pm1sub$Date))
lines(pm1sub$Date, v, col = "red") # median
abline(h = mean(pm1sub$Sample.Value, na.rm = TRUE), col = "green") # mean
abline(h = sd(pm1sub$Sample.Value, na.rm = TRUE), col = "grey") # standard deviation
legend("topright", c("Median","Mean", "Standard Deviation"),lty = c(1,1), lwd = c(2.5,2.5), col = c("red", "green", "grey"), cex = 0.65, bty = 'n')


# You can see that the median is going down between the two years at this monitor
# and more interestingly actually is the fact that there's a huge spread of points 
# in the 1999 data and there's a relatively modest spread of points for the 2012 data.
# What do these two facts mean according to you in terms of air pollution phenomenon 
# and people health?

## --------------------------------------------------------------------
## 5) Exploring change at the state level
# We'll look at the individual states to see how the individual states have improved or not across the years.

# First we setup 1999 data:
# The positive value without the missing values are:

#IF YOU WANT REMOVE ONLY NA VALUES (AND NOT THE NEGATIVE) DECOMMENT THIS LINE OF CODE AND COMMENT THE NEXT
#pm0NA = pm0[!is.na(pm0$Sample.Value),] # remove the NA

pm0NA = pm0[!is.na(pm0$Sample.Value) & pm0$Sample.Value >= 0,] # remove the NA values and neg
dim(pm0NA)
stateCodeCol = pm0NA$State.Code # we take the State Code Column
firstVal = stateCodeCol[1] 
valuePMCol = pm0NA$Sample.Value # and the Sample Value column
cont = 1
list = 1

# We create a matrix with 3 columns: StateCode, Tot PM, and cont
# We do this to analyze better the results
a0 = matrix(nrow = length(stateCodeCol), 
            ncol = 3,             # number of columns 
            byrow = TRUE)         # fill matrix by rows

# We initialize the first row of the matrix
a0[1, 1] = firstVal
a0[1, 2] = valuePMCol[1]
a0[1, 3] = cont

# Now we can fill the matrix
for (i in 2 : length(stateCodeCol)){
  
  if(stateCodeCol[i] == stateCodeCol[i - 1]){
    a0[list, 2] = a0[list, 2] + valuePMCol[i]
    a0[list, 3] = a0[list, 3] + 1 
  }
  else{
    list = list + 1
    cont = 1
    a0[list, 1] = stateCodeCol[i]
    a0[list, 2] = valuePMCol[i]
    a0[list, 3] = cont
  }
}
b0 = head(a0, list)

#IF YOU WANT REMOVE ONLY NA VALUES (AND NOT THE NEGATIVE) DECOMMENT THIS LINE OF CODE AND COMMENT THE NEXT
#pm1NA = pm1[!is.na(pm0$Sample.Value),] # remove the NA

# We now do the same thing for 2012 data:
pm1NA = pm1[!is.na(pm1$Sample.Value) & pm1$Sample.Value >= 0,] # remove the NA values and neg val
dim(pm1NA)
stateCodeCol = pm1NA$State.Code # we take the State Code Column
firstVal = stateCodeCol[1]
valuePMCol = pm1NA$Sample.Value # and the Sample Value column

cont = 1
list = 1

# Create a matrix with 3 columns: StateCode, Tot PM, and cont
# We do this to analyze better the results
a1 = matrix(nrow = length(stateCodeCol), 
            ncol = 3,             # number of columns 
            byrow = TRUE)         # fill matrix by rows

# Now we initialize the first row of the matrix
a1[1, 1] = firstVal
a1[1, 2] = valuePMCol[1]
a1[1, 3] = cont

# And finally we can fill the other matrix also for 2012 data
for (i in 2 : length(stateCodeCol)){
  
  if(stateCodeCol[i] == stateCodeCol[i - 1]){
    a1[list, 2] = a1[list, 2] + valuePMCol[i]
    a1[list, 3] = a1[list, 3] + 1 #errore
  }
  else{
    list = list + 1
    cont = 1
    a1[list, 1] = stateCodeCol[i]
    a1[list, 2] = valuePMCol[i]
    a1[list, 3] = cont
  }
}
b1 = head(a1, list)

namesCol <- c("StateCode", "PMtot", "Cont", "Mean") # Names of the column

# Now we calculate the mean vaue for 1999
m0 = b0[, 2]/ b0[, 3] # Divide the second column (the PMtot column) by the third column (the Cont column) and obtain the relative mean
b0 = cbind(b0, m0) # cbind() function combines vector, matrix or data frame by columns
b00 <- data.frame(b0) # b00 is the new dataframe with the matrix bo
colnames(b00) <- namesCol # We assign the names at the column

# Now we calculate the mean vaue for 2012
m1 = b1[, 2] / b1[, 3]
b1 = cbind(b1, m1) 
b11 <- data.frame(b1)
colnames(b11) <- namesCol

# Working on the data frame, you can do multiple tasks in a simpler way
# dataframe with the mean of the state between 1999 and 2012
# What I want you to create is a new data frame which has one row for state and then for each row
# there's the state average of PM2.5 for 1999 and the state average of PM2.5 for 2012.
c <- merge(b00, b11, by = "StateCode", all = TRUE) # Merge the two dataframe with the same StateCode
toRemove <- c(2, 3, 5, 6) #Remove the useless columns
cc <- c[, -toRemove]

# Now what we want to do is create a plot that plots those state averages and then connects them with the line.
line1999 <- c(1 : 53)
line1999[1 : 53] <- 1999
line2012 <- c(1 : 53)
line2012[1 : 53] <- 2012
par(mfrow = c(1, 1))


# Plot the first line of points relative at the 1999 data
plot(line1999, cc[, 2], xlim = c(1999, 2012), ylim = c(4, 21))
# Plot on the same graph the second line of points relative at the 2012 data
points(line2012, cc[, 3])


# in this part of code we connect with a line the pair of points concerning the same State
# in this way we can see if a state increased or decreased the level of air pollution
stateNO <- c()
for (i in 1 : dim(b00)[1]) {
  
  if(!is.na(cc[i, 3])){
    if(cc[i,2] > cc[i, 3]){
      segments(line1999[i], cc[i, 2], line2012[i], cc[i, 3], col = "green")  
      # Color with green if the state decreased the level of air pollution 
    }else{
      segments(line1999[i], cc[i, 2], line2012[i], cc[i, 3], col = "red")
      # Color with red if the state increased the level of air pollution
      print(cc[i, 1])
    }
  }
}


## PUNTO 2 CORREZIONE
pm0MISS = pm0[is.na(pm0$Sample.Value),]
colnames(pm0MISS) = nameList

counterSiteID = analyseAttr(pm0MISS, pm0MISS$Site.ID)

# chiamare per altri attributi e capire dalle percentuali se sono distribuiti


## --------------------------------------------------------------------
## 6) Predictions

Ohio12 = pm1[pm1$State.Code == 39, ]
pm1NA = Ohio12[!is.na(Ohio12$Sample.Value), ]

# For the complete neural network (not only on Ohio),
# you have to replace the last two rows with the following one,
# all the rest doesn't change
#pm1NA = pm1[!is.na(pm1$Sample.Value),]  

date1 = correctDate(pm1NA$Date)
pm1CorrectDate = cbind(pm1NA, date1) # append column for corrected dates
monthCol1 = extractMonths(pm1NA$Date)
monthCol1 <- as.integer(monthCol1)
pm1month = cbind(pm1CorrectDate, monthCol1) # add column for month number
days = format(pm1month$date1, "%d") # extract days
days <- as.integer(days)
pm1days = cbind(pm1month, days) # append days

# We add startingHour as value for the hour

myTime <- factor(pm1days$Start.Time)
hour = format(as.POSIXct(myTime,format="%H:%M"),"%H")
hour <- as.integer(hour)
pm1days <- cbind(pm1days, hour)

dayNumber <- c()
for(i in 1:length(pm1days$Date)){dayNumber[i]<-fromDateToNumber(pm1days$Date[i])}
pm1days <- cbind(pm1days, dayNumber)

lista = split(pm1days, pm1days$monthCol1) # list of dataframes splitted by month

df <- lista[[1]]

dfTest <- sqldf(strwrap(sprintf("select * from df where days >= %d and days <= %d", 15, 21)))
dfTrain <- sqldf(strwrap(sprintf("select * from df where days >= %d and days <= %d", 0, 14)))
dfTrain <- rbind(dfTrain,sqldf(strwrap(sprintf("select * from df where days >= %d and days <= %d", 22, 31))))

for(i in 2 : 9){ # selects every dataframe in the list
  dfTest = rbind(dfTest, selectTrainOrTest(lista[[i]], 15, 21, days))
  dfTrain = rbind(dfTrain, selectTrainOrTest(lista[[i]], 0, 14, days))
  dfTrain = rbind(dfTrain, selectTrainOrTest(lista[[i]], 22, 31, days))
}

library(neuralnet)
dfNames <- names(dfTrain)

dfTrainNorm <- apply(dfTrain[c(4:5,7,8,10,11,13, 32, 33)], 2, norm.fun)
dfTestNorm <- apply(dfTest[c(4:5,7,8,10,11, 13, 32, 33)], 2, norm.fun)

dfTrainNorm <- data.frame(dfTrainNorm)


myNn2 <- neuralnet(Sample.Value ~ County.Code + Site.ID + POC
                   + Sample.Duration + Method  + hour + dayNumber,
                   data = dfTrainNorm, hidden = 2, linear.output = TRUE, err.fct = "sse")

myNn5 <- neuralnet(Sample.Value ~ County.Code + Site.ID + POC
                   + Sample.Duration + Method  + hour + dayNumber,
                   data = dfTrainNorm, hidden = 5, linear.output = TRUE, err.fct = "sse")
myNn10 <- neuralnet(Sample.Value ~ County.Code + Site.ID + POC
                    + Sample.Duration + Method  + hour + dayNumber,
                    data = dfTrainNorm, hidden = 10, linear.output = TRUE, err.fct = "sse")
myNn15 <- neuralnet(Sample.Value ~ County.Code + Site.ID + POC
                    + Sample.Duration + Method  + hour + dayNumber,
                    data = dfTrainNorm, hidden = 15, linear.output = TRUE, err.fct = "sse")
myNn20 <- neuralnet(Sample.Value ~ County.Code + Site.ID + POC
                    + Sample.Duration + Method  + hour + dayNumber,
                    data = dfTrainNorm, hidden = 20, linear.output = TRUE, err.fct = "sse")


save.image("workspaceOHIOnewX5.RData")


myPredict2 <- compute(myNn2, dfTestNorm[, c(1 : 5, 7, 8)])$net.result
myRmse2 <- sqrt(mean(abs(dfTestNorm[, 7] - myPredict2)))

myPredict5 <- compute(myNn5, dfTestNorm[, c(1 : 5, 7, 8)])$net.result
myRmse5 <- sqrt(mean(abs(dfTestNorm[, 7] - myPredict5)))

myPredict10 <- compute(myNn10, dfTestNorm[, c(1 : 5, 7, 8)])$net.result
myRmse10 <- sqrt(mean(abs(dfTestNorm[, 7] - myPredict10)))

myPredict15 <- compute(myNn15, dfTestNorm[, c(1 : 5, 7, 8)])$net.result
myRmse15 <- sqrt(mean(abs(dfTestNorm[, 7] - myPredict15)))

myPredict20 <- compute(myNn20, dfTestNorm[, c(1 : 5, 7, 8)])$net.result
myRmse20 <- sqrt(mean(abs(dfTestNorm[, 7] - myPredict20)))
