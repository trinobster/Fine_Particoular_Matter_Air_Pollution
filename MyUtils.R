# Given the path to a file from working directory, returns the dataframe contained
readFile = function(filePath){
  pm = read.table(filePath, header = F, sep ="|", na.strings = "")
  return(pm)
}

# Returns a vector of name columns that you can use in a dataframe
getNames = function(percorso){
  res = readLines(percorso)
  vectNames = unlist(strsplit(res[1], "[|]"))
  vectNames[1] = substring(vectNames[1], 3)
  
  nameList = vectNames
  nameList = make.names(nameList)
  return(nameList)
}

# Visualizes a few useful informations about dataframe samples given
visualizeSamples = function(pm){
  x = pm$Sample.Value
  print(class(x))
  print(str(x))
  print(summary(x))
  return(x)
}

# Returns a list with the number of missing values in a vector
getMissingValue = function(vector){
  x = summary(vector)
  missing = x["NA's"]
  return(missing)
}

# Given a column of file, returns a vector with months
extractMonths = function(dateCols){
  monthsCol = substr(dateCols,5,6)
  return(monthsCol)
}

# Corrects format dates
correctDate = function(vector){
  date = as.Date(as.character(vector), "%Y%m%d")
  return(date)
}

# Calculate inner fences for a sample vector (both major and minor)
calcInnerFences = function(vector, pm, factor){
  q1 = quantile(vector, na.rm = T)[2]
  q3 = quantile(vector, na.rm = T)[4]
  interquantileRange = (q3 - q1) * factor # factor can be 1.5 or 3
  highBound = interquantileRange + q3
  lowBound = q1 - interquantileRange
  pmOut = subset(pm, Sample.Value < lowBound | Sample.Value > highBound)
  return(pmOut)
}

# The mode is the value that appears most often in a set of data and that's what this
# function returns
Mode <- function(x) {
  xna = x[!is.na(x)] # cleans vector from NA
  ux <- unique(xna)
  ux[which.max(tabulate(match(x, ux)))]
}

# Counts the observations contained in pm
countObs = function(pm){
  obs = c()
  
  for(j in 1 : length(pm)){
    obs[j] = dim(pm[[j]])[1]
  }
  
  return(obs)
}

# Splits a dataframe pm into a list of dataframes sorted by attr.
# Then returns a vector containing the number of observations for every element in the list
analyseAttr = function(pm, attr, attrName, pm0){
  lista = split(pm, attr)
  counter = countObs(lista)
  buildMatrix(counter, lista, attrName, pm0)
}

fun = function(vector, num){
  vector == num
}

buildMatrix = function(counter, splittaggio, attrName, pm0){
  attrNum = which(colnames(pm0) == attrName)
  mat = matrix(nrow = length(counter), ncol = 4, byrow = TRUE)
  
  for(i in 1 : length(splittaggio)){ # for every distinct value
    mat[i, 1] = splittaggio[[i]][1, attrNum] # State.Code or Site.ID or County.Code
    mat[i, 2] = counter[i] # Total number of missing observations for value in column 1
    # we extract from pm0 values which corresponds to NA:
    totalObs = (sapply(pm0[attrNum], fun, mat[i, 1]))
    mat[i, 3] = length(totalObs[totalObs == TRUE]) # Total number of obs. for value in column 1
    mat[i, 4] = ceiling((mat[i, 2] / mat[i, 3]) * 100) # Observation percentage
  }
  colnames(mat) = c(attrName, "NA_obs", "TOT_obs", "Perc")
  return(mat)
}


selectTest = function(df, day0, dayf, dfTest, nameCol){#, counter){
  
  res = sqldf(strwrap(sprintf("select * from df where days >= %d and days < %d", day0, dayf)))
  
  if(dim(res)[1] != 0){ # if the query result is not empty
    dfTest <- rbind(dfTest, res)
  }
  return(dfTest)
}

selectTrain = function(df, day0, dayf,  nameCol){#, counter){
  
  res = sqldf(strwrap(sprintf("select * from df where days >= %d and days <= %d", day0, dayf)))
  
  if(dim(res)[1] != 0){ # if the query result is not empty
    dfTrain <- res
  }
  return(dfTrain)
}

# function for selecting a subset of the dataframe on the basis of intervals of days
# param @day0, @dayf: indicate initial and final days (included) in the interval
selectTrainOrTest = function(df, day0, dayf,  nameCol){
  dfTrainOrTest <- c()
  res = sqldf(strwrap(sprintf("select * from df where '$nameCol' >= %d 
                              and '$nameCol' <= %d", day0, dayf)))
  
  if(dim(res)[1] != 0){ # if the query result is not empty
    dfTrainOrTest <- res
  }
  return(dfTrainOrTest)
}

# test columns with all NA values
na.test <-  function (x) {

  w <- sapply(x, function(x)all(is.na(x)))
  if (any(w)) {
    stop(paste("All NA in columns", paste(which(w), collapse=", ")))
  }
}


# function to transform a date in a day between 1 and 366
# @param dt: date in yyyymmdd format
fromDateToNumber <- function(dt){
  withoutYear <- dt %% 10000
  day <- withoutYear %% 100
  month <- floor(withoutYear / 100)
  numberOfDay <- 0
  for (i in 1:(month-1)) {
    numberOfDay <- numberOfDay + dayInMonths[i]
  }
  numberOfDay <- numberOfDay + day
  if(month == 1){
    return (day)
  }
  return(numberOfDay)
}


# function for values normalization
norm.fun <- function(x){
  (x - min(x))/ (max(x)-min(x))
  }

