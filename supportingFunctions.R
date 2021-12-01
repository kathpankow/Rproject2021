#convert .txt files with different delimiters into .csv files
csvConvert <- function(dir){ #fileN includes path
  list <- list.files(dir, pattern = "*.txt")
  for(n in 1:length(list)){
    table <- read.table(paste(dir, list[n], sep = "/"), header = TRUE, sep = "")
    write.csv(table, file = paste(dir, "/", substr(list[n],1,nchar(list[n])-4),".csv", sep = ""), row.names = FALSE)
  }
}
csvConvert("countryY")

#compile data from all .csv files in a directory into a single .csv file
csvCompile <- function(dir, outputfile, rmNA = TRUE, warnNA = TRUE){
  #list all .csv files in directory
  list <- list.files(dir, pattern = "*.csv")
  #do file 1 with headers
  table <- read.table(paste(dir, list[1], sep = "/"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  table$country <- rep(substr(dir, start = 7, stop = 7), times = nrow(table))
  dayofYear <- as.numeric(substr(list[1], start = 8, stop = 10))
  table$dayofYear <- rep(dayofYear, times = nrow(table))
  if(rmNA == TRUE){
    for(n in 1:nrow(table)){
      if(anyNA(table[n,]) == TRUE){
        table <- table[-c(n),]
      }
    }
  }else if(warnNA == TRUE){ #warn for NA values if warnNA is set to TRUE
    if(anyNA(table) == TRUE){
      print("Warning: Some rows in compiled data contain NA values.")
    }
  }
  #without headers
  for(n in 2:length(list)){
    #read file
    table <- read.table(paste(dir, list[n], sep = "/"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
    #add country column
    table$country <- rep(substr(dir, start = 7, stop = 7), times = nrow(table))
    #add dayofYear column
    dayofYear <- as.numeric(substr(list[n], start = 8, stop = 10))
    table$dayofYear <- rep(dayofYear, times = nrow(table))
    #remove rows with NA if rmNA is set to TRUE
    if(rmNA == TRUE){
      for(n in 1:nrow(table)){
        if(anyNA(table[n,]) == TRUE){
          table <- table[-c(n),]
        }
      }
    }else if(warnNA == TRUE){ #warn for NA values if warnNA is set to TRUE
      if(anyNA(table) == TRUE){
        print("Warning: Some rows in compiled data contain NA values.")
      }
    }
    #add table to output file (headers are only added once at the beginning)
    if(n == 1){
      write.table(table, file = outputfile, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
    }else{
      write.table(table, file = outputfile, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    }
  }
}
csvCompile("countryX", "test.csv")

#summarize the compiled data set in terms of: 
#number of screens run 
#percent of patients screened that were infected 
#male vs. female patients
#age distribution of patients
dataSummary <- function(file){
  data <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  #number of screens run
  nscreens <- nrow(data)
  #percent of patients screened that were infected
  ninfected <- 0
  for(n in 1:nrow(data)){
    if(is.element(1, data[n,3:12]) == TRUE){
      ninfected <- ninfected + 1
    }
  }
  percentInfected <- ninfected / nscreens * 100
  #male vs. female patients
  nMale <- 0
  nFemale <- 0
  for(n in 1:nrow(data))
    #return statement
    returns <- list("number of screens" = nscreens, "percent infected" = percentInfected)
  return(returns)
}
dataSummary("allData.csv")
