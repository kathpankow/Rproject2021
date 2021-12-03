#Custom function to convert .txt files into .csv files
csvConvert <- function(dir){    #"dir" is the directory
  list <- list.files(dir, pattern = "*.txt")
  for(n in 1:length(list)){
    table <- read.table(paste(dir, list[n], sep = "/"), header = TRUE, sep = "")
    write.csv(table, file = paste(dir, "/", substr(list[n],1,nchar(list[n])-4),".csv", sep = ""), row.names = FALSE)
  }
}

#Custom function to compile data from all .csv files in a directory into a single .csv file
csvCompile <- function(dir, outputfile, rmNA = TRUE, warnNA = TRUE){  #"dir" is the directory of interest, "outputfile" is the name of the compiled file you want to make  #"rmNA" is whether you want to keep rows with NA #"warnNA" is whether you want to be warned of NAs
  #List all the .csv files in a directory
  list <- list.files(dir, pattern = "*.csv")
  #Create initial table
  compiled <- read.table(paste(dir, list[1], sep = "/"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  #Add country column
  compiled$country <- rep(substr(dir, start = 8, stop = 8), times = nrow(compiled))
  #Add dayofYear column
  dayofYear <- as.numeric(substr(list[1], start = 8, stop = 10))
  compiled$dayofYear <- rep(dayofYear, times = nrow(compiled))
  #Remove rows with NA if rmNA is set to TRUE
  if(rmNA == TRUE){
    for(n in 1:nrow(compiled)){
      if(anyNA(compiled[n,]) == TRUE){
        compiled <- compiled[-c(n),]
      }
    }
  }else if(warnNA == TRUE){   #warn for NA values if warnNA is set to TRUE
    if(anyNA(table) == TRUE){
      print("Warning: Some rows in compiled data contain NA values.")
    }
  }
  ##COMMENT HERE?
  for(n in 2:length(list)){
    #Read file
    table <- read.table(paste(dir, list[n], sep = "/"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
    #Add country column
    table$country <- rep(substr(dir, start = 8, stop = 8), times = nrow(table))
    #Add dayofYear column
    dayofYear <- as.numeric(substr(list[n], start = 8, stop = 10))
    table$dayofYear <- rep(dayofYear, times = nrow(table))
    #Remove rows with NA if rmNA is set to TRUE
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
    #Compiled data 
    compiled <- rbind(compiled,table)
  }
  #Writing file that contains the compiled data
  write.csv(compiled, file = outputfile, row.names = FALSE)
}

#Custom function to summarize the compiled data set in terms of: 
#number of screens run 
#percent of patients screened that were infected 
#male vs. female patients: percent of infected patients that are male and percent of infected patients that are female
#age distribution of patients: age distribution of all patients
dataSummary <- function(file){     #"file" is the file name
  #Load data 
  data <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  #Number of screens run
  nscreens <- nrow(data)
  #Percent of patients screened that were infected
  nInfected <- 0
  for(n in 1:nrow(data)){
    if(is.element(1, data[n,3:12]) == TRUE){
      nInfected <- nInfected + 1
    }
  }
  percentInfected <- nInfected / nscreens * 100
  #Male vs. female patients
  nMale <- 0
  nFemale <- 0
  for(n in 1:nrow(data)){
    if(is.element(1, data[n,3:12]) == TRUE){
      if(data[n,1] == "male"){
        nMale <- nMale + 1
      }else{
        nFemale <- nFemale + 1
      }
    }
  }
  pMale <- nMale / nInfected * 100
  pFemale <- nFemale / nInfected * 100
  #Age distribution of patients
  ageDist <- summary(data$age)
  #return statement
  returns <- list("Number of screens run:" = nscreens, "Percent infected" = percentInfected, "Percent of infected patients that are male" = pMale, "Percent of infected patients that are female" = pFemale, "Age distribution of all patients" = ageDist)
  return(returns)
}
