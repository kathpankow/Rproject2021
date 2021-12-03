#Load ggplot and cowplot
library(ggplot2)
library(cowplot)

#Use source() to load functions defined in supportingFunctions.R
source("supportingFunctions.R")

#Compile all data into a single .csv file
csvConvert("countryY")
csvCompile("countryX", outputfile = "Xcompiled.csv")
csvCompile("countryY", outputfile = "Ycompiled.csv")
Xcompiled <- read.csv("Xcompiled.csv", header = TRUE, stringsAsFactors = FALSE)
Ycompiled <- read.csv("Ycompiled.csv", header = TRUE, stringsAsFactors = FALSE)
allData <- rbind(Xcompiled, Ycompiled)
write.table(allData, file = "allCompiled.csv", sep = ",", row.names = FALSE)

###In which country did the outbreak likely begin?
#Create empty dataframe for each country (dayofyear vs. ninfected)
countryXoutbreak <- data.frame(dayofYear = 120:175, nInfected = rep(NA, times = 176-120))
countryYoutbreak <- data.frame(dayofYear = 120:175, nInfected = rep(NA, times = 176-120))
#Fill countryXoutbreak table
row <- 1
for(m in 120:175){
  nInfectedX <- 0
  for(n in 1:nrow(Xcompiled)){
    if(Xcompiled[n,14] == m){
      if(is.element(1, Xcompiled[n,3:12]) == TRUE){
        nInfectedX <- nInfectedX + 1
      }
    }
  }
  countryXoutbreak[row,2] <- nInfectedX
  row <- row + 1
}
#Fill countryYoutbreak table
row <- 1
for(m in 120:175){
  nInfectedY <- 0
  for(n in 1:nrow(Ycompiled)){
    if(Ycompiled[n,14] == m){
      if(is.element(1, Ycompiled[n,3:12]) == TRUE){
        nInfectedY <- nInfectedY + 1
      }
    }
  }
  countryYoutbreak[row,2] <- nInfectedY
  row <- row + 1
}
#Create scatter plot for country X. The scatter plot will graph day of year vs. number of infected patients
ggplot(data = countryXoutbreak, aes(x = dayofYear, y = nInfected)) +
  geom_point() +
  ggtitle("Country X - Cases vs. Date") +
  xlab("Day of Year") +
  ylab("Number of Cases") +
  theme_classic()
#Create scatter plot for country Y. The scatter plot will graph day of year vs. number of infected patients
ggplot(data = countryYoutbreak, aes(x = dayofYear, y = nInfected)) +
  geom_point() +
  ggtitle("Country Y - Cases vs. Date") +
  xlab("Day of Year") +
  ylab("Number of Cases") +
  theme_classic()

##The disease outbreak likely began in Country X. This is supported by the scatter plots which show that Country X had many cases early in the year, while country Y had very little cases earlier in the year. 

###If country Y develops a vaccine for the disease, would it work in country X?
#Make empty dataframe for each country
countryXmarkers <- data.frame(marker = 1:10, count = rep(NA, times = 10))
countryYmarkers <- data.frame(marker = 1:10, count = rep(NA, times = 10))
#Marker counts for country X
rowX <- 1
for(i in 3:12){
  total <- sum(Xcompiled[,i])
  countryXmarkers[rowX,2] <- total
  rowX <- rowX + 1
}
#Marker counts for country Y
rowY <- 1
for(i in 3:12){
  total <- sum(Ycompiled[,i])
  countryYmarkers[rowY,2] <- total
  rowY <- rowY + 1
}
#Bar plot for country X. The graph shows the number of times each marker shows up among the patients. 
ggplot(data = countryXmarkers, aes(x = as.factor(marker), y = count)) +
  geom_bar(stat = "identity") +
  xlab("Marker") +
  ylab("Count") +
  ggtitle("Appearances of Each Marker in Country X") +
  theme_classic()
#Bar plot for country Y. The graph shows the number of times each marker shows up among the patients.
ggplot(data = countryYmarkers, aes(x = as.factor(marker), y = count)) +
  geom_bar(stat = "identity") +
  xlab("Marker") +
  ylab("Count") +
  ggtitle("Appearances of Each Marker in Country Y") +
  theme_classic()

##A vaccine for country Y will not work for citizens in country X. 
#This is because the patient's in the two countries have very different markers. 
#Country X citizens mainly display markers 1-5 and very little of 6-10. Country Y citizens display markers 6 and 7 to a great extent as well as markers 8-10. 