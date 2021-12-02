#load ggplot and cowplot
library(ggplot2)
library(cowplot)

#use source() to load functions defined in supportingFunctions.R
source("supportingFunctions.R")

#compile all data into a single .csv file
csvConvert("countryY")
csvCompile("countryX", outputfile = "Xcompiled.csv")
csvCompile("countryY", outputfile = "Ycompiled.csv")
Xcompiled <- read.csv("Xcompiled.csv", header = TRUE, stringsAsFactors = FALSE)
Ycompiled <- read.csv("Ycompiled.csv", header = TRUE, stringsAsFactors = FALSE)
allData <- rbind(Xcompiled, Ycompiled)
write.table(allData, file = "allCompiled.csv", sep = ",", row.names = FALSE)

###in which country did the outbreak likely begin?
#create empty dataframe for each country (dayofyear vs ninfected)
countryXoutbreak <- data.frame(dayofYear = 120:175, nInfected = rep(NA, times = 176-120))
countryYoutbreak <- data.frame(dayofYear = 120:175, nInfected = rep(NA, times = 176-120))
#fill countryXoutbreak table
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
#fill countryYoutbreak table
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
#create scatterplot for country X
ggplot(data = countryXoutbreak, aes(x = dayofYear, y = nInfected)) +
  geom_point() +
  ggtitle("Country X - Cases vs. Date") +
  xlab("Day of Year") +
  ylab("Number of Cases") +
  theme_classic()
#create scatterplot for country Y
ggplot(data = countryYoutbreak, aes(x = dayofYear, y = nInfected)) +
  geom_point() +
  ggtitle("Country Y - Cases vs. Date") +
  xlab("Day of Year") +
  ylab("Number of Cases") +
  theme_classic()

###if country Y develops a vaccine, would it work in country X?
#make empty dataframe for each country
countryXmarkers <- data.frame(marker = 1:10, count = rep(NA, times = 10))
countryYmarkers <- data.frame(marker = 1:10, count = rep(NA, times = 10))
#marker counts for country X
rowX <- 1
for(i in 3:12){
  total <- sum(Xcompiled[,i])
  countryXmarkers[rowX,2] <- total
  rowX <- rowX + 1
}
#marker counts for country Y
rowY <- 1
for(i in 3:12){
  total <- sum(Ycompiled[,i])
  countryYmarkers[rowY,2] <- total
  rowY <- rowY + 1
}
#bar plot for country X
ggplot(data = countryXmarkers, aes(x = as.factor(marker), y = count)) +
  geom_bar(stat = "identity") +
  xlab("Marker") +
  ylab("Count") +
  ggtitle("Appearances of Each Marker in Country X") +
  theme_classic()
#bar plot for country Y
ggplot(data = countryYmarkers, aes(x = as.factor(marker), y = count)) +
  geom_bar(stat = "identity") +
  xlab("Marker") +
  ylab("Count") +
  ggtitle("Appearances of Each Marker in Country Y") +
  theme_classic()
