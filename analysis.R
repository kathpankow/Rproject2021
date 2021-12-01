#use source() to load functions defined in supportingFunctions.R

#compile all data into a single .csv file

###in which country did the outbreak likely begin?
#create empty dataframe for each country (dayofyear vs ninfected)
countryXoutbreak <- data.frame(dayofYear = 120:175, nInfected = rep(NA, times = 176-120))
countryYoutbreak <- data.frame(dayofYear = 120:175, nInfected = rep(NA, times = 176-120))
for(n in nrow(data)){
  if(data$country == "X"){
    for(m in 120:175){
      
      if(data$dayofYear == m){
        
      }
    }
  }
}
###if country Y develops a vaccine, would it work in country X?