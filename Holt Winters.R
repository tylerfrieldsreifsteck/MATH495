library(tidyverse)
library(stats)
library(forecast)
library(data.table)

#function
five_to_ones<-function(rate1, rate2, start_1, start_2){
  y1<- rate1
  y2<- rate2
  x1<- start_1
  x2<- start_2
  m <- (y2-y1)/(x2-x1)

  
   y_vect<-vector(mode = "numeric", length = start_2-start_1-1)
  for(i in 1:start_2-start_1-1){
    y_vect[i]<- y1 + i*m
}
  y_vect<- prepend(y_vect, rate1, before = 1)
  y_vect<- append(y_vect, rate2, after = length(y_vect))
  return(y_vect)
  
}

five_to_ones(.78, .79, 1990, 1995)

#create vector of years from 1990 to 2019

years<- 1990:2019



#next we need to use this function to fill in the gaps



#first group split the migrant list stuff again
UN_Migrant_Data<- group_split(UN_Migrant_Data, Region)

for(l in 1: length(UN_Migrant_Data)){
  UN_Migrant_Data[[l]][,10:15]<- as.numeric(UN_Migrant_Data[[l]][,10:15])
}

new_data<- vector(mode = "list", length = length(UN_Migrant_Data))

for(m in 1: length(UN_Migrant_Data)){
  new_data[[m]]<- as.data.frame(new_data[[m]])
}



five_year<- vector(mode = "list", length = 6)
for(i in 1:length(UN_Migrant_Data)){
  for(j in 10:ncol(UN_Migrant_Data[[i]])){
    for(k in 1:nrow(UN_Migrant_Data[[i]]-1)){
     vect<-five_to_ones(as.numeric(UN_Migrant_Data[[i]][k, j]), as.numeric(UN_Migrant_Data[[i]][k+1,j]),
                     as.numeric(UN_Migrant_Data[[i]][k, 1]), as.numeric(UN_Migrant_Data[[i]][k+1, 1]))
     five_year[[k]]<- vect
    }
    five_year<- rbindlist(five_year)
    five_year<- distinct(five_year)
    new_data[[i]][,j]<- five_year
  }
  
}



ggplot(regional_stats, aes(x = Year, y = Birth_Rate)) +
  geom_line(aes(color = Region))

ggplot(regional_stats, aes(x = Year, y = Death_Rate)) +
  geom_line(aes(color = Region))

regional_stats_list<- regional_stats %>% group_split(Region)

#find the frequency of the time series object for each region
findfrequency(regional_stats_list[[1]]$Births)
findfrequency(regional_stats_list[[1]]$Deaths) #83

findfrequency(regional_stats_list[[2]]$Births)
findfrequency(regional_stats_list[[2]]$Deaths)

findfrequency(regional_stats_list[[3]]$Births)
findfrequency(regional_stats_list[[3]]$Deaths)

findfrequency(regional_stats_list[[4]]$Births)
findfrequency(regional_stats_list[[4]]$Deaths)

findfrequency(regional_stats_list[[5]]$Births) #37
findfrequency(regional_stats_list[[5]]$Deaths)

findfrequency(regional_stats_list[[6]]$Births) #40
findfrequency(regional_stats_list[[6]]$Deaths) #1 

#create a vector to pull from:
freq_vect_birth<- c(1,1,1,1,37,40)
freq_death_vect<- c(83,1,1,1,1,1)

hw_birth_list<- vector("list", length = length(regional_stats_list))
hw_death_list<- vector("list", length = length(regional_stats_list))
forecast_death_list<- vector("list", length = length(regional_stats_list))
forecast_birth_list<- vector("list", length = length(regional_stats_list))

for(i in 1:length(regional_stats_list)){
  data<- regional_stats_list[[i]]
  
  ts_birth<- ts(data$Birth_Rate, start = c(1950), end = 2019)
  ts_death<- ts(data$Death_Rate, start = 1950, end = 2019)
  
  
  hw_birth<- HoltWinters(ts_birth, beta = T, gamma = F)
  
  hw_death<- HoltWinters(ts_death, beta = T, gamma = F)

  #hw_birth<- ts_birth %>% 
    #auto.arima() 
  #hw_death<- ts_death %>% 
    #auto.arima()
  
  
  hw_birth_list[[i]]<- as.data.frame(forecast(hw_birth))
  
  hw_death_list[[i]]<- as.data.frame(forecast(hw_death))
  
  forecast_death_list[[i]]<- forecast(hw_death, level = 0.89)
  forecast_birth_list[[i]]<- forecast(hw_birth, level = 0.89)
  
}


plot(forecast_death_list[[1]])


migrant_list<-UN_Migrant_Data %>% group_split(Region)
migrant_pred<- vector(mode = "list", length = length(migrant_list))
for(j in 1:length(migrant_list)){
  #do a holt winters for each of the 6 regions inflow for each region coming in
  df<- migrant_list[[i]]
  
  ts_northern_america<- ts(df$northern_america_rate, start = 1, end = 7)
  ts_asia<- ts(df$asia_rate, start = 1, end = 7)
  ts_africa<- ts(df$africa_rate, start = 1, end = 7)
  ts_latin_america<- ts(df$latin_america_rate, start = 1, end = 7)
  ts_europe<- ts(df$europe_rate, start = 1, end = 7)
  ts_oceania<- ts(df$oceania_rate, start = 1, end = 7)
  
  hw_na<- HoltWinters(ts_northern_america, beta = T, gamma = F)
  hw_asia<- HoltWinters(ts_asia, beta = T, gamma = F)
  hw_africa<- HoltWinters(ts_africa, beta = T, gamma = F)
  hw_la<- HoltWinters(ts_latin_america, beta = T, gamma = F)
  hw_europe<- HoltWinters(ts_europe, beta = T, gamma = F)
  hw_oceania<- HoltWinters(ts_oceania, beta = T, gamma = F)
  
  migrant_list[[i]]<- as.data.frame(migrant_list[[i]])
  migrant_list[[i]]$na_pred<- forecast(hw_na, level =0.89)
  migrant_list[[i]]$asia_pred <- forecast(hw_asia, level = 0.89)
  migrant_list[[i]]$africa_pred <- forecast(hw_africa, level = 0.89)
  migrant_list[[i]]$la_pred <- forecast(hw_la, level = 0.89)
  migrant_list[[i]]$europe_pred <- forecast(hw_europe, level = 0.89)
  migrant_list[[i]]$oceania_pred <- forecast(hw_oceania, level = 0.89)
}
