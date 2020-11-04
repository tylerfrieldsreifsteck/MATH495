library(tidyverse)
library(stats)
library(forecast)


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


plot(forecast_birth_list[[4]])
