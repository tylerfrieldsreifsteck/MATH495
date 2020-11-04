library(tidyverse)
library(stats)
library(forecast)

ggplot(regional_stats, aes(x = Year, y = Birth_Rate)) +
  geom_line(aes(color = Region))

ggplot(regional_stats, aes(x = Year, y = Death_Rate)) +
  geom_line(aes(color = Region))

regional_stats_list<- regional_stats %>% group_split(Region)

hw_birth_list<- vector("list", length = length(regional_stats_list))
hw_death_list<- vector("list", length = length(regional_stats_list))
forecast_death_list<- vector("list", length = length(regional_stats_list))
forecast_birth_list<- vector("list", length = length(regional_stats_list))

for(i in 1:length(regional_stats_list)){
  data<- regional_stats_list[[i]]
  
  ts_birth<- ts(data$Birth_Rate, start = c(1950), end = c(2020))
  ts_death<- ts(data$Death_Rate, start = 1950)
  
  hw_birth<- HoltWinters(x = ts_birth, beta = T, gamma = F)
  hw_death<- HoltWinters(x = ts_death, beta = T, gamma = F)
  
  hw_birth_list[[i]]<- as.data.frame(forecast(hw_birth))
  
  hw_death_list[[i]]<- as.data.frame(forecast(hw_death))
  
  forecast_death_list[[i]]<- forecast(hw_death)
  forecast_birth_list[[i]]<- forecast(hw_birth)
  
}


regional_stats_list[[1]]
plot(forecast_birth_list[[1]])
plot(forecast_death_list[[1]])
