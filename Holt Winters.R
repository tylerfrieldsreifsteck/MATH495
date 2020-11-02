library(tidyverse)
library(stats)
library(forecast)

ggplot(regional_stats, aes(x = Year, y = Birth_Rate)) +
  geom_line(aes(color = Region))

ggplot(regional_stats, aes(x = Year, y = Death_Rate)) +
  geom_line(aes(color = Region))

regional_stats_list<- regional_stats %>% group_split(Region)

hw_birth_list<- vector(type = "list", length = length(regional_stats_list))
hw_death_list<- vector(type = "list", length = length(regional_stats_list))

for(i in 1:length(regional_stats_list)){
  data<- regional_stats_list[[i]]
  
  ts_birth<- ts(data$Birth_Rate, start = 1950)
  ts_death<- ts(data$Death_Rate, start = 1950)
  
  hw_birth<- HoltWinters(x = ts_birth, beta = T)
  hw_death<- HoltWinters(x = ts_death, beta = T)
  
  hw_birth_list[[i]]<- forecast(hw_birth)
  
  hw_death_list[[i]]<- forecast(hw_death)
  
}

hw_birth_list<-rbind_list(hw_birth_list)
hw_death_list<- rbind_list(hw_death_list)



regional_stats<- filter(regional_stats, Region == "Africa")

death_rate_ts<- ts(regional_stats$Death_Rate, start = 1950)

hw<-HoltWinters(x= death_rate_ts)

forecast(hw)

plot(forecast(hw))
