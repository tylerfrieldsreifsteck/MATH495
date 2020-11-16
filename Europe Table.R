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
  
  
  y_vect<-vector(mode = "numeric", length = (start_2-start_1-1))
  for(i in 1:start_2-start_1-1){
    y_vect[i]<- y1 + i*m
  }
  y_vect<- prepend(y_vect, rate1, before = 1)
  y_vect<- append(y_vect, rate2, after = length(y_vect))
  return(y_vect)
  
}

years<- 1990:2019

UN_Migrant_Data<- group_split(UN_Migrant_Data, Region)

africa_table <- UN_Migrant_Data[[1]]
asia_table <- UN_Migrant_Data[[2]]
europe_table <-UN_Migrant_Data[[3]]
LA_table <- UN_Migrant_Data[[4]]
NA_table <- UN_Migrant_Data[[5]]
oceania_table <- UN_Migrant_Data[[6]]

africa_table <- africa_table %>% 
  select(-Northern_America, -Europe, -Asia, -Africa, -Latin_America, -Oceania, -Population)
asia_table <- asia_table %>% 
  select(-Northern_America, -Europe, -Asia, -Africa, -Latin_America, -Oceania, -Population)
europe_table <- europe_table %>% 
  select(-Northern_America, -Europe, -Asia, -Africa, -Latin_America, -Oceania, -Population)
LA_table <- LA_table %>% 
  select(-Northern_America, -Europe, -Asia, -Africa, -Latin_America, -Oceania, -Population)
NA_table <- NA_table %>% 
  select(-Northern_America, -Europe, -Asia, -Africa, -Latin_America, -Oceania, -Population)
oceania_table <- oceania_table %>% 
  select(-Northern_America, -Europe, -Asia, -Africa, -Latin_America, -Oceania, -Population)

#North America to Europe 
na_to_eu <- vector("numeric")
for(i in 2:nrow(europe_table)) {
  vec <- five_to_ones(as.numeric(europe_table[i-1,3]), as.numeric(europe_table[i,3]), 
                      as.numeric(europe_table[i-1,1]), as.numeric(europe_table[i,1]))
  na_to_eu <- append(na_to_eu, vec, after = length(na_to_eu))
}

NA_to_EU <- unique(na_to_eu)

#Asia to Europe 
asia_to_eu <- vector("numeric")
for(i in 2:nrow(europe_table)) {
  vec <- five_to_ones(as.numeric(europe_table[i-1,4]), as.numeric(europe_table[i,4]), 
                      as.numeric(europe_table[i-1,1]), as.numeric(europe_table[i,1]))
  asia_to_eu <- append(asia_to_eu, vec, after = length(asia_to_eu))
}

Asia_to_EU <- unique(asia_to_eu)

#Europe to Europe 
EU_to_EU <- vector("numeric", length = 30)

#Africa to Europe 
africa_to_eu <- vector("numeric")
for(i in 2:nrow(europe_table)) {
  vec <- five_to_ones(as.numeric(europe_table[i-1,5]), as.numeric(europe_table[i,5]), 
                      as.numeric(europe_table[i-1,1]), as.numeric(europe_table[i,1]))
  africa_to_eu <- append(africa_to_eu, vec, after = length(africa_to_eu))
}

Africa_to_EU <- unique(africa_to_eu)

#Latin America to Europe 
la_to_eu <- vector("numeric")
for(i in 2:nrow(europe_table)) {
  vec <- five_to_ones(as.numeric(europe_table[i-1,7]), as.numeric(europe_table[i,7]), 
                      as.numeric(europe_table[i-1,1]), as.numeric(europe_table[i,1]))
  la_to_eu <- append(la_to_eu, vec, after = length(la_to_eu))
}

LA_to_EU <- unique(la_to_eu)

#Oceania to Europe 
oceania_to_eu <- vector("numeric")
for(i in 2:nrow(europe_table)) {
  vec <- five_to_ones(as.numeric(europe_table[i-1,8]), as.numeric(europe_table[i,8]), 
                      as.numeric(europe_table[i-1,1]), as.numeric(europe_table[i,1]))
  oceania_to_eu <- append(oceania_to_eu, vec, after = length(oceania_to_eu))
}

Oceania_to_EU <- unique(oceania_to_eu)

Europe_Table_Complete <- data.frame(Year = years, Region = "Europe", northern_america_rate = NA_to_EU,
                                    asia_rate = Asia_to_EU, 
                                    africa_rate = Africa_to_EU,
                                    europe_rate = EU_to_EU,
                                    latin_america_rate = LA_to_EU, 
                                    oceania_rate = Oceania_to_EU
)
