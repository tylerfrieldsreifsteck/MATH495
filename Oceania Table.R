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

#North America to Oceania 
na_to_oceania <- vector("numeric")
for(i in 2:nrow(oceania_table)) {
  vec <- five_to_ones(as.numeric(oceania_table[i-1,3]), as.numeric(oceania_table[i,3]), 
                      as.numeric(oceania_table[i-1,1]), as.numeric(oceania_table[i,1]))
  na_to_oceania <- append(na_to_oceania, vec, after = length(na_to_oceania))
}

NA_to_Oceania <- unique(na_to_oceania)

#Asia to Oceania 
asia_to_oceania <- vector("numeric")
for(i in 2:nrow(oceania_table)) {
  vec <- five_to_ones(as.numeric(oceania_table[i-1,4]), as.numeric(oceania_table[i,4]), 
                      as.numeric(oceania_table[i-1,1]), as.numeric(oceania_table[i,1]))
  asia_to_oceania <- append(asia_to_oceania, vec, after = length(asia_to_oceania))
}

Asia_to_Oceania <- unique(asia_to_oceania)

#Oceania to Oceania 
Oceania_to_Oceania <- vector("numeric", length = 30)

#Africa to Oceania 
africa_to_oceania <- vector("numeric")
for(i in 2:nrow(oceania_table)) {
  vec <- five_to_ones(as.numeric(oceania_table[i-1,5]), as.numeric(oceania_table[i,5]), 
                      as.numeric(oceania_table[i-1,1]), as.numeric(oceania_table[i,1]))
  africa_to_oceania <- append(africa_to_oceania, vec, after = length(africa_to_oceania))
}

Africa_to_Oceania <- unique(africa_to_oceania)

#Latin America to Oceania 
la_to_oceania <- vector("numeric")
for(i in 2:nrow(oceania_table)) {
  vec <- five_to_ones(as.numeric(oceania_table[i-1,7]), as.numeric(oceania_table[i,7]), 
                      as.numeric(oceania_table[i-1,1]), as.numeric(oceania_table[i,1]))
  la_to_oceania <- append(la_to_oceania, vec, after = length(la_to_oceania))
}

LA_to_Oceania <- unique(la_to_oceania)

#Europe to Oceania 
eu_to_oceania <- vector("numeric")
for(i in 2:nrow(oceania_table)) {
  vec <- five_to_ones(as.numeric(oceania_table[i-1,6]), as.numeric(oceania_table[i,6]), 
                      as.numeric(oceania_table[i-1,1]), as.numeric(oceania_table[i,1]))
  eu_to_oceania <- append(eu_to_oceania, vec, after = length(eu_to_oceania))
}

EU_to_Oceania <- unique(eu_to_oceania)

Oceania_Table_Complete <- data.frame(Year = years, Region = "Oceania", northern_america_rate = NA_to_Oceania,
                                    asia_rate = Asia_to_Oceania, 
                                    africa_rate = Africa_to_Oceania,
                                    europe_rate = EU_to_Oceania,
                                    latin_america_rate = LA_to_Oceania, 
                                    oceania_rate = Oceania_to_Oceania
)
