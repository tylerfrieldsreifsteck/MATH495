library(tidyverse)
library(stats)
library(forecast)
library(data.table)

##Asia Table

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

#North America to Asia 
na_to_Asia <- vector("numeric")
for(i in 2:nrow(asia_table)) {
  vec <- five_to_ones(as.numeric(asia_table[i-1,3]), as.numeric(asia_table[i,3]), 
                      as.numeric(asia_table[i-1,1]), as.numeric(asia_table[i,1]))
  na_to_Asia <- append(na_to_Asia, vec, after = length(na_to_Asia))
}

NA_to_Asia <- unique(na_to_Asia)

#Asia to Asia
Asia_to_Asia <- vector("numeric", length = 30)

#Africa to Asia
africa_to_Asia <- vector("numeric")
for(i in 2:nrow(asia_table)) {
  vec <- five_to_ones(as.numeric(asia_table[i-1,5]), as.numeric(asia_table[i,5]), 
                      as.numeric(asia_table[i-1,1]), as.numeric(asia_table[i,1]))
  africa_to_Asia <- append(africa_to_Asia, vec, after = length(africa_to_Asia))
}

Africa_to_Asia <- unique(africa_to_Asia)

#Europe to Asia 
europe_to_Asia <- vector("numeric")
for(i in 2:nrow(asia_table)) {
  vec <- five_to_ones(as.numeric(asia_table[i-1,6]), as.numeric(asia_table[i,6]), 
                      as.numeric(asia_table[i-1,1]), as.numeric(asia_table[i,1]))
  europe_to_Asia <- append(europe_to_Asia, vec, after = length(europe_to_Asia))
}

Europe_to_Asia <- unique(europe_to_Asia)

#Latin America to Asia
la_to_Asia <- vector("numeric")
for(i in 2:nrow(asia_table)) {
  vec <- five_to_ones(as.numeric(asia_table[i-1,7]), as.numeric(asia_table[i,7]), 
                      as.numeric(asia_table[i-1,1]), as.numeric(asia_table[i,1]))
  la_to_Asia <- append(la_to_Asia, vec, after = length(la_to_Asia))
}

LA_to_Asia <- unique(la_to_Asia)

#Oceania to Asia
oceania_to_Asia <- vector("numeric")
for(i in 2:nrow(asia_table)) {
  vec <- five_to_ones(as.numeric(asia_table[i-1,8]), as.numeric(asia_table[i,8]), 
                      as.numeric(asia_table[i-1,1]), as.numeric(asia_table[i,1]))
  oceania_to_Asia <- append(oceania_to_Asia, vec, after = length(oceania_to_Asia))
}

Oceania_to_Asia <- unique(oceania_to_Asia)

Asia_Table_Complete <- data.frame(Year = years, Region = "Asia", northern_america_rate = NA_to_Asia,
                                    asia_rate = Asia_to_Asia, 
                                    africa_rate = Africa_to_Asia,
                                    europe_rate = Europe_to_Asia,
                                    latin_america_rate = LA_to_Asia, 
                                    oceania_rate = Oceania_to_Asia
)
