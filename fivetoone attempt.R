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

#North America to Africa 
na_to_Africa <- vector("numeric")
for(i in 2:nrow(africa_table)) {
  vec <- five_to_ones(as.numeric(africa_table[i-1,3]), as.numeric(africa_table[i,3]), 
                               as.numeric(africa_table[i-1,1]), as.numeric(africa_table[i,1]))
  na_to_Africa <- append(na_to_Africa, vec, after = length(na_to_Africa))
}

NA_to_Africa <- unique(na_to_Africa)

#Asia to Africa 
asia_to_Africa <- vector("numeric")
for(i in 2:nrow(africa_table)) {
  vec <- five_to_ones(as.numeric(africa_table[i-1,4]), as.numeric(africa_table[i,4]), 
                      as.numeric(africa_table[i-1,1]), as.numeric(africa_table[i,1]))
  asia_to_Africa <- append(asia_to_Africa, vec, after = length(asia_to_Africa))
}

Asia_to_Africa <- unique(asia_to_Africa)

#Africa to Africa 
Africa_to_Africa <- vector("numeric", length = 30)

#Europe to Africa 
europe_to_Africa <- vector("numeric")
for(i in 2:nrow(africa_table)) {
  vec <- five_to_ones(as.numeric(africa_table[i-1,6]), as.numeric(africa_table[i,6]), 
                      as.numeric(africa_table[i-1,1]), as.numeric(africa_table[i,1]))
  europe_to_Africa <- append(europe_to_Africa, vec, after = length(europe_to_Africa))
}

Europe_to_Africa <- unique(europe_to_Africa)

#Latin America to Africa 
la_to_Africa <- vector("numeric")
for(i in 2:nrow(africa_table)) {
  vec <- five_to_ones(as.numeric(africa_table[i-1,7]), as.numeric(africa_table[i,7]), 
                      as.numeric(africa_table[i-1,1]), as.numeric(africa_table[i,1]))
  la_to_Africa <- append(la_to_Africa, vec, after = length(la_to_Africa))
}

LA_to_Africa <- unique(la_to_Africa)

#Oceania to Africa 
oceania_to_Africa <- vector("numeric")
for(i in 2:nrow(africa_table)) {
  vec <- five_to_ones(as.numeric(africa_table[i-1,8]), as.numeric(africa_table[i,8]), 
                      as.numeric(africa_table[i-1,1]), as.numeric(africa_table[i,1]))
  oceania_to_Africa <- append(oceania_to_Africa, vec, after = length(oceania_to_Africa))
}

Oceania_to_Africa <- unique(oceania_to_Africa)

Africa_Table_Complete <- data.frame(Year = years, Region = "Africa", northern_america_rate = NA_to_Africa,
                                                                     asia_rate = Asia_to_Africa, 
                                                                     africa_rate = Africa_to_Africa,
                                                                     europe_rate = Europe_to_Africa,
                                                                     latin_america_rate = LA_to_Africa, 
                                                                     oceania_rate = Oceania_to_Africa
                                                                     )
