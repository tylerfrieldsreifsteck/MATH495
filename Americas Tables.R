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

#Latin America Table
#North America to Latin America  
na_to_la <- vector("numeric")
for(i in 2:nrow(LA_table)) {
  vec <- five_to_ones(as.numeric(LA_table[i-1,3]), as.numeric(LA_table[i,3]), 
                      as.numeric(LA_table[i-1,1]), as.numeric(LA_table[i,1]))
  na_to_la <- append(na_to_la, vec, after = length(na_to_la))
}

NA_to_LA <- unique(na_to_la)

#Asia to Latin America 
asia_to_LA <- vector("numeric")
for(i in 2:nrow(LA_table)) {
  vec <- five_to_ones(as.numeric(LA_table[i-1,4]), as.numeric(LA_table[i,4]), 
                      as.numeric(LA_table[i-1,1]), as.numeric(LA_table[i,1]))
  asia_to_LA <- append(asia_to_LA, vec, after = length(asia_to_LA))
}

Asia_to_LA <- unique(asia_to_LA)

#Africa to Latin America
africa_to_LA <- vector("numeric")
for(i in 2:nrow(LA_table)) {
  vec <- five_to_ones(as.numeric(LA_table[i-1,5]), as.numeric(LA_table[i,5]), 
                      as.numeric(LA_table[i-1,1]), as.numeric(LA_table[i,1]))
  africa_to_LA <- append(africa_to_LA, vec, after = length(africa_to_LA))
}

Africa_to_LA <- unique(africa_to_LA)

#Europe to Latin America 
europe_to_la <- vector("numeric")
for(i in 2:nrow(LA_table)) {
  vec <- five_to_ones(as.numeric(LA_table[i-1,6]), as.numeric(LA_table[i,6]), 
                      as.numeric(LA_table[i-1,1]), as.numeric(LA_table[i,1]))
  europe_to_la <- append(europe_to_la, vec, after = length(europe_to_la))
}

Europe_to_LA <- unique(europe_to_la)

#Latin America to Latin America
LA_to_LA <- vector("numeric", length = 30)

#Oceania to Latin America 
oceania_to_la <- vector("numeric")
for(i in 2:nrow(LA_table)) {
  vec <- five_to_ones(as.numeric(LA_table[i-1,8]), as.numeric(LA_table[i,8]), 
                      as.numeric(LA_table[i-1,1]), as.numeric(LA_table[i,1]))
  oceania_to_la <- append(oceania_to_la, vec, after = length(oceania_to_la))
}

Oceania_to_LA <- unique(oceania_to_la)

LA_Table_Complete <- data.frame(Year = years, Region = "Latin_America", northern_america_rate = NA_to_LA,
                                    asia_rate = Asia_to_LA, 
                                    africa_rate = Africa_to_LA,
                                    europe_rate = Europe_to_LA,
                                    latin_america_rate = LA_to_LA, 
                                    oceania_rate = Oceania_to_LA
)

#North America Table 
#North America to North America 
NA_to_NA <- vector("numeric", length = 30)

#Asia to North America 
asia_to_NA <- vector("numeric")
for(i in 2:nrow(NA_table)) {
  vec <- five_to_ones(as.numeric(NA_table[i-1,4]), as.numeric(NA_table[i,4]), 
                      as.numeric(NA_table[i-1,1]), as.numeric(NA_table[i,1]))
  asia_to_NA <- append(asia_to_NA, vec, after = length(asia_to_NA))
}

Asia_to_NA <- unique(asia_to_NA)

#Africa to North America
africa_to_NA <- vector("numeric")
for(i in 2:nrow(NA_table)) {
  vec <- five_to_ones(as.numeric(NA_table[i-1,5]), as.numeric(NA_table[i,5]), 
                      as.numeric(NA_table[i-1,1]), as.numeric(NA_table[i,1]))
  africa_to_NA <- append(africa_to_NA, vec, after = length(africa_to_NA))
}

Africa_to_NA <- unique(africa_to_NA)

#Europe to Latin America 
europe_to_na <- vector("numeric")
for(i in 2:nrow(NA_table)) {
  vec <- five_to_ones(as.numeric(NA_table[i-1,6]), as.numeric(NA_table[i,6]), 
                      as.numeric(NA_table[i-1,1]), as.numeric(NA_table[i,1]))
  europe_to_na <- append(europe_to_na, vec, after = length(europe_to_na))
}

Europe_to_NA <- unique(europe_to_na)

#Latin America to North America
la_to_na <- vector("numeric")
for(i in 2:nrow(NA_table)) {
  vec <- five_to_ones(as.numeric(NA_table[i-1,7]), as.numeric(NA_table[i,7]), 
                      as.numeric(NA_table[i-1,1]), as.numeric(NA_table[i,1]))
  la_to_na <- append(la_to_na, vec, after = length(la_to_na))
}

LA_to_NA <- unique(la_to_na)

#Oceania to North America 
oceania_to_na <- vector("numeric")
for(i in 2:nrow(NA_table)) {
  vec <- five_to_ones(as.numeric(NA_table[i-1,8]), as.numeric(NA_table[i,8]), 
                      as.numeric(NA_table[i-1,1]), as.numeric(NA_table[i,1]))
  oceania_to_na <- append(oceania_to_na, vec, after = length(oceania_to_na))
}

Oceania_to_NA <- unique(oceania_to_na)

NA_Table_Complete <- data.frame(Year = years, Region = "Northern_America", northern_america_rate = NA_to_NA,
                                asia_rate = Asia_to_NA, 
                                africa_rate = Africa_to_NA,
                                europe_rate = Europe_to_NA,
                                latin_america_rate = LA_to_NA, 
                                oceania_rate = Oceania_to_NA
)
