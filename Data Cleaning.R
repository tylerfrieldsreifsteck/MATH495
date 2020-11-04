library(readxl)
library(tidyverse)
library(rvest)
library(formulaic)
library(rlang)

#bring in data we got from various sources.
UN_MigrantStockByOriginAndDestination_2019 <- read_excel("Matches_Ethan_UN_MigrantStockByOriginAndDestination_2019.xlsx", 
                                                         sheet = "Table 1")
annual_number_of_births_by_world_region <- read_csv("annual-number-of-births-by-world-region.csv")
annual_number_of_deaths_by_world_region <- read_csv("annual-number-of-deaths-by-world-region.csv")
world_population_by_world_regions <- read_csv("world-population-by-world-regions-post-1820.csv")



#cleaning the UN Migrant Stock Data 


#bring in data we got from various sources.
UN_MigrantStockByOriginAndDestination_2019 <- read_excel("Matches_Ethan_UN_MigrantStockByOriginAndDestination_2019.xlsx", 
                                                         sheet = "Table 1")
region_countries<-read_csv("countries_by_region.csv")

#going to change the name of a column in the migration data to make it easier
colnames(UN_MigrantStockByOriginAndDestination_2019)[3]<- "region"

#now let's filter out everything that's not a region
UN_MigrantStockByOriginAndDestination_2019<- filter(UN_MigrantStockByOriginAndDestination_2019, 
                                                    region == "Africa" |
                                                      region == "Asia" |
                                                      region == "Europe" |
                                                      region == "Northern America" |
                                                      region == "Oceania" |
                                                      region == "Latin America and the Caribbean" 
                                                    )
#convert country columns to numeric
cols <- names(UN_MigrantStockByOriginAndDestination_2019)[7:ncol(UN_MigrantStockByOriginAndDestination_2019)]
UN_MigrantStockByOriginAndDestination_2019[cols] <- lapply(UN_MigrantStockByOriginAndDestination_2019[cols], as.numeric)

#create northern america countries...
northern_america <- filter(region_countries, Region == "Northern America")
northern_america$Country<-add.backtick(northern_america$Country)
northern_america<- northern_america$Country

northern_america<-paste(northern_america, collapse = " + ")
LHS<- "Northern_America"
#create northern america formula...

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(northern_america))

#can delete ? (this was a check)
UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    Northern_America2 = Bermuda + Canada +  Greenland +
                                                    `Saint Pierre and Miquelon` + `United States of America`)

#It works...let's do the exact same thing for all other regions...
Africa<- filter(region_countries, Region == "Africa", Country != "British Indian Ocean Territory",
                Country != "French Southern Territories", Country != "Bissau")
Africa$Country<- add.backtick(Africa$Country)
Africa<- Africa$Country

Africa<-paste(Africa, collapse = " + ")
LHS<- "Africa"

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(Africa))
#need to fix the weird characters and make sure the names line up but other than that, should be good to go..
#just need to do it for the other regions..

#Asia

Asia<- filter(region_countries, Region == "Asia", Country != "Taiwan", Country !='China, Hong Kong Special Administrative Region',
              Country !='China, Macao Special Administrative Region', Country!= 'Timor')

Asia<- filter(region_countries, Region == "Asia", Country != "Taiwan")

Asia$Country<- add.backtick(Asia$Country)
Asia<- Asia$Country

Asia<-paste(Asia, collapse = " + ")
LHS<- "Asia"

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(Asia))

#Europe
Europe<- filter(region_countries, Region == "Europe", Country != "Aland Islands", Country != "Guernsey", Country != "Jersey", 
                                                      Country != "Sark", Country != "Svalbard and Jan Mayen Islands")

Europe$Country<- add.backtick(Europe$Country)
Europe<- Europe$Country

Europe<-paste(Europe, collapse = " + ")
LHS<- "Europe"

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(Europe))
#Latin America
Latin_America<- filter(region_countries, Region == "Latin America and the Caribbean", Country != "Saint Barthelemy",
                                                                                      Country != "Saint Martin (French Part)",
                                                                                      Country != "Bouvet Island",
                                                                                      Country != "South Georgia and the South Sandwich Islands")

Latin_America$Country<- add.backtick(Latin_America$Country)
Latin_America<- Latin_America$Country

Latin_America<-paste(Latin_America, collapse = " + ")
LHS<- "Latin_America"

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(Latin_America))

#Oceana

Oceania<- filter(region_countries, Region == "Oceania", Country != "Christmas Island",
                                                        Country != "Cocos (Keeling) Islands",
                                                        Country != "Heard Island and McDonald Islands",
                                                        Country != "Norfolk Island",
                                                        Country != "United States Minor Outlying Islands",
                                                        Country != "Pitcairn")

Oceania$Country<- add.backtick(Oceania$Country)
Oceania<- Oceania$Country

Oceania<-paste(Oceania, collapse = " + ")
LHS<- "Oceania"

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(Oceania))

UN_Migrant_Data <- select(UN_MigrantStockByOriginAndDestination_2019, Year, region, Northern_America, Asia, Africa, Europe, Latin_America, Oceania)
names(UN_Migrant_Data)[2] <- "Region"
