library(readxl)
library(tidyverse)
library(rvest)
library(formulaic)
library(rlang)
#bring in data we got from various sources.
UN_MigrantStockByOriginAndDestination_2019 <- read_excel("UN_MigrantStockByOriginAndDestination_2019.xlsx", 
                                                         sheet = "Table 1")
annual_number_of_births_by_world_region <- read_csv("annual-number-of-births-by-world-region.csv")
annual_number_of_deaths_by_world_region <- read_csv("annual-number-of-deaths-by-world-region.csv")
world_population_by_world_regions <- read_csv("world-population-by-world-regions-post-1820.csv")



#going to attempt to do something else here. Let's use one year of data from the deaths table to start create our region table...

region_countries<-read_csv("countries_by_region.csv")

#actually looks like the death table and the births table's regions are going to line up with our other data's regions.
#with that in mind, just for now filter these guys.
annual_number_of_births_by_world_region<- filter(annual_number_of_births_by_world_region, 
                                                    Entity == "Africa" |
                                                      Entity == "Asia" |
                                                      Entity== "Europe" |
                                                      Entity == "Northern America" |
                                                      Entity == "Oceania" |
                                                      Entity == "Latin America and the Caribbean" 
)

annual_number_of_deaths_by_world_region<- filter(annual_number_of_deaths_by_world_region, 
                                                 Entity == "Africa" |
                                                   Entity == "Asia" |
                                                   Entity== "Europe" |
                                                   Entity == "Northern America" |
                                                   Entity == "Oceania" |
                                                   Entity == "Latin America and the Caribbean" 
)



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

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    Northern_America2 = Bermuda + Canada +  Greenland +
                                                    `Saint Pierre and Miquelon` + `United States of America`)
write.csv(UN_MigrantStockByOriginAndDestination_2019, "UN_MIGRANT_STOCK.csv")

#It works...let's do the exact same thing for all other regions...
Africa<- filter(region_countries, Region == "Africa")
Africa$Country<- add.backtick(Africa$Country)
Africa<- Africa$Country

Africa<-paste(Africa, collapse = " + ")
LHS<- "Africa"

UN_MigrantStockByOriginAndDestination_2019<- mutate(UN_MigrantStockByOriginAndDestination_2019,
                                                    !!LHS := !!parse_expr(Africa))
#need to fix the weird characters and make sure the names line up but other than that, should be good to go..
#just need to do it for the other regions..
