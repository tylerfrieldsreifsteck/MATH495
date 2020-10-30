library(readxl)
library(tidyverse)
library(rvest)

annual_number_of_births_by_world_region <- read_csv("annual-number-of-births-by-world-region.csv")
annual_number_of_deaths_by_world_region <- read_csv("annual-number-of-deaths-by-world-region.csv")
world_population_by_world_regions <- read_csv("world-population-by-world-regions-post-1820.csv")
countries_by_region <- read_csv("countries_by_region.csv")

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

#match up countries to region to get population region 
world_population_by_world_regions$Region <- as.character(0)
northern_america <- filter(countries_by_region, Region == "Northern America")
world_population_by_world_regions <- mutate(world_population_by_world_regions, 
                                            Region = ifelse(world_population_by_world_regions$Entity %in% northern_america$Country,
                                                            "Northern America", Region))
latin_america_caribbean <-filter(countries_by_region, Region == "Latin America and the Caribbean")
world_population_by_world_regions <- mutate(world_population_by_world_regions, 
                                            Region = ifelse(world_population_by_world_regions$Entity %in% latin_america_caribbean$Country, 
                                                            "Latin America and the Caribbean", Region))
europe <-filter(countries_by_region, Region == "Europe")
world_population_by_world_regions <- mutate(world_population_by_world_regions, 
                                            Region = ifelse(world_population_by_world_regions$Entity %in% europe$Country, 
                                                            "Europe", Region))
africa <-filter(countries_by_region, Region == "Africa")
world_population_by_world_regions <- mutate(world_population_by_world_regions, 
                                            Region = ifelse(world_population_by_world_regions$Entity %in% africa$Country, 
                                                            "Africa", Region))
asia <-filter(countries_by_region, Region == "Asia")
world_population_by_world_regions <- mutate(world_population_by_world_regions, 
                                            Region = ifelse(world_population_by_world_regions$Entity %in% asia$Country, 
                                                            "Asia", Region))
oceania <-filter(countries_by_region, Region == "Oceania")
world_population_by_world_regions <- mutate(world_population_by_world_regions, 
                                            Region = ifelse(world_population_by_world_regions$Entity %in% oceania$Country, 
                                                            "Oceania", Region))
#checking that all countries are covered 
countries_with_out_region <- filter(world_population_by_world_regions, 
                                    Region == "0")

#get rid of region stats 
world_population_by_world_regions <- filter(world_population_by_world_regions,
                                            Region != "0")

#get only years of interest
annual_number_of_births_by_world_region <- filter(annual_number_of_births_by_world_region, 
                                                  Year >= 1990)

annual_number_of_deaths_by_world_region <- filter(annual_number_of_deaths_by_world_region,
                                                  Year >= 1990)

world_population_by_world_regions <- filter(world_population_by_world_regions,
                                            Year >= 1990)

#sum population by region for each year
#or selecting rows already for Entity 






