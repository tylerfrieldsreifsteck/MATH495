library(readxl)
library(tidyverse)
library(rvest)

annual_number_of_births_by_world_region <- read_csv("annual-number-of-births-by-world-region.csv")
annual_number_of_deaths_by_world_region <- read_csv("annual-number-of-deaths-by-world-region.csv")
world_population_by_world_regions <- read_csv("world-population-by-world-regions-post-1820.csv")

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

#something not right with this not including all regions (N. American and Latin American included out)
world_population_by_world_regions <- filter(world_population_by_world_regions, 
                                            Entity == "Africa" |
                                            Entity == "Asia" |
                                            Entity== "Europe" |
                                            Entity == "Northern America" |
                                            Entity == "Oceania" |
                                            Entity == "Latin America and the Caribbean" 
)

annual_number_of_births_by_world_region <- filter(annual_number_of_births_by_world_region, 
                                                  Year == 1990 |
                                                  Year == 1995 |
                                                  Year == 2000 |
                                                  Year == 2005 |
                                                  Year == 2010 |
                                                  Year == 2015 |
                                                  Year == 2019)

annual_number_of_deaths_by_world_region <- filter(annual_number_of_deaths_by_world_region,
                                                  Year == 1990 |
                                                  Year == 1995 |
                                                  Year == 2000 |
                                                  Year == 2005 |
                                                  Year == 2010 |
                                                  Year == 2015 |
                                                  Year == 2019)

world_population_by_world_regions <- filter(world_population_by_world_regions,
                                            Year == 1990 |
                                            Year == 1995 |
                                            Year == 2000 |
                                            Year == 2005 |
                                            Year == 2010 |
                                            Year == 2015 |
                                            Year == 2019)



