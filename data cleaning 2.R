library(tidyverse)
library(rvest)

#Cleaning the Births/Deaths/Population Data

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
#checking that all countries are covered (fixing names)
#countries_with_out_region <- filter(world_population_by_world_regions, 
                                    #Region == "0")

#get only years of interest
annual_number_of_births_by_world_region <- filter(annual_number_of_births_by_world_region, 
                                                  Year >= 1950)
annual_number_of_births_by_world_region <- filter(annual_number_of_births_by_world_region, 
                                                  Year != 2020)

annual_number_of_deaths_by_world_region <- filter(annual_number_of_deaths_by_world_region,
                                                  Year >= 1950)
#get only years of interest (1950-2019) -- UN data only has (1990-2019)
#annual_number_of_births_by_world_region <- filter(annual_number_of_births_by_world_region, 
#Year >= 1990)
annual_number_of_births_by_world_region <- filter(annual_number_of_births_by_world_region, 
                                                  Year != 2020)

#annual_number_of_deaths_by_world_region <- filter(annual_number_of_deaths_by_world_region,
#Year >= 1990)
annual_number_of_deaths_by_world_region <- filter(annual_number_of_deaths_by_world_region,
                                                  Year != 2020)

world_population_by_world_regions <- filter(world_population_by_world_regions,
                                            Year >= 1950)




#changing Total Population column name 
colnames(world_population_by_world_regions)[4] <- "Population"

#sum population by region for each year
world_population_by_world_regions <- world_population_by_world_regions %>% 
  group_by(Region, Year) %>% 
  summarise(Population = sum(Population)) %>% 
  filter(Region != "0")

names(annual_number_of_births_by_world_region)[1] <- "Region"
names(annual_number_of_deaths_by_world_region)[1] <- "Region"

world_pop_5_year <- filter(world_population_by_world_regions, Year == 1990 |
                             Year ==1995 | Year == 2000 | Year ==2005 | Year ==2010 | Year ==2015 |
                             Year == 2019)

#joining tables 
regional_stats <- left_join(world_population_by_world_regions, annual_number_of_births_by_world_region, by = c("Region", "Year"))
regional_stats <- left_join(regional_stats, annual_number_of_deaths_by_world_region, by = c("Region", "Year"))
names(regional_stats)[c(5,7)] <- c("Births", "Deaths")
regional_stats <- mutate(regional_stats, Birth_Rate = (Births*1000)/Population, Death_Rate = (Deaths*1000)/Population)
regional_stats <- select(regional_stats, Region, Year, Population, Births, Deaths, Birth_Rate, Death_Rate)


##Connect UN Migrant data to the world_pop_5_year data....

UN_Migrant_Data<- left_join(UN_Migrant_Data, world_pop_5_year, by = c("Region", "Year"))

#one last thing we need to do here.
UN_Migrant_Data<- mutate(UN_Migrant_Data, Region = ifelse(Region == "Northern America", 
                                                          "Northern_America", ifelse(
                                                          Region == "Latin America and the Caribbean",
                                                          "Latin_America", Region
                                                          )))


#if the region = name of the row,
for(i in 3:ncol(UN_Migrant_Data)){
  for(j in 1:nrow(UN_Migrant_Data)){
    UN_Migrant_Data[j, i] = ifelse(UN_Migrant_Data[j , 2] == names(UN_Migrant_Data[, i]), 0,
                                                                  UN_Migrant_Data[j,i])
  }
}

