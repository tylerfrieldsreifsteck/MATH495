library(readxl)
library(tidyverse)
library(rvest)

#bring in data we got from various sources.
UN_MigrantStockByOriginAndDestination_2019 <- read_excel("UN_MigrantStockByOriginAndDestination_2019.xlsx", 
                                                         sheet = "Table 1")
annual_number_of_births_by_world_region <- read_csv("annual-number-of-births-by-world-region.csv")
annual_number_of_deaths_by_world_region <- read_csv("annual-number-of-deaths-by-world-region.csv")
world_population_by_world_regions <- read_csv("world-population-by-world-regions-post-1820.csv")


#Now let's bring in a table of the country and references for data cleaning later.
#Going to do some webscraping for this.

page<- read_html("https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification")

tables<- html_table(page, fill = T)
region_table<-tables[[1]]

#lets see how many regions are in this data.
region_table %>% group_by(Region) %>% summarize(n())
#seven regions in this table... 248 countries.

#let's see how many countries are in the birth rate and death rate data..
deaths_countries<-annual_number_of_deaths_by_world_region %>% group_by(Entity) %>% summarise(n())
#looks like there are 236 rows unique countries here. Need to see which countries aren't matching up here
deaths_countries<-annual_number_of_deaths_by_world_region %>% group_by(Entity) %>% summarise(n())
region_countries<-region_table %>% group_by(Country) %>% summarise(n())

#sadly doesn't look like this table is going to work:(

#going to attempt to do something else here. Let's use one year of data from the deaths table to start create our region table...



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

#bam that was a bit easier than expected. What I'm thinking we do next is change the data layout to make things a bit easier to work with.
#essentially flip the rows and columns of the dataframe here.


