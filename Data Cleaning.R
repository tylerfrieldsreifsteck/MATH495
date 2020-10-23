library(readxl)
library(tidyverse)
UN_MigrantStockByOriginAndDestination_2019 <- read_excel("UN_MigrantStockByOriginAndDestination_2019.xlsx", 
                                                         sheet = "Table 1")
annual_number_of_births_by_world_region <- read_csv("annual-number-of-births-by-world-region.csv")
annual_number_of_deaths_by_world_region <- read_csv("annual-number-of-deaths-by-world-region.csv")
world_population_by_world_regions <- read_csv("world-population-by-world-regions-post-1820.csv")
