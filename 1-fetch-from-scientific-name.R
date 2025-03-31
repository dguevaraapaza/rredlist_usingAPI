library(rredlist)
library(tidyverse)
library(openxlsx)

source("function.R")

#### Get the info ####
# 1 . load the info
data <- read_csv(file.choose(), show_col_types = FALSE)
data <- data$nombre_sinonimo
# run the API and store
iucn1 <- get.search(data, "db-arboles")
iucn2 <- get.search_region(data, "db-arboles2")

# 2. search by country
# peru <- get.search.country("PE")
# # clean bad namess
# removed <- c("Arctocephalus australis Peruvian/Northern Chilean subpopulation")
# peru_clean <- peru[!(peru$scientific_name %in% removed),]

# run the API and store
complete_peru <- get.search(peru_clean$scientific_name, "peru_completo")
