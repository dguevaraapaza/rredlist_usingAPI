library(rredlist)
library(tidyverse)

#### 1. Load rredlistkey ####
token_key = "get_your_own_LOL"

#### 2. Function ####
get.search <- function(species.list, token.key, name){
  # a. Create the output dataframes
  df.iucn <- data.frame()
  count.iucn <- 0
  df.fails <- ""
  count.fails <- 0
  
  # remove NAs
  species.list <- species.list[!is.na(species.list)]
  
  # remove duplicates and whitespaces
  species.list <- trimws(unique(species.list))
  
  # loop
  for (specie in species.list){
    temp <- rl_search(name = specie, key = token.key)
    if (is.null(temp$result) | class(temp$result) == "list") {
      df.fails <- c(df.fails, specie)
      writeLines(paste("FAIL ON: ", specie))
      count.fails <- count.fails + 1
    } else {
      df.iucn <- bind_rows(df.iucn, temp)
      writeLines(paste("Success on: ", specie))
      count.iucn <- count.iucn + 1
    }
  }
  writeLines(" ")
  writeLines(paste(count.iucn, " retrieved\n", count.fails, " fails.", sep = ""))
  
  # export iucn
  write.csv(df.iucn, paste("output/iucn_", name, ".csv", sep = ""), 
            row.names = FALSE)
  # export review
  write.csv(data.frame(df.fails), paste("output/review_", name, ".csv", sep = ""), 
            row.names = FALSE)
  
  return(list(df.iucn, df.fails))
}

#### Get the info ####

#### Aves ####
# 2 ebird checklist curated (with no duplications) from the Megantoni district in Peru
megantoni_aves <- read_csv("input/aves-megantoni.csv", show_col_types = FALSE)$scientific_names
# run the API
iucn_aves <- get.search(megantoni_aves, token_key, "aves")

#### Aves 2 ####
# Review aves megantoni for previuos works
megantoni_aves2 <- read_csv("input/rev-aves-megantoni.csv", show_col_types = FALSE)$species
# run the API
iucn_aves2 <- get.search(megantoni_aves2, token_key, "aves2")

#### Plants ####
# botanical data curated for previous works
megantoni_plants <- read_csv("input/plants_megantoni.csv", show_col_types = FALSE)$species
# run the API
iucn_plants <- get.search(megantoni_plants, token_key, "plantas")

#### Herpetofauna ####
# Herpetofauna data curated for previous works
megantoni_herpe <- read_csv("input/herpetofauna_megantoni.csv", show_col_types = FALSE)$species
# Run the API
iucn_herpe <- get.search(megantoni_herpe, token_key, "herpe")

#### Mammals ####
# Review mammals megantoni for previuos works
megantoni_mamm <- read_csv("input/mammals_megantoni.csv", show_col_types = FALSE)$species
# run the API
iucn_mamm <- get.search(megantoni_mamm, token_key, "mammals")

#### B10K ####
# Review mammals megantoni for previuos works
b10k <- read_csv("input/B10K_dont_collect.csv", show_col_types = FALSE)$species
# run the API
iucn_b10k <- get.search(b10k, token_key, "b10k")

#### Aves - rerun ####
# Review mammals megantoni for previuos works
megantoni_aves3 <- read_csv("input/reviewed_aves.csv", show_col_types = FALSE)$new_name
# run the API
iucn_aves3 <- get.search(megantoni_aves3, token_key, "aves3")
