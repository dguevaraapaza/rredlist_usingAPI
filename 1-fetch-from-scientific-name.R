library(rredlist)
library(tidyverse)

#### 1. Load rredlistkey ####
token_key = "get_your_own_LOL"

#### 2. Function ####
get.search <- function(species.list, token.key){
  # a. Create the output dataframes
  df.iucn <- data.frame()
  count.iucn <- 0
  df.fails <- ""
  count.fails <- 0
  
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
  return(list(df.iucn, df.fails))
}

#### 1. Load info from Megantoni ####
# 2 ebird checklist curated (with no duplications) from the Megantoni district in Peru
megantoni_aves <- read_csv("input/aves-megantoni.csv", show_col_types = FALSE)$scientific_names

# botanical data curated for previous works
megantoni_plants <- read_csv("input/plants_megantoni.csv", show_col_types = FALSE)$species
megantoni_plants <- megantoni_plants[!is.na(megantoni_plants)]

# Herpetofauna data curated for previous works
megantoni_herpe <- read_csv("input/herpetofauna_megantoni.csv", show_col_types = FALSE)$species
megantoni_herpe <- megantoni_herpe[!is.na(megantoni_herpe)]

# Review aves megantoni for previuos works
megantoni_aves2 <- read_csv("input/rev-aves-megantoni.csv", show_col_types = FALSE)$species
megantoni_aves2 <- megantoni_aves2[!is.na(megantoni_aves2)]

# Review mammals megantoni for previuos works
megantoni_mamm <- read_csv("input/mammals_megantoni.csv", show_col_types = FALSE)$species
megantoni_mamm <- megantoni_aves2[!is.na(megantoni_mamm)]

#### 2. Run API ####
# Aves
iucn_aves <- get.search(megantoni_aves, token_key)
# export csv
write.csv(iucn_aves[[1]], "output/iucn_aves.csv", row.names = FALSE)
# export review
write.csv(data.frame(iucn_aves[[2]]), "output/review_aves.csv", row.names = FALSE)

# Plant
iucn_plants <- get.search(megantoni_plants, token_key)
# export csv
write.csv(iucn_plants[[1]], "output/iucn_plants.csv", row.names = FALSE)
# export review
write.csv(data.frame(iucn_plants[[2]]), "output/review_plants.csv", row.names = FALSE)

# Herpetofauna
iucn_herpe <- get.search(megantoni_herpe, token_key)
# export csv
write.csv(iucn_herpe[[1]], "output/iucn_herpe.csv", row.names = FALSE)
# export review
write.csv(data.frame(iucn_herpe[[2]]), "output/review_herpe.csv", row.names = FALSE)

# Aves 2
iucn_aves2 <- get.search(megantoni_aves2, token_key)
# export csv
write.csv(iucn_aves2[[1]], "output/iucn_aves2.csv", row.names = FALSE)
# export review
write.csv(data.frame(iucn_aves2[[2]]), "output/review_aves2.csv", row.names = FALSE)

# mammals
iucn_mamm <- get.search(megantoni_mamm, token_key)
# export csv
write.csv(iucn_mamm[[1]], "output/iucn_mammals.csv", row.names = FALSE)
# export review
write.csv(data.frame(iucn_mamm[[2]]), "output/review_mammals.csv", row.names = FALSE)