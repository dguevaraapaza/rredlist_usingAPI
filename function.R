# get information based on the scientific name
get.search <- function(species.list, name){
  # load key
  token_key = "4d02b0e18d984c6ebdc189406edc761bb169faca792f65b2b7d91ac61cf90716"
  
  # a. Create the output dataframes
    df.iucn <- data.frame() # final dataframe
    count.total <- 0 # count total
    count.iucn <- 0 # count done
    df.fails <- "" # get the bad ones
    count.fails <- 0 # count fails
    df.common_names <- data.frame()
    # remove NAs
    species.list <- species.list[!is.na(species.list)]
    species.list <- sort(species.list)
    # remove duplicates and whitespaces
    species.list <- trimws(unique(species.list))
  
  # b. loop
  for (specie in species.list){
    count.total <- count.total + 1
    writeLines(paste(count.total, "/", length(species.list), ": ", specie))
    temp <- rl_search(name = specie, key = token_key)
    
    if (is.null(temp$result) | class(temp$result) == "list") {
      df.fails <- c(df.fails, specie)
      writeLines("FAIL_______________________________")
      count.fails <- count.fails + 1
    } else {
      # get only the dataframe
      temp <- temp[[2]]
      # get common names
      cname_temp <- rl_common_names(specie, key = token_key)
      if(class(cname_temp$result) == "data.frame"){
        # check names
        cname_temp <- cname_temp$result %>% 
          filter(language == "spa")
        # Handle many names
        if (nrow(cname_temp) == 0){
          writeLines(paste("-No Spanish common names-"))
        } else if (nrow(cname_temp) > 1){
          cname_temp <- cname_temp %>%
            group_by(language) %>%
            mutate(taxonname = paste0(taxonname, collapse = "; ")) %>%
            select(language, taxonname) %>%
            distinct(language, .keep_all = TRUE)
            # store in other info
            temp <- bind_cols(temp, cname_temp)
        } else {
          # store in other info
          temp <- bind_cols(temp, cname_temp)
        }
        # store iucn info
        df.iucn <- bind_rows(df.iucn, temp)
        # success       
        writeLines("SUCCESS_______________________________")
        count.iucn <- count.iucn + 1
      }
      else{
        writeLines(paste("-No Spanish common names-"))
        # store iucn info
        df.iucn <- bind_rows(df.iucn, temp)
        # success
        writeLines("SUCCESS_______________________________")
        count.iucn <- count.iucn + 1
      }
    }
  }
  writeLines(" ")
  writeLines(paste(count.iucn, " retrieved\n", count.fails, " fails.", sep = ""))
  # export iucn
  write.xlsx(df.iucn, paste("output/iucn_", name, ".xlsx", sep = ""))
  if(length(df.fails) > 0) {
    # export review
    write.csv(data.frame(df.fails), paste("output/review_", name, ".csv", sep = ""),
              row.names = FALSE)
  }
  return(list(df.iucn, df.fails))
}


get.search_region <- function(species.list, name){
  # load key
  token_key = "4d02b0e18d984c6ebdc189406edc761bb169faca792f65b2b7d91ac61cf90716"
  
  # a. Create the output dataframes
  df.iucn <- data.frame() # final dataframe
  count.total <- 0 # count total
  count.iucn <- 0 # count done
  df.fails <- "" # get the bad ones
  count.fails <- 0 # count fails
  df.common_names <- data.frame()
  # remove NAs
  species.list <- species.list[!is.na(species.list)]
  species.list <- sort(species.list)
  # remove duplicates and whitespaces
  species.list <- trimws(unique(species.list))
  
  # b. loop
  for (specie in species.list){
    count.total <- count.total + 1
    writeLines(paste(count.total, "/", length(species.list), ": ", specie))
    temp <- rl_occ_country(name = specie, key = token_key)
    
    if (is.null(temp$result) | class(temp$result) == "list") {
      df.fails <- c(df.fails, specie)
      writeLines("FAIL_______________________________")
      count.fails <- count.fails + 1
    } else {
      # get only the dataframe
      temp <- temp$result %>% 
        mutate(especie = specie)
      # store iucn info
      df.iucn <- bind_rows(df.iucn, temp)
      # success       
      writeLines("SUCCESS_______________________________")
      count.iucn <- count.iucn + 1
    }
  }
  writeLines(" ")
  writeLines(paste(count.iucn, " retrieved\n", count.fails, " fails.", sep = ""))
  # export iucn
  write.xlsx(df.iucn, paste("output/iucn_", name, ".xlsx", sep = ""))
  if(length(df.fails) > 0) {
    # export review
    write.csv(data.frame(df.fails), paste("output/review_", name, ".csv", sep = ""),
              row.names = FALSE)
  }
  return(list(df.iucn, df.fails))
}


get.search.country <- function(country){
  # load key
  token_key = "4d02b0e18d984c6ebdc189406edc761bb169faca792f65b2b7d91ac61cf90716"
  
  # load
  result <- rl_sp_country(country, key = token_key)
  
  return(result$result)
}