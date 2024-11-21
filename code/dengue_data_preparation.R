# June 9, 2022 -- updating functions to compile datasets to be consistent with 
# the forecasting analysis 
library(dplyr) 
library(lubridate)
library(reshape2)
library(forecast)
library(purrr)

# General functions # 
unaccent <- function(text){
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

#### Population Data Functions ####
load_pop_data <- function(country, start_year, end_year) {
  file_name <- paste(country, sep = "_", "pop.csv")
  folder_name <- "data/"
  dat <- read.csv(paste(folder_name, file_name, sep = "/"))
  dat$department <- tolower(unaccent(dat$department))
  dat$pop <- as.numeric(as.character(dat$pop))
  dat$year <- as.numeric(as.character(dat$year))
  
  interp_dat <- list(NULL)
  dept <- unique(dat$department)
  for(i in 1:length(dept)){
    if(dept[i]=="lima province"){
      next()
    }
    dept_dat <- dat %>% filter(department == dept[i]) 
    
    time <- data.frame(year = rep((start_year):end_year, each = 1),
                       department = dept[i],
                       country = country)
    time_dat <- merge(time, dept_dat, all = T)    
    time_dat$pop <- round(as.numeric(approx(time_dat$pop, 
                                            n = nrow(time_dat))$y))
    interp_dat[[i]] <- time_dat
  }
  interp_dat <- suppressWarnings(bind_rows(interp_dat))
  interp_dat$country <- paste(country)
  interp_dat$db_name <- gsub(" ", "_", interp_dat$department)
  
  # Importing the lat-lon data
  state_names <- read.csv(paste(folder_name, "lat_long.csv", sep = "/"), 
                          header = T, na.strings = c(NA, ""), 
                          stringsAsFactors = FALSE, fileEncoding = "latin1")
  
  # extracting only the part of the dataset with the specific country names
  dat2 <- state_names[state_names$country == paste(country), ]
  # joining the datasets
  dat2$department <- tolower(unaccent(dat2$department))
  dat2$db_name <- tolower(unaccent(dat2$db_name))
  
  # merging with the population dataset
  dat3 <- suppressWarnings(left_join(interp_dat, dat2, 
                                     by = c("db_name", "country"))) %>% 
    filter(year >= start_year & year <= end_year) %>% 
    dplyr::select(db_name, year, country, pop, db_name, lat, long) %>%
    arrange(db_name, year) %>% 
    mutate(co_province = paste(country, sep = "_", db_name))
  
  # Adding country order to population data
  if(country == "mx"){
    dat3$country_order <- 1
  } else if(country == 'gt'){
    dat3$country_order <- 2
  } else if(country == 'sv'){
    dat3$country_order <- 3 
  } else if(country == 'dr'){
    dat3$country_order <- 4
  } else if(country == 'pr'){
    dat3$country_order <- 5
  } else if(country == 'bb'){
    dat3$country_order <- 6
  } else if(country == 'cr'){
    dat3$country_order <- 7
  } else if(country == 'pa'){
    dat3$country_order <- 8
  } else if(country == 've'){
    dat3$country_order <- 9
  } else if(country == 'co'){
    dat3$country_order <- 10
  } else if(country == "ec"){
    dat3$country_order <- 11
    dat3$db_name[dat3$db_name == "santo_domingo"] <- "santo_domingo_de_los_tsachilas"
  } else if(country == 'pe'){
    dat3$country_order <- 12 
  } else if(country == 'br'){
    dat3$country_order <- 13
  } else if (country == 'ar'){
    (dat3$country_order <- 14)}
  
  dat3 <- dat3[, c("country", "db_name", "co_province", "year", "pop", 'lat', 'long', 'country_order')]

    return(dat3)
}

get_avg_pop_data <- function(dat) {
  avg_dat <- dat %>%
    group_by(country, co_province, db_name, country_order) %>%
    summarise(avg_pop = mean(pop, na.rm = T))
  avg_dat <- data.frame(avg_dat)
  return(avg_dat)
}

#### Spatial data functions #### 
extract_distance <- function(data) {
  dist_df <- data.frame()
  for (i in 1:dim(data)[1]) {
    temp_df <- slice(data, i)
    dist_df[i, 1] <- temp_df$prov1
    dist_df[i, 2] <- temp_df$prov2
    dist_df[i, 3] <- geosphere::distHaversine(
      p1 = as.numeric(temp_df[,c ('long1', "lat1")]), 
      p2 = as.numeric(temp_df[,c ('long2', "lat2")])
    )
  }
  
  dist_df <- dist_df %>% 
    rename(co_province1 = V1, 
           co_province2 = V2, 
           dist_m = V3
    ) %>% 
    mutate(dist_km = dist_m/1000) %>%
    dplyr::select(-dist_m)
  return(dist_df)
}

get_lat_data <- function(pop_dat){
  pop_dat %>% group_by(country, co_province) %>% 
         summarise(lat = max(lat), 
                   long = max(long))
}

get_paired_coord_data <- function(coord_dat) {
 
  prov1_dat <- unique(coord_dat[,c("country", "co_province", "lat", "long")]) %>% 
    rename(country1 = country, 
           province1 = co_province, 
           lat1 = lat, 
           long1 = long)
  # Province 2 
  prov2_dat <- unique(coord_dat[,c("country", "co_province", "lat", "long")]) %>% 
    rename(country2 = country, 
           province2 = co_province, 
           lat2 = lat, 
           long2 = long)
  
 
  loc_combos <- crossing(prov1 = prov1_dat$province1, 
                         prov2 = prov2_dat$province2) %>%
    filter(prov1 != prov2)
  loc_combos <- data.frame(loc_combos)

  combine_dat <- left_join(loc_combos, prov1_dat, by = c("prov1" = "province1"))
  combine_dat <- left_join(combine_dat, prov2_dat, by = c("prov2" = "province2"))
  

  combine_dat$diff_lat <- combine_dat$lat1 - combine_dat$lat2
  combine_dat$diff_long <- combine_dat$long1 - combine_dat$long2
  return(combine_dat)  
}
