# Climate data formatting # 

load_and_format_temp_data <- function(){
  tmax <- read.csv('data/country_tmax.csv') %>%
    dplyr::select(-X) 
  tmin <- read.csv('data/country_tmin.csv') %>%
    dplyr::select(-X) 
  
  range <- left_join(tmax, tmin) %>% 
    mutate(trange = tmax - tmin)
  avg_temp <- range %>% group_by(co_province, time) %>% 
    summarise(avg_temp = mean(tmax, tmin, na.rm=T)) 
  temp_data <- left_join(range, avg_temp) %>% 
    dplyr::select(country, -department, db_name, co_province, time, year, month, 
                  tmin, tmax, avg_temp, trange) %>% 
    rename(range = trange)
  # Filtering the data to match the dengue time-series ranges 
  mx_temp_data <- temp_data %>% filter(country == 'mx', year <= 2016) 
  ar_temp_data <- temp_data %>% filter(country == "ar" & 
                                         db_name!= 'cordoba', year <= 2016 & year >= 2010)
  cb_temp_data <- temp_data %>% filter(country == "ar" &
                                         db_name == "cordoba", year <= 2018 & year >= 2009)
  bb_temp_data <- temp_data %>% filter(country == "bb", year <= 2016 & year >= 1999)
  br_temp_data <- temp_data %>% filter(country == "br", year <= 2017 & year >= 2001)
  cr_temp_data <- temp_data %>% filter(country == "cr", year <= 2013 & year >= 2001)
  ec_temp_data <- temp_data %>% filter(country == "ec", year <= 2017 & year >= 1998)
  gt_temp_data <- temp_data %>% filter(country == "gt", year <= 2017 & year >= 2004)
  pr_temp_data <- temp_data %>% filter(country == "pr", year <= 2015 & year >= 1987)
  co_temp_data <- temp_data %>% filter(country == "co", year <= 2015 & year >= 2007)
  sv_temp_data <- temp_data %>% filter(country == "sv", year <= 2017 & year >= 2008)
  pa_temp_data <- temp_data %>% filter(country == "pa", year <= 2016 & year >= 2010)
  pe_temp_data <- temp_data %>% filter(country == "pe", year <= 2017 & year >= 2007) 
  ve_temp_data <- temp_data %>% filter(country == "ve", year <= 2015 & year >= 2001) 
  dr_temp_data <- temp_data %>% filter(country == "dr", year <= 2018 & year >= 1997)
  
  data <- bind_rows(mx_temp_data, ar_temp_data, cb_temp_data, 
                    cr_temp_data, bb_temp_data, br_temp_data, 
                    co_temp_data, ec_temp_data, gt_temp_data, 
                    sv_temp_data, pa_temp_data, pe_temp_data, 
                    pr_temp_data, dr_temp_data, ve_temp_data)
  return(data)
}

get_avg_temp_data <- function(data){
  
  avg_data <- data %>% group_by(co_province, year) %>% 
    summarise(yr_avg = mean(avg_temp, na.rm=T)) %>% 
    group_by(co_province) %>% 
    summarise(avg_temp = mean(yr_avg, na.rm = T))
}

load_and_format_ppt_data <- function(){
  ppt <- read.csv('data/country_ppt.csv') %>%
    dplyr::select(-X) 
  
  mx_ppt_data <- ppt %>% filter(country == 'mx', year <= 2016) 
  ar_ppt_data <- ppt %>% filter(country == "ar" & db_name!= 'cordoba', year <= 2016 & year >= 2010)
  cb_ppt_data <- ppt %>% filter(country == "ar" & db_name == "cordoba", year <= 2018 & year >= 2009)
  bb_ppt_data <- ppt %>% filter(country == "bb", year <= 2016 & year >= 1999)
  br_ppt_data <- ppt %>% filter(country == "br", year <= 2017 & year >= 2001)
  cr_ppt_data <- ppt %>% filter(country == "cr", year <= 2013 & year >= 2001)
  ec_ppt_data <- ppt %>% filter(country == "ec", year <= 2017 & year >= 1998)
  gt_ppt_data <- ppt %>% filter(country == "gt", year <= 2017 & year >= 2004)
  pr_ppt_data <- ppt %>% filter(country == "pr", year <= 2015 & year >= 1987)
  co_ppt_data <- ppt %>% filter(country == "co", year <= 2015 & year >= 2007)
  sv_ppt_data <- ppt %>% filter(country == "sv", year <= 2017 & year >= 2008)
  pa_ppt_data <- ppt %>% filter(country == "pa", year <= 2016 & year >= 2010)
  pe_ppt_data <- ppt %>% filter(country == "pe", year <= 2017 & year >= 2007) 
  ve_ppt_data <- ppt %>% filter(country == "ve", year <= 2015 & year >= 2001) 
  dr_ppt_data <- ppt %>% filter(country == "dr", year <= 2018 & year >= 1997)
  
  data <- bind_rows(mx_ppt_data, ar_ppt_data, cb_ppt_data, 
                    cr_ppt_data, bb_ppt_data, br_ppt_data, 
                    co_ppt_data, ec_ppt_data, gt_ppt_data, 
                    sv_ppt_data, pa_ppt_data, pe_ppt_data, 
                    pr_ppt_data, dr_ppt_data, ve_ppt_data)
  return(data)
}

get_avg_temp_and_range_data <- function(data){
  
  avg_temp <- data %>% group_by(co_province, year) %>% 
    summarise(yr_avg = mean(avg_temp, na.rm = T), 
              yr_range = mean(range, na.rm = T)) %>% 
    group_by(co_province) %>%
    summarise(avg_temp = mean(yr_avg), 
              avg_range = mean(yr_range))
}

get_avg_ppt_data <- function(data){
  
  avg_data <- data %>% group_by(co_province, year) %>% 
    summarise(yr_avg = mean(ppt, na.rm=T)) %>% 
    group_by(co_province) %>% 
    summarise(avg_ppt = mean(yr_avg, na.rm = T))
}

# # Matrices 
# get_temp_matrix <- function(data, co){
#   data <- dplyr::filter(data, country == co)
#   wide_dat <- tidyr::pivot_wider(data, id_cols = time, 
#                                  names_from = co_province, values_from = avg_temp)
#   matrix_dat <- as.matrix(wide_dat[, 1:ncol(wide_dat)])
#   return(matrix_dat)
# }

get_ppt_matrix <- function(data, co){
  data <- data %>% filter(country == co) 
  wide_dat <- tidyr::pivot_wider(data, id_cols = time, 
                                 names_from = co_province, values_from = ln_ppt)
  # turning into matrix
  matrix_dat <- as.matrix(wide_dat[, 1:ncol(wide_dat)])
  return(matrix_dat)
}


# Elevation 
format_elev_data <- function(p){
  dat <- read.csv("data/prov_elevation.csv") %>% 
    mutate(elevation = as.numeric(as.character(elevation)))
  
  # updating labels of provinces 
  dat$co_province[dat$country == "co" & 
                    dat$co_province == "norte de santander"] <- "norte santander"
  dat$co_province[dat$country == "co" & 
                    dat$co_province == "san andres y providencia"] <- "arch san andres-prov-sta catalina"
  dat$co_province[dat$country == "ar" & 
                    dat$co_province == "ciudad de buenos aires"] <- "ciudad buenos aires"
  dat$co_province[dat$country == "pe" & 
                    dat$co_province == "lima co_province"] <- "lima"
  dat$co_province[dat$country == "gt" & 
                    dat$co_province == "peten"] <- "el peten"
  dat$co_province[dat$country == "gt" & 
                    dat$co_province == "quezaltenango"] <- "quetzaltenango"
  dat$co_province[dat$country == "dr" & 
                    dat$co_province == "dominican republic"] <- "all"
  dat$co_province[dat$country == "pr" & 
                    dat$co_province == "puerto rico"] <- "all"
  dat$co_province[dat$country == "bb" & 
                    dat$co_province == "barbados"] <- "all"
  # replacing all empty spaces with _
  dat$co_province <- gsub(" ", "_", dat$co_province)
  # Dropping additional locations that aren't needed 
  drop_names <- c("pe_apurimac", "pe_arequipa", "pe_huancavelica", "pe_tacna",'ar_chubut', "ar_neuquen",
                  "ar_rio_negro", 'ar_san_luis', 'ar_santa_cruz',"ar_tierra_del_fuego")
  
  final_dat <- dat[!(dat$co_province %in% drop_names), ]
  
  return(final_dat)
}

load_enso <- function(){
  enso <- read.delim("data/meiv2.data.txt", 
                     header = F, sep = "", skip = 1, na.strings = "-999.00", 
                     col.names = c('year', "m1", "m2", "m3", "m4", "m5", "m6", 
                                   "m7", "m8", "m9", "m10", "m11", "m12")) 
  enso <- enso[1:45, ]
  enso_long <- melt(enso, id.var = "year", measure.var = colnames(enso)[2:ncol(enso)], 
                    variable.name = "month", value.name = "enso")
  enso_long <- enso_long %>% filter(!is.na(year)) %>% 
    mutate(month = as.numeric(gsub("\\D", "", month)),
           year = as.numeric(as.character(year)),
           time = year + (month - 1)/12, 
           enso = as.numeric(as.character(enso))) %>% 
    dplyr::select(time, enso, -year, -month) %>% 
    filter(time >= 1985 & time < 2020)
  
  return(enso_long)
}
