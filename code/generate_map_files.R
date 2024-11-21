require(geodata)
require(sf)
require(raster)

countries <- c("MEX", "GTM", "SLV", "PAN",
  "COL", "VEN", "PER", "ECU",
  "BRA", "CRI", "ARG",
  "BRB", "PRI", "DOM", "BLZ", "BOL",
  "CHL", "GUF",
  "GUY", "HND", "NIC",
  "PRY", "URY", "SUR",
  "AIA", "ATG", "VGB",
  "CUB", "CUW", "GRD",
  "HTI", "JAM", "MTQ",
  "DMA", "GLP", "BHS",
  "CYM", "ABW", "BLM",
  "MAF", "KNA", "MSR",
  "LCA", "VCT", "SXM",
  "TTO", "TCA", "VIR",
  "BES")
gadm(country=countries, level=0, resolution=2, path='data/')
gadm(country=countries, level=1, resolution=2, path='data/')

### create shapefile with country borders
country_abbr <- c(
  "mx", "gt", "sv", "pa", "co", "ve", "pe", "ec", "br", "cr", "ar", # for the country boundaries
  "bb", "pr", "dr", # end of study locations
  "bz", "bl",
  "ch", "fg",
  "gy", "hn", "nc",
  "py", "uy", "su",
  "ai", "ag", "bv",
  "cb", "cw", "gr",
  "ht", "ja", "mq",
  "do", "gp", "bh",
  "cy", "ab", "bl",
  "mf", "kn", "ms",
  "lc", "vt", "sx",
  "tt", "tc", "vi",
  "be"
)

country.df <- list()
for (i in 1:length(countries)) {
  country.sv <- readRDS(paste0('data/gadm/gadm41_', countries[i], '_0_pk_low.rds'))
  country.df[[i]] <- as(country.sv, "Spatial")
  country.df[[i]]@data$prov <- "all"
  country.df[[i]]$co_prov <- paste(country_abbr[i], sep = "_", country.df[[i]]$prov)
}

map_country <- do.call(bind, country.df)
shapefile(map_country, filename='data/am_low_res_country.shp')

### create shapefile with province borders
prov <- c(
  "mx", "gt", "sv", "pa", "co", "ve", "pe", "ec", "br", "cr", "ar" # all are in analysis in this row
)

prov_countries <- c(
  "MEX", "GTM", "SLV",
  "PAN", "COL", "VEN",
  "PER", "ECU", "BRA",
  "CRI", "ARG" 
)

prov.df <- list()
for (i in 1:length(prov_countries)) {
  prov.sv <- readRDS(paste0('data/gadm/gadm41_', countries[i], '_1_pk_low.rds'))
  prov.df[[i]] <- as(prov.sv, "Spatial")
  prov.df[[i]]@data$prov <- iconv(tolower(prov.df[[i]]$NAME_1), from = "utf-8", to = "ASCII//TRANSLIT", "")
    prov.df[[i]]$prov <- gsub(" ", "_", tolower(gsub("'", "", prov.df[[i]]$prov, fixed = T)), fixed = T)
    prov.df[[i]]$co_prov <- paste(prov[i], sep = "_", prov.df[[i]]$prov)
}
  
map_adm1 <- do.call(bind, c(prov.df))
  
# writing the shape file
shapefile(map_adm1, filename='data/am_low_res_province_map.shp')

