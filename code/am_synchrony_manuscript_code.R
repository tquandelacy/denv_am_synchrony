library(dplyr)
library(tidyr)
library(stringr)
library(GGally)
library(mgcv)
library(gtsummary)

# masked base intersect function
intersect <- base::intersect

# Sourced functions
# Functions for data
source("dengue_data_preparation.R")
source("climate_data_preparation.R")
# Functions to run analyses
source("WaveletPackage_functions.R")
source("calculate_country_wavelets.R")
# Functions for figures
source("manuscript_figure_functions.R")

#### Load country case data ####
all_cases <- read.csv("data/compiled_denv_cases.csv",
  header = TRUE, na.strings = c(NA, ""),
  stringsAsFactors = FALSE, fileEncoding = "latin1"
)

all_cases <- mutate(all_cases,
  province = ifelse(str_detect(province, "_all"), "all", province),
  province = str_replace_all(province, "\\s", "_"),
  co_province = paste(country, province, sep = "_"),
  time = case_year + (case_month - 1) / 12,
  ln_cases = log(cases + 1)
)

uniq_prov <- unique(all_cases$co_province)

#### Load population data ####
ar_pop <- load_pop_data(country = "ar", start_year = 2010, end_year = 2016) %>%
  filter(db_name != "cordoba")
cb_pop <- load_pop_data(country = "ar", start_year = 2009, end_year = 2018) %>%
  filter(db_name == "cordoba")
# bb_pop <- load_pop_data(country ='bb', start_year = 1999, end_year = 2016)
br_pop <- load_pop_data(country = "br", start_year = 2001, end_year = 2017)
# cr_pop <- load_pop_data(country ='cr', start_year = 2001, end_year = 2013)
ec_pop <- load_pop_data(country = "ec", start_year = 1998, end_year = 2017)
# gt_pop <- load_pop_data(country ='gt', start_year = 2004, end_year = 2017)
mx_pop <- load_pop_data(country = "mx", start_year = 1985, end_year = 2016)
pr_pop <- load_pop_data(country = "pr", start_year = 1987, end_year = 2015)
co_pop <- load_pop_data(country = "co", start_year = 2007, end_year = 2015)
dr_pop <- load_pop_data(country = "dr", start_year = 1997, end_year = 2018)
# sv_pop <- load_pop_data(country = 'sv', start_year = 2008, end_year = 2017)
# pa_pop <- load_pop_data(country = 'pa', start_year = 2009, end_year = 2016)
pe_pop <- load_pop_data(country = "pe", start_year = 2007, end_year = 2017)
ve_pop <- load_pop_data(country = "ve", start_year = 2001, end_year = 2015)

all_pop <- bind_rows(
  mx_pop, # gt_pop, sv_pop,
  dr_pop, pr_pop, # bb_pop,
  # cr_pop, pa_pop,
  ve_pop,
  co_pop, ec_pop, pe_pop,
  br_pop, ar_pop, cb_pop
)


avg_pop <- get_avg_pop_data(dat = all_pop)

plotting_order_loc <- dplyr::select(all_pop, country, db_name, co_province, lat, long, country_order) %>%
  unique() %>%
  mutate(
    lat_order = dense_rank(lat),
    lat_order = ifelse(lat_order < 10, paste("0", lat_order, sep = ""), as.character(lat_order)),
    province_rank = as.numeric(paste(country_order, lat_order, sep = "."))
  )

#### load spatial data ####
prov_coords <- get_lat_data(all_pop)
# population and spatial coords data set
avg_pop_coord <- left_join(avg_pop, prov_coords, by = c("co_province" = "co_province", "country" = "country"))
# Paired distance datasets
combined_coord <- get_paired_coord_data(coord_dat = all_pop)
prov_dist <- extract_distance(data = combined_coord)

#### Load climate data ####
# Temperature
temp <- load_and_format_temp_data()
avg_temp <- get_avg_temp_and_range_data(data = temp)

# Precipitation
ppt <- load_and_format_ppt_data()
ln_ppt <- load_and_format_ppt_data() %>%
  mutate(ln_ppt = log(ppt + 1))

avg_ppt <- get_avg_ppt_data(ppt)

# Elevation
elev <- format_elev_data()

# ENSO DATA #
enso <- load_enso() %>%
  arrange(time)

#### WAVELET ANALYSES #####
# compile wavelet data #
prov_df <- bind_rows(
  calculate_country_wavelets(all_cases[all_cases$country == "br", ]),
  calculate_country_wavelets(all_cases[all_cases$country == "mx", ]),
  # calculate_country_wavelets(all_cases[all_cases$country == 'gt', ]),
  calculate_country_wavelets(all_cases[all_cases$country == "ve", ]),
  calculate_country_wavelets(all_cases[all_cases$country == "co", ]),
  calculate_country_wavelets(all_cases[all_cases$country == "pe", ]),
  calculate_country_wavelets(all_cases[all_cases$country == "pr", ]),
  calculate_country_wavelets(all_cases[all_cases$country == "dr", ]),
  #  calculate_country_wavelets(all_cases[all_cases$country == 'sv', ]),
  #  calculate_country_wavelets(all_cases[all_cases$country == 'pa', ]),
  #  calculate_country_wavelets(all_cases[all_cases$country == 'bb', ]),
  calculate_country_wavelets(all_cases[all_cases$country == "ec", ]),
  #  calculate_country_wavelets(all_cases[all_cases$country == 'cr', ]),
  calculate_country_wavelets(all_cases[all_cases$co_province == "ar_cordoba", ]),
  calculate_country_wavelets(all_cases[all_cases$country == "ar" & all_cases$province != "cordoba", ])
)

#### W1: Extracting seasonal and multiannual wavelets ####
mlt_countries <- c("mx", "dr", "pr", "ve", "ec", "pe", "br")
caribbean_countries <- c("dr", "pr")

prov_df$ann_wave <- list(NULL)
prov_df$mlt_wave <- list(NULL)
for (i in 1:dim(prov_df)[1]) {
  prov_df$ann_wave[[i]] <- extract_wavelet_coi(
    wave = prov_df$wave[[i]],
    scale = prov_df$scale[[i]],
    coi = prov_df$coi[[i]],
    time = prov_df$time[[i]],
    low = 8,
    high = 16
  )

  prov_df$mlt_wave[[i]] <- extract_wavelet_coi(
    wave = prov_df$wave[[i]], scale = prov_df$scale[[i]],
    coi = prov_df$coi[[i]], time = prov_df$time[[i]],
    low = 16
  )
}

ann_wavelet <- (prov_df) %>%
  dplyr::select(country, province, ann_wave) %>%
  tidyr::unnest(cols = c(ann_wave))

ann_wavelet <- left_join(ann_wavelet, prov_coords,
  by = c("country" = "country", "province" = "co_province")
)

# Including only countries with over 10+ years of data
mlt_wavelet <- (prov_df) %>%
  filter(country %in% mlt_countries) %>%
  dplyr::select(country, province, mlt_wave) %>%
  tidyr::unnest(cols = c(mlt_wave))

mlt_wavelet <- left_join(mlt_wavelet, prov_coords,
  by = c("province" = "co_province")
)

#### Figure 1: Heat maps ####
#png(paste("fig1_", Sys.Date(), ".png", sep = ""),
#    width = 30, height = 37, unit = 'in', res = 600)
plot_fig1(prov_df, plotting_order_loc, mlt_countries)
#dev.off()

#### Figure 2: Seasonal patterns ####
# Estimating the average pattern
avg_seas <- ann_wavelet %>%
  mutate(
    year = as.numeric(sub("\\..*", "", time)),
    month = round((((time - year) * 12) + 1), 0)
  ) %>%
  group_by(country, province, month) %>%
  summarize(mean_wave = mean(wavelet), .groups='drop')

# Estimating the month of the peak
avg_peak <- avg_seas %>%
  group_by(country, province) %>%
  filter(mean_wave == max(mean_wave)) %>%
  dplyr::select(country, province, mean_wave, month) %>%
  rename(co_province = province)


# Figure 2A: Map
#png(paste("fig2a_", Sys.Date(), ".png", sep = ""),
#    width = 20, height = 30, unit = 'in', res = 600)
plot_fig2a(data = avg_peak, prov_coords = prov_coords, caribbean_countries) # panel 1
#dev.off()

# Figure 2B
# png(paste("fig2b_", Sys.Date(), ".png", sep = ""),
#    width = 25, height = 25, unit = 'in', res = 600)
plot_fig2b(seas_data = avg_seas, loc_data = plotting_order_loc) # panel 2
# dev.off()

#### Dengue Spectra analysis ####
# SPECTRA EXTRACTION #
prov_ts <- (prov_df) %>%
  select(country, province, time, Ts) %>%
  tidyr::unnest(cols = c(time, Ts))

locations <- unique(prov_ts$province)
alphas <- tibble()
for (i in 1:length(locations)) {
  this_ts <- filter(prov_ts, province == locations[i])$Ts
  this_ts <- normalizeSeries(this_ts)
  this_ar1 <- ar1(this_ts)
  alphas <- bind_rows(
    alphas,
    tibble(
      province = locations[i],
      alpha = this_ar1$ar,
      var = this_ar1$var,
      n = length(this_ts)
    )
  )
}

alpha_df <- left_join(
  alphas,
  select(prov_ts, country, province) %>% filter(!duplicated(.))
)

alpha_country <- group_by(alpha_df, country) %>%
  summarize(
    median = median(alpha, na.rm = T),
    mean = mean(alpha, na.rm = T),
    min = min(alpha, na.rm = T),
    max = max(alpha, na.rm = T),
    median_var = median(var, na.rm = T),
    max_n = max(n),
    n_prov = n()
  )


# Estimates global spectra
global_cwts <- tibble()
for (i in 1:length(locations)) {
  this_ts <- filter(prov_ts, province == locations[i])$Ts
  this_cwt <- completeCWT(data.frame(t = 1:length(this_ts), x = this_ts), dt = 1, dj = 1 / 4, sigType = "red")
  global_cwts <- bind_rows(
    global_cwts,
    tibble(
      country = unique(prov_ts$country[prov_ts$province == locations[i]]),
      location = locations[i],
      period = this_cwt$periods,
      scales = this_cwt$scales,
      power = this_cwt$global,
      sig_red = this_cwt$globalSig$upper
    )
  )
}

global_sig_country <- tibble()
for (i in 1:nrow(alpha_country)) {
  this_spec <- alpha_powerChiSquare(
    n = alpha_country$max_n[i],
    alpha = alpha_country$median[i]
  )
  this_spec_white <- alpha_powerChiSquare(n = alpha_country$max_n[i], alpha = 0)
  global_sig_country <- bind_rows(
    global_sig_country,
    tibble(
      country = alpha_country$country[i],
      period = this_spec$period,
      sig_red = this_spec$upper,
      sig_white = this_spec_white$upper
    )
  )
}

chisq_spec <- tibble()
for (i in 1:length(locations)) {
  this_location <- filter(global_cwts, location == locations[i])
  this_country <- unique(this_location$country)
  this_chisq <- get_signif_power(
    exp_power = filter(global_sig_country, country == this_country),
    obs_power = filter(this_location)$power,
    i
  )
  chisq_spec <- bind_rows(
    chisq_spec,
    tibble(this_chisq,
      location = locations[i]
    )
  )
}

#### COHERENCE EXTRACTION ####
prov_combos <- crossing(
  ref_prov = prov_df$province,
  other_prov = prov_df$province
) %>%
  filter(ref_prov != other_prov)

uniq_prov_pairs <- data.frame(t(combn(unique(prov_combos$ref_prov), 2))) %>%
  rename(
    ref_prov = X1,
    other_prov = X2
  )

# Setting up lists
prov_combos$coherence <- list(NULL)
prov_combos$phase <- list(NULL)
prov_combos$scales <- list(NULL)
prov_combos$coi <- list(NULL)
prov_combos$times <- list(NULL)

for (i in 1:dim(prov_combos)[1]) {
  ref_prov <- filter(prov_df, province == prov_combos$ref_prov[i])
  other_prov <- filter(prov_df, province == prov_combos$other_prov[i])
  match_obj <- match_transforms(
    wave1 = ref_prov$wave[[1]], wave2 = other_prov$wave[[1]],
    time1 = ref_prov$time[[1]], time2 = other_prov$time[[1]],
    scale1 = ref_prov$scale[[1]], scale2 = other_prov$scale[[1]]
  )

  prov_combos$scales[[i]] <- match_obj$scales
  prov_combos$coi[[i]] <- get_coi(n = length(match_obj$times), dt = 1)
  prov_combos$times[[i]] <- match_obj$times
  # Coherence
  prov_combos$coherence[[i]] <- coh(
    cwt1 = match_obj$wave1, # Ref prov
    cwt2 = match_obj$wave2, # other prov
    scales = match_obj$scales,
    dt = 1,
    dj = 1 / 4
  )
  prov_combos$phase[[i]] <- phase_updated(
    cwt1 = match_obj$wave1, cwt2 = match_obj$wave2,
    scales = match_obj$scales,
    times = match_obj$times,
    dt = 1,
    dj = 1 / 4
  )
}

#### GLOBAL COHERENCE ####
# Simulated global coherence #
sim_gcoh <- coherence_ar1_mc(
  n = 384,
  alpha = 0.796741,
  var = 0.3256995,
  dt = 1, dj = 1 / 4,
  s0 = 2,
  normalize = T,
  mcSim = 1000,
  sigLvl = .95
)

#  Estimate observed global coherence #
uniq_combos <- left_join(uniq_prov_pairs, prov_combos)
gcoh_combos <- list(NULL)
gcoh_combos$est_gcoh <- list(NULL)
gcoh_combos$signif_gcoh <- list(NULL)

for (i in 1:nrow(uniq_combos)) {
  gcoh_combos$est_gcoh[[i]] <- get_obs_global_coh(
    coh_list = uniq_combos$coherence[[i]],
    scale = uniq_combos$scales[[i]],
    coi = uniq_combos$coi[[i]]
  )
  gcoh_combos$signif_gcoh[[i]] <- get_signif_global_coh(
    exp_coh = sim_gcoh,
    obs_coh = gcoh_combos$est_gcoh[[i]], i
  )
}
signif_gcoh <- bind_rows(gcoh_combos$signif_gcoh)


#### Observed average power for seasonal and multiannual cycles ####
# getting avg ann and mlt spectra
avg_power <- tibble()
locations <- unique(prov_ts$province)
for (i in 1:length(locations)) {
  this_df <- filter(global_cwts, location == locations[i])
  avg_power <- bind_rows(
    avg_power,
    tibble(
      country = this_df$country,
      location = locations[i],
      ann_power = get_prov_avg_spectra(this_df, low = 8, high = 16)$power,
      mlt_power = get_prov_avg_spectra(this_df, low = 17)$power
    )
  )
}
avg_power <- bind_rows(unique(avg_power))

####  Factors associated with average power (Regression analyses) ####
prov_ann_spectra <- avg_power %>%
  dplyr::select(country, location, ann_power) %>%
  tidyr::unnest(cols = c(ann_power))

# loading the data
ann_inc <- all_cases %>%
  group_by(co_province, case_year) %>%
  summarise(
    ann_cases = sum(cases, na.rm = T),
    no_months = length(case_month),
    .groups = 'drop'
  ) %>%
  left_join(all_pop, by = c("co_province" = "co_province", "case_year" = "year")) %>%
  mutate(ann_inc = ann_cases / pop)

avg_ann_inc <- ann_inc %>%
  group_by(co_province) %>%
  summarise(
    avg_ann_inc = mean(ann_inc, na.rm = T),
    ts_length = sum(no_months)
  ) %>%
  mutate(log_inc = ifelse(avg_ann_inc == 0, log(avg_ann_inc + 1), log(avg_ann_inc)))

ann_power_covs <- prov_ann_spectra %>%
  left_join(avg_ann_inc, by = c("location" = "co_province")) %>%
  left_join(avg_pop, by = c("location" = "co_province", "country" = "country")) %>%
  left_join(avg_temp, by = c("location" = "co_province")) %>%
  left_join(avg_ppt, by = c("location" = "co_province")) %>%
  left_join(prov_coords, by = c("location" = "co_province", "country" = "country")) %>%
  left_join(elev, by = c("location" = "co_province", "country" = "country")) %>%
  mutate(
    abs_lat = abs(lat),
    log_pop = log(avg_pop),
    log_range = log(avg_range),
    log_ppt = log(avg_ppt),
    log_power = log(ann_power),
    log_elevation = ifelse(elevation < 0, log(0.0001), log(elevation + 1))
  ) %>%
  rename(power = ann_power) %>%
  filter(!is.na(log_power) & !is.na(avg_temp) & !is.na(avg_ppt) & !is.na(log_elevation))

# Getting the model results to obtain the coefs, 95% CI, AIC and model R2
var_names <- c(
  "log_inc", "ts_length", "log_pop", "abs_lat",
  "avg_temp", "avg_range", "log_ppt", "log_elevation"
)
ann_ulm_aic <- list()
ann_aic <- data.frame(model = var_names, coef = rep(NA, 8), coef_l95 = rep(NA, 8), coef_u95 = rep(NA, 8), r2 = rep(NA, 8), aic = rep(NA, 8))
for (i in 1:length(var_names)) {
  var_df <- ann_power_covs %>%
    select(log_power, var_names[i])
  m1 <- lm(log_power ~ .,
    data = var_df
  )
  ann_ulm_aic[[i]] <- m1
  ann_aic$coef[i] <- round(summary(m1)$coef[2], 3)
  ann_aic$coef_l95[i] <- round(summary(m1)$coef[2] - 1.96 * sqrt(vcov(m1)[2, 2]), 3)
  ann_aic$coef_u95[i] <- round(summary(m1)$coef[2] + 1.96 * sqrt(vcov(m1)[2, 2]), 3)
  ann_aic$r2[i] <- round(summary(m1)$r.squared, 2)
  ann_aic$aic[i] <- round(AIC(m1))
}


# looking at a fully saturated model - no spline
tbl_ann_mlm <- lm(
  log_power ~ log_inc + ts_length + log_pop + abs_lat + avg_temp + avg_range +
    log_ppt + log_elevation,
  data = ann_power_covs
) %>%
  tbl_regression(
    exponentiate = FALSE,
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_glance_table(
    label = list(sigma ~ "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )

# merging
tbl_ann_merge <- tbl_merge(
  tbls = list(tbl_ann_mlm),
  tab_spanner = c("Annual\nSaturated model")
)

tbl_ann_merge

#### Saturated model including
# Non-linear term for latitude
ann_mlm4 <- gam(
  log_power ~ log_inc + ts_length + log_pop + s(abs_lat) + avg_temp + avg_range +
    log_ppt + log_elevation,
  data = ann_power_covs
)
coef_ann_mlm4 <- coef(ann_mlm4)
se_ann_mlm4 <- sqrt(diag(vcov(ann_mlm4)))
summary(ann_mlm4)
round(AIC(ann_mlm4))
# lb95
l95_ann_mlm4 <- round(coef_ann_mlm4 - 1 * qnorm(0.975) * se_ann_mlm4, 3)
# ub95
u95_ann_mlm4 <- round(coef_ann_mlm4 + 1 * qnorm(0.975) * se_ann_mlm4, 3)

# Final model 1 -  Reducing the model
ann_mlm5 <- gam(log_power ~ log_inc + log_pop + s(abs_lat),
  data = ann_power_covs
)
summary(ann_mlm5)

# getting the coef and 95CIs
coef_ann_mlm5 <- round(coef(ann_mlm5), 3)
se_ann_mlm5 <- sqrt(diag(vcov(ann_mlm5)))
# lb95
l95_ann_mlm5 <- round(coef_ann_mlm5 - 1 * qnorm(0.975) * se_ann_mlm5, 3)
# ub95
u95_ann_mlm5 <- round(coef_ann_mlm5 + 1 * qnorm(0.975) * se_ann_mlm5, 3)

# Final model 2 - Looking at lat + only the climate variables
ann_mlm6 <- gam(log_power ~ log_pop + s(abs_lat) + avg_temp + avg_range,
  data = ann_power_covs
)
summary(ann_mlm6)

# getting the coef and 95CIs
coef_ann_mlm6 <- round(coef(ann_mlm6), 3)
se_ann_mlm6 <- sqrt(diag(vcov(ann_mlm6)))
# lb95
l95_ann_mlm6 <- round(coef_ann_mlm6 - 1 * qnorm(0.975) * se_ann_mlm6, 3)
# ub95
u95_ann_mlm6 <- round(coef_ann_mlm6 + 1 * qnorm(0.975) * se_ann_mlm6, 3)

# multiannual power analysis
prov_mlt_spectra <- avg_power %>%
  dplyr::select(country, location, mlt_power) %>% 
  tidyr::unnest(cols = c(mlt_power)) 

mlt_power_covs <- prov_mlt_spectra %>%
  filter(country %in% mlt_countries) %>%
  left_join(avg_ann_inc, by = c("location" = "co_province")) %>%
  left_join(avg_pop, by = c("location" = "co_province", "country" = "country")) %>%
  left_join(avg_temp, by = c("location" = "co_province")) %>%
  left_join(avg_ppt, by = c("location" = "co_province")) %>%
  left_join(prov_coords, by = c("location" = "co_province", "country" = "country")) %>%
  left_join(elev, by = c("location" = "co_province", "country" = "country")) %>%
  mutate(
    abs_lat = abs(lat),
    log_pop = log(avg_pop),
    log_range = log(avg_range),
    log_ppt = log(avg_ppt),
    log_power = log(mlt_power),
    log_elevation = ifelse(elevation < 0, log(0.0001), log(elevation + 1))
  ) %>%
  rename(power = mlt_power) %>%
  filter(!is.na(log_power) &
    !is.na(avg_temp) & !is.na(avg_ppt) & !is.na(elevation))


# MLT power : Univariate regression
# Getting the model results to obtain the AIC and model R2
var_names <- c(
  "log_inc", "ts_length", "log_pop", "abs_lat",
  "avg_temp", "avg_range", "log_ppt", "log_elevation"
)
mlt_ulm_aic <- list()
mlt_aic <- data.frame(
  model = var_names, coef = rep(NA, 8), coef_l95 = rep(NA, 8),
  coef_u95 = rep(NA, 8), r2 = rep(NA, 8), aic = rep(NA, 8)
)
for (i in 1:length(var_names)) {
  var_df <- mlt_power_covs %>%
    select(log_power, var_names[i])
  m1 <- lm(log_power ~ .,
    data = var_df
  )
  mlt_ulm_aic[[i]] <- m1
  mlt_aic$coef[i] <- round(summary(m1)$coef[2], 3)
  mlt_aic$coef_l95[i] <- round(summary(m1)$coef[2] - 1.96 * sqrt(vcov(m1)[2, 2]), 3)
  mlt_aic$coef_u95[i] <- round(summary(m1)$coef[2] + 1.96 * sqrt(vcov(m1)[2, 2]), 3)
  mlt_aic$r2[i] <- round(summary(m1)$r.squared, 2)
  mlt_aic$aic[i] <- round(AIC(m1))
}

# Saturated model including latitude as a spline - in manuscript
# Non-linear term for latitude
mlt_mlm3 <- gam(
  log_power ~ log_inc + ts_length + log_pop + s(abs_lat) + avg_temp + avg_range +
    log_ppt + log_elevation,
  data = mlt_power_covs
)
summary(mlt_mlm3)

coef_mlt_mlm3 <- round(coef(mlt_mlm3), 3)
se_mlt_mlm3 <- sqrt(diag(vcov(mlt_mlm3)))
# lb95
l95_mlt_mlm3 <- round(coef_mlt_mlm3 - 1 * qnorm(0.975) * se_mlt_mlm3, 3)
# ub95
u95_mlt_mlm3 <- round(coef_mlt_mlm3 + 1 * qnorm(0.975) * se_mlt_mlm3, 3)

# Final model 1:  incidence and spline for latitude - in manuscript
mlt_mlm4 <- gam(log_power ~ log_inc + s(abs_lat),
  data = mlt_power_covs
)
summary(mlt_mlm4)

coef_mlt_mlm4 <- round(coef(mlt_mlm4), 3)
se_mlt_mlm4 <- sqrt(diag(vcov(mlt_mlm4)))
# lb95
l95_mlt_mlm4 <- round(coef_mlt_mlm4 - 1 * qnorm(0.975) * se_mlt_mlm4, 3)
# ub95
u95_mlt_mlm4 <- round(coef_mlt_mlm4 + 1 * qnorm(0.975) * se_mlt_mlm4, 3)

# Final model 2: temperature and temperature rnage
mlt_mlm5 <- gam(log_power ~ avg_temp + avg_range,
  data = mlt_power_covs
)
summary(mlt_mlm5)

coef_mlt_mlm5 <- round(coef(mlt_mlm5), 3)
se_mlt_mlm5 <- sqrt(diag(vcov(mlt_mlm5)))
# lb95
l95_mlt_mlm5 <- round(coef_mlt_mlm5 - 1 * qnorm(0.975) * se_mlt_mlm5, 3)
# ub95
u95_mlt_mlm5 <- round(coef_mlt_mlm5 + 1 * qnorm(0.975) * se_mlt_mlm5, 3)

#### Extracting coh/phase over time and average coh/phase ####
# Extracting the annual coherence over time of dengue cycles comparing location-pairs #
ann_coh <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  ann_coh[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = prov_combos$coherence[[i]],
    time = prov_combos$times[[i]],
    scales = prov_combos$scales[[i]],
    coi = prov_combos$coi[[i]],
    low = 8, high = 16
  ))

  ann_coh[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  ann_coh[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
ann_coh <- bind_rows(ann_coh)
uniq_ann_coh <- suppressWarnings(left_join(uniq_prov_pairs, ann_coh,
  by = c("ref_prov", "other_prov")
))

# Extracting average annual coherence of dengue cycles between location-pairs
avg_ann_coh <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  avg_ann_coh[[i]] <- extract_avg_coh_phase_coi_updated(prov_combos$coherence[[i]],
    prov_combos$scales[[i]],
    prov_combos$coi[[i]],
    low = 8, high = 16
  )
  avg_ann_coh[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  avg_ann_coh[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
avg_ann_coh <- bind_rows(avg_ann_coh)

# unique pairs
uniq_avg_ann_coh <- suppressWarnings(left_join(uniq_prov_pairs, avg_ann_coh,
  by = c("ref_prov", "other_prov")
))

# Extracting the annual phase difference over time of dengue cycles comparing location-pairs
ann_phase <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  ann_phase[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = prov_combos$phase[[i]],
    time = prov_combos$times[[i]],
    scales = prov_combos$scales[[i]],
    coi = prov_combos$coi[[i]],
    low = 8, high = 16
  ))
  ann_phase[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  ann_phase[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
ann_phase <- bind_rows(ann_phase)
ann_phase$est_lag <- phaseToTime(ann_phase$est, p = 11.3, dt = 1)

# Extracting the average annual phase differences of dengue cycles comparing location-pairs
avg_ann_phase <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  avg_ann_phase[[i]] <- extract_avg_coh_phase_coi_updated(prov_combos$phase[[i]],
    prov_combos$scales[[i]],
    prov_combos$coi[[i]],
    low = 8, high = 16
  )
  avg_ann_phase[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  avg_ann_phase[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
avg_ann_phase <- bind_rows(avg_ann_phase)
avg_ann_phase$phase_lag <- phaseToTime(avg_ann_phase$est, p = 11.3, dt = 1)

uniq_avg_ann_phase <- suppressWarnings(left_join(uniq_prov_pairs, avg_ann_phase,
  by = c("ref_prov", "other_prov")
))

#### Extracting multiyear coherence & phase ####
# Filtering provs to only those with 10+ years for multiannual analyses
mlt_combos <- prov_df %>% filter(country %in% mlt_countries)
mlt_combos <- crossing(
  ref_prov = mlt_combos$province,
  other_prov = mlt_combos$province
) %>%
  filter(ref_prov != other_prov)
mlt_combos <- left_join(mlt_combos, prov_combos)

# Extracting the multiannual coherence over time of dengue cycles comparing location-pairs
mlt_coh <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  mlt_coh[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = mlt_combos$coherence[[i]],
    time = mlt_combos$times[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  ))

  mlt_coh[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  mlt_coh[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
mlt_coh <- bind_rows(mlt_coh)
# Unique pairs
uniq_mlt_coh <- suppressWarnings(left_join(uniq_prov_pairs, mlt_coh,
  by = c("ref_prov", "other_prov")
))

# Extracting the average multiannual coherence of dengue cycles comparing location-pairs
avg_mlt_coh <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  avg_mlt_coh[[i]] <- extract_avg_coh_phase_coi_updated(
    dat = mlt_combos$coherence[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  )
  avg_mlt_coh[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  avg_mlt_coh[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
avg_mlt_coh <- bind_rows(avg_mlt_coh)

# unique pairs
uniq_avg_mlt_coh <- suppressWarnings(left_join(uniq_prov_pairs,
  avg_mlt_coh,
  by = c("ref_prov", "other_prov")
))

# Extracting the multiannual phase over time for location-pairs
mlt_phase <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  mlt_phase[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = mlt_combos$phase[[i]], time = mlt_combos$times[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  ))
  mlt_phase[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  mlt_phase[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
mlt_phase <- bind_rows(mlt_phase)
mlt_phase$est_lag <- phaseToTime(mlt_phase$est, p = 33, dt = 1)

# Extracting the average annual phase differences of dengue cycles comparing location-pairs
avg_mlt_phase <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  avg_mlt_phase[[i]] <- extract_avg_coh_phase_coi_updated(
    dat = mlt_combos$phase[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  )
  avg_mlt_phase[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  avg_mlt_phase[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
avg_mlt_phase <- bind_rows(avg_mlt_phase)
avg_mlt_phase$phase_lag <- phaseToTime(avg_mlt_phase$est, p = 33, dt = 1)

uniq_avg_mlt_phase <- suppressWarnings(left_join(uniq_prov_pairs, avg_mlt_phase,
  by = c("ref_prov", "other_prov")
))

#### VECTOR ANALYSIS ####
#### Generating the measure for coh-phase for annual cycles ####
avg_ann_coh <- avg_ann_coh %>% rename(coh_est = est)
avg_ann_phase <- avg_ann_phase %>% rename(phase_est = est)

avg_ann_vec <- left_join(avg_ann_coh, avg_ann_phase, by = c("ref_prov", "other_prov")) %>%
  mutate(
    phase_pi = (phase_est / pi),
    phase_pi2 = (1 - abs(phase_pi)) * ifelse(phase_est > 0, 1, -1),
    est = coh_est * phase_pi2,
    abs_est = abs(est)
  )

ann_vec_bearing <- get_bearing_est(dat = avg_ann_vec, coord_dat = combined_coord)
ann_vec_delta <- get_delta_est(dat = ann_vec_bearing)


#### Generating the measure of coh-phase for multiannual cycles ####
avg_mlt_coh_dat <- avg_mlt_coh %>% rename(coh_est = est)
avg_mlt_phase_dat <- avg_mlt_phase %>% rename(phase_est = est)

avg_mlt_vec <- left_join(avg_mlt_coh_dat, avg_mlt_phase_dat, by = c("ref_prov", "other_prov")) %>%
  mutate(
    phase_pi = (phase_est / pi),
    phase_pi2 = (1 - abs(phase_pi)) * ifelse(phase_est > 0, 1, -1),
    est = coh_est * phase_pi2,
    abs_est = abs(est)
  )

mlt_vec_bearing <- get_bearing_est(dat = avg_mlt_vec, coord_dat = combined_coord)
mlt_vec_delta <- get_delta_est(dat = mlt_vec_bearing)

#### Figure 4: Explanatory figure ####
# png(paste0("./figures/fig4_", Sys.Date(), ".png", sep = ""),
#    width = 16, height = 10, unit = 'in', res = 600)
plot_fig4(
  prov_df,
  ann_coh,
  mlt_coh,
  ann_phase,
  mlt_phase, prov_coords
)
# dev.off()

#### Figure 5: Synchrony by Distance  ####
# Panel A & B - coherence and phase difference by distance
# png(paste("fig5a_", Sys.Date(), ".png", sep = ""),
#    width = 8, height = 12, unit = 'in', res = 600)
plot_fig5a(
  ann_coh_dat = avg_ann_coh, ann_phase_dat = avg_ann_phase,
  mlt_coh_dat = avg_mlt_coh, mlt_phase_dat = avg_mlt_phase,
  dist_dat = prov_dist, uniq_prov_pairs = uniq_prov_pairs
)
# dev.off()

# Panel C - average synchrony by distance
# png(paste("fig5b_", Sys.Date(), ".png", sep = ""),
#    width = 12, height = 12, unit = 'in', res = 600)
plot_fig5b(
  ann_coh_dat = avg_ann_coh, ann_phase_dat = avg_ann_phase,
  mlt_coh_dat = avg_mlt_coh, mlt_phase_dat = avg_mlt_phase,
  dist_dat = prov_dist, uniq_prov_pairs = uniq_prov_pairs
)
# dev.off()

#### Figure 6: Average synchrony by <2000km, 2000-3999, and 4000+####
# png(paste0("fig6_", Sys.Date(), ".png", sep = ""),
#    width = 16, height = 12, unit = 'in', res = 250)
plot_fig6(
  ann_data = avg_ann_vec,
  mlt_data = avg_mlt_vec,
  prov_coords = prov_coords,
  prov_dist = prov_dist
)
# dev.off()

#### Figure 7: Vector map ####
#png(paste0("fig7_", Sys.Date(), ".png", sep = ""),
#  width = 12.5, height = 10, unit = "in", res = 600
#)
plot_fig7(
  ann_alpha_data = ann_vec_delta,
  mlt_alpha_data = mlt_vec_delta,
  pop_data = avg_pop_coord,
  ymax = 31, ymin = -22,
  xmax = -36, xmin = -115,
  ann_arrow_scale = 50,
  mlt_arrow_scale = 50
)
#dev.off()

#### Estimating weighted phase-coh over time ####
pairs_df <- crossing(
  ref_prov = prov_df$province,
  other_prov = prov_df$province
) %>%
  filter(ref_prov != other_prov)

begin_year <- 1985:2014
end_year <- 1990:2019
# Annual cycles
# 5-year annual coherence estimates
ann_coh_5yr <- list(NULL)
for (i in 1:30) {
  ann_coh_5yr[[i]] <- get_phase_coh_time(ann_coh,
    pairs_df,
    start_time = begin_year[i],
    end_time = end_year[i]
  )
}
ann_coh_5yr_time <- bind_rows(ann_coh_5yr)

# 5-year Ann phase estimates
begin_year <- 1985:2014
end_year <- 1990:2019
ann_phase_5yr <- list(NULL)
for (i in 1:30) {
  ann_phase_5yr[[i]] <- get_phase_coh_time(ann_phase,
    pairs_df,
    start_time = begin_year[i],
    end_time = end_year[i]
  )
}
ann_phase_5yr_time <- bind_rows(ann_phase_5yr)

# Multiannual cycles #
begin_year <- 1985:2014
end_year <- 1990:2019

# 5-year multiannual coherence
mlt_coh_5yr <- list(NULL)
for (i in 1:30) {
  mlt_coh_5yr[[i]] <- get_phase_coh_time(mlt_coh,
    pairs_df,
    start_time = begin_year[i],
    end_time = end_year[i]
  )
}
mlt_coh_5yr_time <- bind_rows(mlt_coh_5yr)

# Code for the mlt phase over time
begin_year <- 1985:2014
end_year <- 1990:2019

mlt_phase_5yr <- list(NULL)
for (i in 1:30) {
  mlt_phase_5yr[[i]] <- get_phase_coh_time(mlt_phase,
    pairs_df,
    start_time = begin_year[i],
    end_time = end_year[i]
  )
}

mlt_phase_5yr_time <- bind_rows(mlt_phase_5yr)

#### Distribution of annual synchrony over time  ####
# Estimating the average coh for each province
avg_ann_coh_time <- ann_coh_5yr_time %>%
  filter(!is.na(est)) %>%
  group_by(ref_prov, other_prov, yr_midpoint) %>%
  summarize(avg_coh = mean(est, na.rm = T), .groups = 'drop')

avg_ann_phase_time <- ann_phase_5yr_time %>%
  filter(!is.na(est)) %>%
  group_by(ref_prov, other_prov, yr_midpoint) %>%
  summarize(avg_phase = mean(est, na.rm = T), .groups = 'drop')

# Creating the synchrony estimate
avg_ann_phase_coh_time <- left_join(avg_ann_coh_time, avg_ann_phase_time, by = c(
  "ref_prov" = "ref_prov", "other_prov" = "other_prov",
  "yr_midpoint" = "yr_midpoint"
)) %>%
  mutate(
    phase_pi = (avg_phase / pi),
    phase_pi2 = (1 - abs(phase_pi)),
    est = avg_coh * phase_pi2
  ) %>%
  group_by(ref_prov, yr_midpoint) %>%
  summarize(avg_est = mean(est, na.rm = T), .groups = 'drop')

#### Multiannual synchrony over time ####
avg_mlt_coh_time <- mlt_coh_5yr_time %>%
  filter(!is.na(est)) %>%
  group_by(ref_prov, other_prov, yr_midpoint) %>%
  summarize(avg_coh = mean(est, na.rm = T), .groups = 'drop')

avg_mlt_phase_time <- mlt_phase_5yr_time %>%
  filter(!is.na(est)) %>%
  group_by(ref_prov, other_prov, yr_midpoint) %>%
  summarize(avg_phase = mean(est, na.rm = T), .groups = 'drop')

avg_mlt_phase_coh_time <- left_join(avg_mlt_coh_time, avg_mlt_phase_time,
  by = c(
    "ref_prov" = "ref_prov", "other_prov" = "other_prov",
    "yr_midpoint" = "yr_midpoint"
  )
) %>%
  ungroup() %>%
  mutate(
    phase_pi = (avg_phase / pi),
    phase_pi2 = (1 - abs(phase_pi)),
    est = avg_coh * phase_pi2
  ) %>%
  group_by(ref_prov, yr_midpoint) %>%
  summarize(avg_est = mean(est, na.rm = T), .groups = 'drop')

# Joining the two
avg_ann_phase_coh_time$time_scale <- "1"
avg_mlt_phase_coh_time$time_scale <- "2"
avg_phase_coh_time <- bind_rows(avg_ann_phase_coh_time, avg_mlt_phase_coh_time) %>%
  mutate(year = floor(yr_midpoint))

#### CLIMATE WAVELETS ####
#### Extracting temp wavelets ####
temp_df <- bind_rows(
  calculate_country_wavelets(filter(temp, country == "br"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "mx"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "gt"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "ve"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "co"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "pe"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "dr"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "pr"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "ec"), data_var = 'avg_temp'),
  calculate_country_wavelets(filter(temp, country == "ar"), data_var = 'avg_temp')
) %>% mutate(measure = "temp")

#### Extracting temperature multiannual wave
mlt_temp_df <- temp_df %>% filter(country %in% mlt_countries)
mlt_temp_df$mlt_wave <- list(NULL)
for (i in 1:dim(mlt_temp_df)[1]) {
  mlt_temp_df$mlt_wave[[i]] <- extract_wavelet_coi(
    wave = mlt_temp_df$wave[[i]], scale = mlt_temp_df$scale[[i]],
    coi = mlt_temp_df$coi[[i]], time = mlt_temp_df$time[[i]],
    low = 16
  )
}

mlt_temp_wavelet <- mlt_temp_df %>%
  select(country, province, mlt_wave) %>%
  tidyr::unnest(cols = c(mlt_wave))

#### Temperature Spectra analysis and global coherence ####
# Temperature spectra #
temp_ts <- temp_df %>%
  select(country, province, time, Ts) %>%
  tidyr::unnest(cols = c(time, Ts))

locations <- unique(temp_ts$province)
alphas <- tibble()
for (i in 1:length(locations)) {
  this_ts <- filter(temp_ts, province == locations[i])$Ts
  this_ts <- normalizeSeries(this_ts)
  this_ar1 <- ar1(this_ts)
  alphas <- bind_rows(
    alphas,
    tibble(
      province = locations[i],
      alpha = this_ar1$ar,
      var = this_ar1$var,
      n = length(this_ts)
    )
  )
}

temp_alpha <- left_join(
  alphas,
  select(temp_ts, country, province) %>% filter(!duplicated(.))
)

alpha_country <- group_by(temp_alpha, country) %>%
  summarize(
    median = median(alpha, na.rm = T),
    mean = mean(alpha, na.rm = T),
    min = min(alpha, na.rm = T),
    max = max(alpha, na.rm = T),
    median_var = median(var, na.rm = T),
    max_n = max(n),
    n_prov = n()
  )

alpha_country

# Estimates global spectra
temp_global_cwts <- tibble()
for (i in 1:length(locations)) {
  this_ts <- filter(temp_ts, province == locations[i])$Ts
  this_cwt <- completeCWT(data.frame(t = 1:length(this_ts), x = this_ts), dt = 1, dj = 1 / 4, sigType = "red")
  temp_global_cwts <- bind_rows(
    temp_global_cwts,
    tibble(
      country = unique(temp_ts$country[temp_ts$province == locations[i]]),
      location = locations[i],
      period = this_cwt$periods,
      scales = this_cwt$scales,
      power = this_cwt$global,
      sig_red = this_cwt$globalSig$upper
    )
  )
}

temp_global_sig_country <- tibble()
for (i in 1:nrow(alpha_country)) {
  this_spec <- alpha_powerChiSquare(
    n = alpha_country$max_n[i],
    alpha = alpha_country$median[i]
  )
  this_spec_white <- alpha_powerChiSquare(n = alpha_country$max_n[i], alpha = 0)
  temp_global_sig_country <- bind_rows(
    temp_global_sig_country,
    tibble(
      country = alpha_country$country[i],
      period = this_spec$period,
      sig_red = this_spec$upper,
      sig_white = this_spec_white$upper
    )
  )
}

chisq_spec <- tibble()
for (i in 1:length(locations)) {
  this_location <- filter(temp_global_cwts, location == locations[i])
  this_country <- unique(this_location$country)
  this_chisq <- get_signif_power(
    exp_power = filter(temp_global_sig_country, country == this_country),
    obs_power = filter(this_location)$power,
    i
  )
  chisq_spec <- bind_rows(
    chisq_spec,
    tibble(this_chisq,
      location = locations[i]
    )
  )
}

#### Extracting temperature & dengue  coherence to compare cycles within each location ####
denv_df <- prov_df %>%
  mutate(measure = "denv")

denv_temp_combos <- data.frame(
  ref_prov = denv_df$province,
  other_prov = denv_df$province
)

# Setting up lists
denv_temp_combos$coherence <- list(NULL)
denv_temp_combos$phase <- list(NULL)
denv_temp_combos$scales <- list(NULL)
denv_temp_combos$coi <- list(NULL)
denv_temp_combos$times <- list(NULL)

for (i in 1:dim(denv_temp_combos)[1]) {
  # Selecting the provinces
  ref_prov <- filter(denv_df, province == denv_temp_combos$ref_prov[i]) # dengue as reference
  other_prov <- filter(temp_df, province == denv_temp_combos$other_prov[i])
  if (nrow(ref_prov) != 1 | nrow(other_prov) != 1) next()
  # Matching the scales and times
  match_obj <- match_transforms(
    wave1 = ref_prov$wave[[1]], wave2 = other_prov$wave[[1]],
    time1 = ref_prov$time[[1]], time2 = other_prov$time[[1]],
    scale1 = ref_prov$scale[[1]], scale2 = other_prov$scale[[1]]
  )

  denv_temp_combos$scales[[i]] <- match_obj$scales
  denv_temp_combos$coi[[i]] <- get_coi(n = length(match_obj$times), dt = 1)
  denv_temp_combos$times[[i]] <- match_obj$times

  # Coherence
  denv_temp_combos$coherence[[i]] <- coh(
    cwt1 = match_obj$wave1, # Ref prov
    cwt2 = match_obj$wave2, # other prov
    scales = match_obj$scales,
    dt = 1,
    dj = 1 / 4
  )
  # Phase
  denv_temp_combos$phase[[i]] <- phase_updated(
    cwt1 = match_obj$wave1,
    cwt2 = match_obj$wave2,
    scales = match_obj$scales,
    times = match_obj$times,
    dt = 1,
    dj = 1 / 4
  )
}

#### Extracting coherence over time of seasonal cycles of dengue - temperature####
ann_temp_coh_time <- list(NULL)
for (i in 1:nrow(denv_temp_combos)) {
  if (is.null(denv_temp_combos$coherence[[i]])) {
    print(paste(denv_temp_combos$ref_prov[[i]], 'missing'))
    next
  }
  ann_temp_coh_time[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = denv_temp_combos$coherence[[i]],
    time = denv_temp_combos$times[[i]],
    scales = denv_temp_combos$scales[[i]],
    coi = denv_temp_combos$coi[[i]],
    low = 8, high = 16
  ))

  ann_temp_coh_time[[i]]$ref_prov <- denv_temp_combos$ref_prov[[i]]
  ann_temp_coh_time[[i]]$other_prov <- denv_temp_combos$other_prov[[i]]
}
ann_temp_coh_time <- bind_rows(ann_temp_coh_time)

#### Extracting seasonal cycle of dengue - temp average coherence ####
avg_ann_temp_coh <- list(NULL)
for (i in 1:nrow(denv_temp_combos)) {
  avg_ann_temp_coh[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(
    dat = denv_temp_combos$coherence[[i]],
    scales = denv_temp_combos$scales[[i]],
    coi = denv_temp_combos$coi[[i]],
    low = 8,
    high = 16
  ))

  avg_ann_temp_coh[[i]]$ref_prov <- denv_temp_combos$ref_prov[[i]]
  avg_ann_temp_coh[[i]]$other_prov <- denv_temp_combos$other_prov[[i]]
}
avg_ann_temp_coh <- bind_rows(avg_ann_temp_coh)

#### Extracting average phase difference over time of seasonal cycles of dengue-temperature ####
avg_ann_temp_phase <- list(NULL)
for (i in 1:nrow(denv_temp_combos)) {
  if (is.null(denv_temp_combos$coherence[[i]])) {
    print(paste(denv_temp_combos$ref_prov[[i]], 'missing'))
    next
  }
  avg_ann_temp_phase[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(denv_temp_combos$phase[[i]],
    denv_temp_combos$scales[[i]],
    denv_temp_combos$coi[[i]],
    low = 8, high = 16
  ))
  avg_ann_temp_phase[[i]]$ref_prov <- denv_temp_combos$ref_prov[[i]]
  avg_ann_temp_phase[[i]]$other_prov <- denv_temp_combos$other_prov[[i]]
}
avg_ann_temp_phase <- bind_rows(avg_ann_temp_phase)
avg_ann_temp_phase$phase_lag <- phaseToTime(avg_ann_temp_phase$est, p = 11.3, dt = 1)

#### Temp-dengue analysis: subsetting multiannual cycles to only countries of interst ####
mlt_denv_temp_combos <- denv_temp_combos %>%
  mutate(country = gsub("_.*", "", ref_prov)) %>%
  filter(country %in% mlt_countries)

#### Extracting coherence over time of multiannual cycles of dengue - temperature ####
mlt_temp_coh_time <- list(NULL)
for (i in 1:nrow(mlt_denv_temp_combos)) {
  if (is.null(denv_temp_combos$coherence[[i]])) {
    print(paste(denv_temp_combos$ref_prov[[i]], 'missing'))
    next
  }
  mlt_temp_coh_time[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = mlt_denv_temp_combos$coherence[[i]],
    time = mlt_denv_temp_combos$times[[i]],
    scales = mlt_denv_temp_combos$scales[[i]],
    coi = mlt_denv_temp_combos$coi[[i]],
    low = 17
  ))

  mlt_temp_coh_time[[i]]$ref_prov <- mlt_denv_temp_combos$ref_prov[[i]]
  mlt_temp_coh_time[[i]]$other_prov <- mlt_denv_temp_combos$other_prov[[i]]
}
mlt_temp_coh_time <- bind_rows(mlt_temp_coh_time)

#### Extracting average coherence of multiannual cycles of dengue - temperature  ####
avg_mlt_temp_coh <- list(NULL)
for (i in 1:nrow(mlt_denv_temp_combos)) {
  avg_mlt_temp_coh[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(
    dat = mlt_denv_temp_combos$coherence[[i]],
    scales = mlt_denv_temp_combos$scales[[i]],
    coi = mlt_denv_temp_combos$coi[[i]],
    low = 17
  ))

  avg_mlt_temp_coh[[i]]$ref_prov <- mlt_denv_temp_combos$ref_prov[[i]]
  avg_mlt_temp_coh[[i]]$other_prov <- mlt_denv_temp_combos$other_prov[[i]]
}
avg_mlt_temp_coh <- bind_rows(avg_mlt_temp_coh)

#### Extracting average mlt phase difference ####
avg_mlt_temp_phase <- list(NULL)
for (i in 1:nrow(mlt_denv_temp_combos)) {
  avg_mlt_temp_phase[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(mlt_denv_temp_combos$phase[[i]],
    mlt_denv_temp_combos$scales[[i]],
    mlt_denv_temp_combos$coi[[i]],
    low = 17
  ))
  avg_mlt_temp_phase[[i]]$ref_prov <- mlt_denv_temp_combos$ref_prov[[i]]
  avg_mlt_temp_phase[[i]]$other_prov <- mlt_denv_temp_combos$other_prov[[i]]
}
avg_mlt_temp_phase <- bind_rows(avg_mlt_temp_phase)
avg_mlt_temp_phase$phase_lag <- phaseToTime(avg_mlt_temp_phase$est, p = 33, dt = 1)

#### Extracting precipitation wavelets ####
ppt_df <- bind_rows(
  calculate_country_wavelets(filter(ln_ppt, country == "br"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "mx"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "gt"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "ve"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "co"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "pe"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "dr"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "pr"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "ec"), data_var = 'ln_ppt'),
  calculate_country_wavelets(filter(ln_ppt, country == "ar"), data_var = 'ln_ppt')
) %>% mutate(measure = "ppt")

#### Extracting rainfall multiannual wavelets
mlt_ppt_df <- ppt_df %>% filter(country %in% mlt_countries)

mlt_ppt_df$mlt_wave <- list(NULL)
for (i in 1:dim(mlt_ppt_df)[1]) {
  mlt_ppt_df$mlt_wave[[i]] <- extract_wavelet_coi(
    wave = mlt_ppt_df$wave[[i]], scale = mlt_ppt_df$scale[[i]],
    coi = mlt_ppt_df$coi[[i]], time = mlt_ppt_df$time[[i]],
    low = 16
  )
}

mlt_ppt_wavelet <- (mlt_ppt_df) %>%
  select(country, province, mlt_wave) %>%
  tidyr::unnest(cols = c(mlt_wave))

#### Extracting precipitation coherence & phase ####
# crossing
denv_ppt_combos <- crossing(
  ref_prov = denv_df$province,
  other_prov = ppt_df$province
) %>%
  filter(ref_prov == other_prov)

# Setting up lists
denv_ppt_combos$coherence <- list(NULL)
denv_ppt_combos$phase <- list(NULL)
denv_ppt_combos$scales <- list(NULL)
denv_ppt_combos$coi <- list(NULL)
denv_ppt_combos$times <- list(NULL)

for (i in 1:dim(denv_ppt_combos)[1]) {
  # Selecting the provinces
  ref_prov <- filter(denv_df, province == denv_ppt_combos$ref_prov[i]) # dengue as reference
  other_prov <- filter(ppt_df, province == denv_ppt_combos$other_prov[i])
  if (nrow(ref_prov) != 1 | nrow(other_prov) != 1) next()
  # Matching the scales and times
  match_obj <- match_transforms(
    wave1 = ref_prov$wave[[1]], wave2 = other_prov$wave[[1]],
    time1 = ref_prov$time[[1]], time2 = other_prov$time[[1]],
    scale1 = ref_prov$scale[[1]], scale2 = other_prov$scale[[1]]
  )

  denv_ppt_combos$scales[[i]] <- match_obj$scales
  denv_ppt_combos$coi[[i]] <- get_coi(n = length(match_obj$times), dt = 1)
  denv_ppt_combos$times[[i]] <- match_obj$times

  # Coherence
  denv_ppt_combos$coherence[[i]] <- coh(
    cwt1 = match_obj$wave1, # Ref prov
    cwt2 = match_obj$wave2, # other prov
    scales = match_obj$scales,
    dt = 1,
    dj = 1 / 4
  )
  # Phase
  denv_ppt_combos$phase[[i]] <- phase_updated(
    cwt1 = match_obj$wave1,
    cwt2 = match_obj$wave2,
    scales = match_obj$scales,
    times = match_obj$times,
    dt = 1,
    dj = 1 / 4
  )
}

#### Extracting seasonal  dengue - precipitation coherence ####
ann_ppt_coh_time <- list(NULL)
for (i in 1:nrow(denv_ppt_combos)) {
  ann_ppt_coh_time[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = denv_ppt_combos$coherence[[i]],
    time = denv_ppt_combos$times[[i]],
    scales = denv_ppt_combos$scales[[i]],
    coi = denv_ppt_combos$coi[[i]],
    low = 8, high = 16
  ))

  ann_ppt_coh_time[[i]]$ref_prov <- denv_ppt_combos$ref_prov[[i]]
  ann_ppt_coh_time[[i]]$other_prov <- denv_ppt_combos$other_prov[[i]]
}
ann_ppt_coh_time <- bind_rows(ann_ppt_coh_time)

#### Extraction average coherence for seasonal cycles of seasonal dengue and precipitation  ####
avg_ann_ppt_coh <- list(NULL)
for (i in 1:nrow(denv_ppt_combos)) {
  avg_ann_ppt_coh[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(
    dat = denv_ppt_combos$coherence[[i]],
    scales = denv_ppt_combos$scales[[i]],
    coi = denv_ppt_combos$coi[[i]],
    low = 8,
    high = 16
  ))

  avg_ann_ppt_coh[[i]]$ref_prov <- denv_ppt_combos$ref_prov[[i]]
  avg_ann_ppt_coh[[i]]$other_prov <- denv_ppt_combos$other_prov[[i]]
}
avg_ann_ppt_coh <- bind_rows(avg_ann_ppt_coh)

#### Extracting average phase difference of seasonal cycles of dengue and precipitation ####
avg_ann_ppt_phase <- list(NULL)
for (i in 1:nrow(denv_ppt_combos)) {
  avg_ann_ppt_phase[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(denv_ppt_combos$phase[[i]],
    denv_ppt_combos$scales[[i]],
    denv_ppt_combos$coi[[i]],
    low = 8, high = 16
  ))
  avg_ann_ppt_phase[[i]]$ref_prov <- denv_ppt_combos$ref_prov[[i]]
  avg_ann_ppt_phase[[i]]$other_prov <- denv_ppt_combos$other_prov[[i]]
}
avg_ann_ppt_phase <- bind_rows(avg_ann_ppt_phase)
avg_ann_ppt_phase$phase_lag <- phaseToTime(avg_ann_ppt_phase$est, p = 11.3, dt = 1)

#### Subsetting ppt-dengue multiannual analyses to countries of interest ####
mlt_denv_ppt_combos <- denv_ppt_combos %>%
  mutate(country = gsub("_.*", "", ref_prov)) %>%
  filter(country %in% mlt_countries)

#### Extracting coherence over time of multiannual cycles of dengue and precipitation ####
mlt_ppt_coh_time <- list(NULL)
for (i in 1:nrow(mlt_denv_ppt_combos)) {
  mlt_ppt_coh_time[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = mlt_denv_ppt_combos$coherence[[i]],
    time = mlt_denv_ppt_combos$times[[i]],
    scales = mlt_denv_ppt_combos$scales[[i]],
    coi = mlt_denv_ppt_combos$coi[[i]],
    low = 17
  ))

  mlt_ppt_coh_time[[i]]$ref_prov <- mlt_denv_ppt_combos$ref_prov[[i]]
  mlt_ppt_coh_time[[i]]$other_prov <- mlt_denv_ppt_combos$other_prov[[i]]
}

#### Extracting average coherence of multiannual cycles of dengue and precipitation ####
avg_mlt_ppt_coh <- list(NULL)
for (i in 1:nrow(mlt_denv_ppt_combos)) {
  avg_mlt_ppt_coh[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(
    dat = mlt_denv_ppt_combos$coherence[[i]],
    scales = mlt_denv_ppt_combos$scales[[i]],
    coi = mlt_denv_ppt_combos$coi[[i]],
    low = 17
  ))

  avg_mlt_ppt_coh[[i]]$ref_prov <- mlt_denv_ppt_combos$ref_prov[[i]]
  avg_mlt_ppt_coh[[i]]$other_prov <- mlt_denv_ppt_combos$other_prov[[i]]
}

avg_mlt_ppt_coh <- bind_rows(avg_mlt_ppt_coh)

#### Extracting average phase difference of multiannual cycles of dengue and ENSO  ####
avg_mlt_ppt_phase <- list(NULL)
for (i in 1:nrow(mlt_denv_ppt_combos)) {
  avg_mlt_ppt_phase[[i]] <- bind_rows(extract_avg_coh_phase_coi_updated(mlt_denv_ppt_combos$phase[[i]],
    mlt_denv_ppt_combos$scales[[i]],
    mlt_denv_ppt_combos$coi[[i]],
    low = 17
  ))
  avg_mlt_ppt_phase[[i]]$ref_prov <- mlt_denv_ppt_combos$ref_prov[[i]]
  avg_mlt_ppt_phase[[i]]$other_prov <- mlt_denv_ppt_combos$other_prov[[i]]
}
avg_mlt_ppt_phase <- bind_rows(avg_mlt_ppt_phase)
avg_mlt_ppt_phase$phase_lag <- phaseToTime(avg_mlt_ppt_phase$est, p = 33, dt = 1)

#### Extracting ENSO wavelets ####
enso_df <- calculate_country_wavelets(mutate(enso, co_province = NA), data_var = "enso")

enso_df$mlt_wave <- list(NULL)
for (i in 1:dim(enso_df)[1]) {
  enso_df$mlt_wave[[i]] <- extract_wavelet_coi(
    wave = enso_df$wave[[i]], scale = enso_df$scale[[i]],
    coi = enso_df$coi[[i]], time = enso_df$time[[i]],
    low = 16
  )
}

mlt_enso_wavelet <- (enso_df) %>%
  select(province, mlt_wave) %>%
  tidyr::unnest(cols = c(mlt_wave))

#### Figure 3: Multiannual wavelets ####
png(paste("fig3_", Sys.Date(), ".png", sep = ""),
  res = 100, width = 20, height = 24, unit = "in"
)
plot_fig3(
  ann_data = ann_wavelet,
  mlt_data = mlt_wavelet,
  sync_data = avg_phase_coh_time,
  temp_data = mlt_temp_wavelet,
  ppt_data = mlt_ppt_wavelet,
  enso_data = mlt_enso_wavelet
)
dev.off()


