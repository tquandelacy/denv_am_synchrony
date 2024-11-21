calculate_country_wavelets <- function(dat, data_var = 'ln_cases') {
  country_name <- dat$country[1]
  if (any(dat$country != country_name)) stop("function is designed for data from one country at a time, where every province has the same number of data points")
  dat <- dat[c("time", "co_province", data_var)]
  dat <- as.matrix(pivot_wider(data = dat, id_cols = "time", names_from = "co_province", values_from = data_var))
  
  # remove provinces with NAs
  dat <- dat[ , apply(is.na(dat), 2, sum) == 0]
  
  n_prov <- ncol(dat) - 1
  country_vec <- rep(country_name, n_prov)
  provvec <- colnames(dat)[2:(n_prov + 1)]
  timelist <- vector(mode = "list", length = n_prov)
  scaleslist <- vector(mode = "list", length = n_prov)
  periodlist <- vector(mode = "list", length = n_prov)
  wavelist <- vector(mode = "list", length = n_prov)
  tslist <- vector(mode = "list", length = n_prov)
  coilist <- vector(mode = "list", length = n_prov)
  for (i in 1:n_prov) {
    tslist[[i]] <- dat[ , i + 1]

    if(all(is.na(dat[, i + 1])) == TRUE) next()
    this_ts <- dat[ , i + 1]

    m <- completeCWT(data.frame(t = dat[, 1], x = this_ts),
                     normalize = T,
                     dj  = 1/4, dt = 1, s0 = 2)

    timelist[[i]] <- dat[, 1]
    scaleslist[[i]] <- m$scales
    periodlist[[i]] <- m$periods
    wavelist[[i]] <- m$transform
    coilist[[i]] <- m$coi
  }

  return(tibble(
    country = country_vec,
    province = provvec,
    Ts = tslist,
    time = timelist,
    scale = scaleslist,
    period = periodlist,
    wave = wavelist,
    coi = coilist))
}
