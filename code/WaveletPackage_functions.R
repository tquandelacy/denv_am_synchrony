# Version 2.0 includes modified names (Java style),
# pink noise capabilities, and the corrected power spectrum (Liu 2007)
# added scale to fourier factor

#### Functions specific to preparing overlapping time-series
match_transforms <- function(wave1, wave2, time1, time2, scale1, scale2){
  # wave datasets are rows = # time, cols = # scales 
  date1 <- as.integer(as.Date(paste(floor(time1), round(1 + 12 * (time1 - floor(time1))), '1', sep ='-')))
  date2 <- as.integer(as.Date(paste(floor(time2), round(1 + 12 * (time2 - floor(time2))), '1', sep ='-')))
  
  date_inds <- intersect( date1, date2 )
  scale_inds <- intersect( scale1, scale2 ) 

  time_matched <- time1[date1 %in% date_inds]
  # indexing the times for each province 
  wave1_time <- match(date_inds, date1)
  wave1_scale <- match(scale_inds, scale1)
  wave2_time <- match(date_inds, date2)
  wave2_scale <- match(scale_inds, scale2)
  
  # subsetting to the same time & scales frame
  wave1 <- wave1[wave1_scale, wave1_time] 
  wave2 <- wave2[wave2_scale, wave2_time] 
  
  return(list(wave1 = wave1, wave2 = wave2, times = time_matched, date_int = date_inds,  scales = scale_inds))
}

extract_wavelet_coi <- function(wave, scale, coi, time, low = 0, high = 2^13) {
  if(is.null(wave) == TRUE) {
    y = data.frame(time = NA, wavelet = NA)    
    return(y)
  }
  # setting the scale indices for the time-scale 
  inds <- which(scale > low & scale <= high)
  # Setting the coi indices 
  # coi_inds <- coi >= scale
  # setting up the matrix 
  y <- matrix(nrow = ncol(wave), ncol = 2)
  y[, 1] <- time
  y[, 2] <- sapply(
    1:ncol(wave),
    function(i) {
      sum(Re(wave[inds, i]) / sqrt(scale[inds]))
    }
  )
  # cutting the time to reflect the coi limits
  # y <- y[coi_inds, ]
  if(length(y[, 2]) == 0){
    y = data.frame(time = NA, wavelet = NA)    
  }
  y <- data.frame(y)
  colnames(y) <- c("time", "wavelet")
  return(y)
}

# n = length of shared time-series
get_coi <- function(n, dt) {
  param <- 6
  fourierFactor <- (4 * pi) / (param + sqrt(2 + param^2)) # Morlet
  return(fourierFactor / sqrt(2) * dt *
           c(1E-5, 1:((n + 1) / 2 - 1), (n / 2 - 1):1, 1E-5)) # COI [Sec.3g]
}

### continuous wavelet transform functions
normalizeSeries <- function(x) {
	x <- x - mean(x)
	x <- x / sd(x)
}

cwt <- function(x, dt, dj, normalize, pad=TRUE, s0=NULL, J=NULL, mother="morlet", param=NULL) {
	if (normalize) x <- normalizeSeries(x)
	n <- length(x)
	if (is.null(s0)) s0 <- 2 * dt
	if (is.null(J)) J <- floor(1/dj * log2(n * dt / s0))
	if (pad) x <- c(x, rep(0, 2^(1 + ceiling(log2(n))) - n)) # pad with zeros power to make 2 if desired
	paddedN <- length(x)
	k <- 0:(paddedN - 1)
	omega <- 2 * pi * k / (paddedN * dt)
	omega[k > paddedN/2] <- (-omega[k > paddedN/2]) # [Eqn.5]
	scales <- s0 * 2^((0:J) * dj) # construct scale vector
	fftx <- fft(x) # [Eqn(3)] compute FFT of the (padded) time series
	wavet <- matrix(0+0i, nrow=J + 1, ncol=paddedN)   # define the complex wavelet array
	for (i in 1:(J + 1)) { # loop through all scales and compute transform
		wBase <- waveletBases(mother, omega, scales[i], param, dt)
		wavet[i, ] <- fft((fftx * wBase$daughter), inverse=T) / 
				length(fftx) # wavelet transform[Eqn(4)]
	}
	periods <- wBase$fourier.factor * scales
	coi <- wBase$coi * dt * c(1E-5, 1:((n + 1) / 2 - 1), (n / 2 - 1):1, 1E-5) # COI [Sec.3g]
	wavet <- wavet[ , 1:n]       # get rid of padding before returning
	return(list("transform"=wavet, "periods"=periods, "scales"=scales, "coi"=coi))
}

waveletBases <- function(mother, omega, scale, param=NULL, dt) {
	mother <- tolower(mother)
	if (mother == "morlet") {
		if (is.null(param)) param <- 6
	  daughter <- pi^(-1/4) * as.numeric(omega > 0) * 
	   		exp(-(scale * omega - param)^2 / 2) # Table 1
		fourierFactor <- (4 * pi) / (param + sqrt(2 + param^2))
				# Scale-->Fourier [Sec.3h] Table 1 without scale
		coi <- fourierFactor / sqrt(2)  # Cone-of-influence [Sec.3g]
	} else if (mother == "paul") {
		if (is.null(param)) param <- 4
	  m <- param
		daughter <- 2^m / sqrt(m * factorial(2 * m - 1)) * 
				as.numeric(omega > 0) * (scale * omega)^m * exp(-scale * omega)
		fourierFactor <- 4 * pi / (2 * m + 1)
		coi <- fourierFactor * sqrt(2)
	} else if (mother == "dog") {
		if (is.null(param)) param <- 2
	  m <- param
		daughter <- (-1i^param) / sqrt(gamma(m + 1 / 2)) * 
				(scale * omega)^m * exp(-(scale * omega)^2 / 2)
		fourierFactor <- 2 * pi / sqrt(m + 1 / 2)
		coi <- fourierFactor / sqrt(2)
	} else stop("wavelet must be morlet, paul, or dog")
	daughter <- sqrt(2 * pi * scale / dt) * daughter        # normalize [Eqn.6]
	w.base <- list("daughter"=daughter, "fourier.factor"=fourierFactor, "coi"=coi)
	return(w.base)
}

globalMean <- function(mat, periods, coi) {
	global <- numeric(length(periods))
	for (i in 1:length(periods)) {
	    global[i] <- mean(mat[i, ][coi >= periods[i]])
	}
	return(global)
}

alpha_powerChiSquare <- function(n, alpha, dt = 1, dj = 1/4, normalize=T, sigLvl=0.95) {
  dummyCWT <- cwt(1:n, dt=dt, dj=dj, normalize=normalize)
  scales <- dummyCWT$scales
  periods <- dummyCWT$periods
  na <- numeric(length(scales))
  for (i in 1:length(scales)) na[i] <- sum(dummyCWT$coi > periods[i])
  gammaFac <- 2.32  # for morlet(6)
  freq <- dt / periods        # freq=k/N for the scales used in the transform 
  fftTheor <- (1 - alpha^2) / (1 + alpha^2 - 2 * alpha * 
                                 cos(freq * 2 * pi)) * 1/scales   # [Eqn(16)] - gives vector P(k)
  v <- 2 * sqrt(1 + (na * dt / (gammaFac * scales))^2)     # [Eqn(23)]
  fftTheor[na == 0] <- v[na == 0] <- NA
  data.frame(
    period = periods,
    mean = fftTheor,
    lower = qchisq((1 - sigLvl) / 2, v) * fftTheor / v,
    upper = qchisq(1 - (1 - sigLvl) / 2, v) * fftTheor / v)
}


get_obs_global_coh <- function(coh_list, scales, coi) {
  global <- numeric(length(scales))
  for (i in 1:length(scales)) {
    global[i] <- mean(coh_list[ i, ][coi >= scales[i]])
  } 
  
  return(global)
}

get_signif_global_coh <- function(exp_coh, obs_coh,  loc_idx){ 
  
  n <- length(obs_coh)
  exp_coh <- exp_coh[1:n, ]
  dat <- cbind(exp_coh, obs_coh)
  dat <- dat[,1:5] 
  sig <- t(apply(cbind(A = data.frame(dat[, "upper"]), 
                       x = obs_coh), 1, function(x) {
                         A <- x[1]
                         x <- x[-1]
                         ifelse(A < x, 1, ifelse(A > x, 0, NA))
                       }))
  
  dat[, 6] <- as.numeric(sig)
  colnames(dat)[6] <- "signif"
  dat  <- mutate(dat, loc_idx = loc_idx)
  return(dat)
}

ar1 <- function(x) {
	arX <- ar(x, order.max=1)
	if (arX$order == 0) beta1 <- 0
	if (arX$order == 1) beta1 <- arX$ar
	return(list("ar"=beta1, "var"=arX$var.pred))
}

simRedNoise <- function(n, arModel) { # generates a red noise series vector of length n and form x[t]=alpha*x[t-1]+N(0, var)
	x <- numeric(n)
	x[1] <- rnorm(1, 0, sd=sqrt(arModel$var))
	for (i in 2:n) {
		x[i] <- arModel$ar * x[i - 1] + rnorm(1, 0, sqrt(arModel$var))
	}
	return(normalizeSeries(x))
}

powerSpectrum <- function(cwtObj) { # adjusted per Liu with pre-existing function
	normalizePower((abs(cwtObj$transform))^2, cwtObj$scales)
}

powerChiSquare <- function(series, dt, dj, normalize, sigType='red', 
                           sigLvl=0.95, mother="morlet", param=6, ...) {
  if (tolower(mother) != "morlet" | param != 6) {
    stop("The Chi-square significance test currently only works for the morlet(6) wavelet")
  }
  if (!normalize) stop("The Chi-square significance test has only been tested with normalize=T")
  n <- length(series)
  dummyCWT <- cwt(1:n, dt=dt, dj=dj, normalize=normalize, ...)
  scales <- dummyCWT$scales
  periods <- dummyCWT$periods
  na <- numeric(length(scales))
  gammaFac <- 2.32  # for morlet(6)
  freq <- dt / periods        # freq=k/N for the scales used in the transform 
  for (i in 1:length(scales)) na[i] <- sum(dummyCWT$coi > periods[i])
  if (sigType == 'red' & ar(series, order.max=1)$order == 1) {
    alpha <- ar(series, order.max=1)$ar
  } else if (sigType == 'pink') {
    stop("this significance test is not paramtereized for pink noise")
  } else alpha <- 0
  fftTheor <- (1 - alpha^2) / (1 + alpha^2 - 2 * alpha * 
                                 cos(freq * 2 * pi)) * 1/scales   # [Eqn(16)] - gives vector P(k)
  localSigTmp <- qchisq(1 - (1 - sigLvl) / 2, 2) * fftTheor / 2	
  localSig <- matrix(rep(localSigTmp, n), nrow=length(periods), ncol=n)
  v <- 2 * sqrt(1 + (na * dt / (gammaFac * scales))^2)     # [Eqn(23)]
  fftTheor[na == 0] <- v[na == 0] <- NA
  globalSig <- matrix(nrow=length(periods), ncol=3, 
                      dimnames=list(1:length(periods), c("lower", "mean", "upper")))
  globalSig[ , "upper"] <- qchisq(1 - (1 - sigLvl) / 2, v) * fftTheor / v
  globalSig[ , "mean"] <- fftTheor
  globalSig[ , "lower"] <- qchisq((1 - sigLvl) / 2, v) * fftTheor / v
  localSig <- qchisq(1 - (1 - sigLvl) / 2, 2) * fftTheor / 2
  return(list("local"=matrix(localSig, nrow=length(scales), ncol=n), 
              "global"=as.data.frame(globalSig)))
}


powerMC <- function(series, dt, dj, normalize, sim=1000, sigLvl=.95, sigType='red', ...) { # use original time series (not normalized)
  	n <- length(series)
  	dummyCWT <- cwt(1:n, dt=dt, dj=dj, normalize=normalize, ...)
  	scales <- dummyCWT$scales
  	periods <- dummyCWT$periods
  	coi <- dummyCWT$coi
  	na <- numeric(length(scales))
  	for (i in 1:length(scales)) na[i] <- sum(coi > periods[i])
  	mcArray <- array(dim=c(length(scales), n, sim))
  	for (i in 1:sim) {
  		if (sigType == 'red') {
  			this_series <- simRedNoise(n, ar1(series))
  		} else if (sigType == 'pink') {
  			this_series <- simPinkNoise(n, alphaPinkNoise(series))
  		} else if (sigType == 'white') {
  		  this_series <- rnorm(n)
  		} else stop("sigType must be 'white', 'pink', or 'red'")
  		this_cwt <- cwt(this_series, dt=dt, dj=dj, normalize=normalize, ...)
  		mcArray[ , , i] <- powerSpectrum(this_cwt)
  	}
  	
  	globalMC <- matrix(nrow = length(scales), ncol=3, 
  			dimnames=list(1:length(scales), c("lower", "mean", "upper")))
  	localMC <- rep(NA, length(scales))
  	for (i in 1:length(scales)) {
  		if (na[i] > 0) {
  			globalMC[i, "mean"] <- 
  					mean(apply(mcArray[i, coi > periods[i], ], 2, mean))
  			globalMC[i, "upper"] <- 
  					quantile(apply(mcArray[i, coi > periods[i], ], 2, mean), 
  					probs=1 - ((1 - sigLvl) / 2), type=8)
  			globalMC[i, "lower"] <- 
  					quantile(apply(mcArray[i, coi > periods[i], ], 2, mean), 
  					probs=(1 - sigLvl) / 2, type=8)
  			localMC[i] <- quantile(mcArray[i, coi > periods[i], ], 
  					probs=1 - ((1 - sigLvl) / 2), type=8)
  		}
  	}
  	return(list("local"=matrix(localMC, nrow=length(scales), ncol=n), 
  			"global"=as.data.frame(globalMC)))
}

completeCWT <- function(series, dj = NULL, normalize = TRUE, sigType = 'red', mcSim = NULL, sigLvl = 0.95, 
		dt=NULL, ...) { ### use mc.sim=-1 for chi-square (non-simulated) significance
	if (is.null(dj)) stop("dj must be specified by the user")
	if (is.null(dt)) dt <- series$t[2] - series$t[1]
	if (sigType == 'pink' & is.null(mcSim)) stop("for pink noise, mcSim is required")
	wavet <- cwt(series$x, dt, dj, normalize, ...)
		cwtPower <- powerSpectrum(wavet)

	global <- globalMean(cwtPower, wavet$periods, wavet$coi)
	if (!is.null(mcSim)) {
	 powerSig <- powerMC(series$x, dt=dt, dj=dj, normalize=normalize, sim=mcSim, 
    sigType=sigType, sigLvl=sigLvl, ...)

	} else {
	  powerSig <- powerChiSquare(series$x, dt=dt, dj=dj, normalize=normalize, 
	    sigType=sigType, sigLvl=sigLvl, ...)
	 
	}
	return(list("transform" = wavet$transform, "local" = cwtPower, 
			"localSig" = powerSig$local, "global" = global, 
			"globalSig"=powerSig$global, "time" = series$t, 
			"periods" = wavet$periods, "scales" = wavet$scales, 
			"coi" = wavet$coi, "x" = series$x, "dj" = dj, "dt" = dt, "normalize" = normalize))
}

### wavelet coherence functions
smoothCWT <- function(wavet, dt, dj, scales) {
	n <- dim(wavet)[2]
	npad <- 2^ceiling(log2(n))	
	k <- c(0, 1:floor(npad / 2), (-ceiling(npad / 2 - 1)):-1)
	snorm <- scales / dt
	for (i in 1:dim(wavet)[1]) {
		timeFilter <- exp(-k^2 / (2 * snorm[i]^2))
		timeFilter <- timeFilter / sum(timeFilter) # normalize
		smooth <- convolve(timeFilter, c(wavet[i, ], rep(0, npad - n)), 
				conj=F) # filter in time
		wavet[i, ] <- smooth[1:n]
	}
	dj0 <- 0.60
	sfWidth <- dj0 / (dj * 2)
	scaleFilter <- c(sfWidth - floor(sfWidth), 
			rep(1, round(2 * sfWidth - 1)), sfWidth - floor(sfWidth))
	scaleFilter <- scaleFilter / sum(scaleFilter) # normalize
	for (i in 1:dim(wavet)[2]) { # row = time, col = scale
		convTmp <- convolve(wavet[ , i], scaleFilter, 
				type='open') # filter in scale
		pad <- (length(convTmp) - length(scales)) / 2
	   wavet[ , i] <- convTmp[(pad + 1):(pad + length(scales))]
	}
	return(wavet)
}

normalizePower <- function(mat, scales) {
	out <- array(dim=dim(mat))
	for (i in 1:dim(out)[2]) {
		out[ , i] <- mat[ , i] * 1/scales  # normalize
	}
	return(out)
}

coh <- function(cwt1, cwt2, scales, dt, dj){# startT, 
                #endT, series1T, series2T) {
  Wxy <- cwt1 * Conj(cwt2)
  numer <- abs(smoothCWT(normalizePower(Wxy, scales), dt, dj, scales))^2
  
  denom <- smoothCWT(normalizePower(abs(cwt1)^2, scales), 
                      dt, dj, scales) * 
            smoothCWT(normalizePower(abs(cwt2)^2, scales), 
                      dt, dj, scales)
  return(numer / denom)
}

phase_updated <- function(cwt1, cwt2, scales, times, dt = 1, dj = 0.25) { 
  Wxy <- cwt1 * Conj(cwt2)
  sWxy <- smoothCWT(normalizePower(Wxy, scales), dt, dj, scales)
  phase <- atan2(Im(sWxy), Re(sWxy))
  # formatting 
  # dim: 22 72 
  rownames(phase) <- scales  
  colnames(phase) <- times
  return(phase)
}

coherence_ar1_mc <- function(n, alpha, var, dt, dj, s0, normalize=T, mcSim=1000, 
                             sigLvl=.95, ...) {
  dummyCWT <- cwt(1:n, dt=dt, dj=dj, s0=s0, normalize=normalize)
  periods <- dummyCWT$periods
  coi <- dummyCWT$coi
  globalMat <- matrix(nrow=length(periods), ncol=mcSim)
  for (i in 1:mcSim) { # generate wavelet transforms for red noise time series
    cwt1 <- cwt(simRedNoise(n, list(ar=alpha, var=var)), dt=dt, dj=dj, s0=s0, normalize=normalize, ...)
    cwt2 <- cwt(simRedNoise(n, list(ar=alpha, var=var)), dt=dt, dj=dj, s0=s0, normalize=normalize, ...)
    cohTmp <- coh(cwt1$transform, cwt2$transform, scales=dummyCWT$scales, dt=dt, dj=dj)
    for (j in 1:length(periods)) {
      globalMat[j, i] <- mean(cohTmp[j, coi > periods[j]])
    }
  }
  return(
    data.frame(
      period = periods,
      mean = apply(globalMat, 1, mean, na.rm=T),
      lower = apply(globalMat, 1, quantile, probs=(1 - sigLvl) / 2, na.rm=T),
      upper = apply(globalMat, 1, quantile, probs=(1 - (1 - sigLvl) / 2), na.rm=T)
    )
  )
}

completeCOH <- function(series1, series2, dj=NULL, normalize=TRUE, sigType, mcSim=1000, sigLvl=0.95,
                        dt=NULL, s0=NULL, J=NULL, ...) {
  if (is.null(dj)) stop("dj must be specified by the user")
  if (is.null(dt)) dt <- series1$t[2] - series1$t[1]
  startT <- max(c(min(series1$t), min(series2$t)))
  endT <- min(c(max(series1$t), max(series2$t)))
  sharedT <- series1$t[startT <= series1$t & series1$t <= endT]
  if (is.null(s0)) s0 <- 2 * dt
  
  if (is.null(J)) J <- floor(1 / dj * log2(length(sharedT) * dt / s0)) # sets the scales of the transforms
  
  sharedCWT <- cwt(sharedT, dt, dj, normalize=normalize, J=J, ...)
  cwt1 <- cwt(series1$x, dt, dj, normalize=normalize, J=J, ...)
  cwt2 <- cwt(series2$x, dt, dj, normalize=normalize, J=J, ...)
  coherence <- coh(cwt1, cwt2, dt, dj, startT, endT, series1$t, series2$t)
  global <- globalMean(coherence, sharedCWT$periods, sharedCWT$coi)
  coherenceSig <- coherenceMC(n=length(sharedT), series1$x, series2$x,
                              dt=dt, dj=dj, normalize=normalize, J=J, sigType=sigType, mcSim=mcSim, sigLvl=sigLvl, ...)
  coherenceSig$local <- coherenceSig$local[ , 1:length(sharedT)]
  return(list("local"=coherence, "localSig"=coherenceSig$local,
              "global"=global, "globalSig"=coherenceSig$global,
              "time"=sharedT, "periods"=sharedCWT$periods,
              "coi"=sharedCWT$coi, "dj"=dj, "dt"=dt))
}  

### additional functions
get_prov_avg_spectra <- function(gcwt, low = 0, high = 2^13){
  
  power <- gcwt$power
  period <- gcwt$period
  scale <- gcwt$scales 
  
  #y1 <- data.frame(power = NA, scale = NA, period = NA)
  gcwt %>% filter(scale > low & scale <= high) %>% 
    summarize(power = mean(power, na.rm = T)) 
}

get_signif_power <- function(obs_power, exp_power, i){ # 
  n <- length(obs_power)
  exp_power <- exp_power[1:n, ]
  dat <- cbind(exp_power, obs_power)
  dat <- dat[ , 1:5] # dropping the extra scale variable
  sig <- t(apply(cbind(A = data.frame(dat[, "sig_red"]), # upper 95% for red noise 
                       x = obs_power), 1, function(x) {
                         A <- x[1]
                         x <- x[-1]
                         ifelse(A < x, 1, ifelse(A > x, 0, NA))
                       }))
  
  dat[, 6] <- as.numeric(sig)
  colnames(dat)[6] <- "signif"
  
  dat  <- mutate(dat,  loc_idx = i)
  return(dat)
}

reconstructSeries <- function(cwtObject, sp1=-1, sp2=-1, 
		mother="morlet", param=6, ...) { 
		# sp1 and sp2 are the range of periods for which reconstruction is desired
	if (mother != "morlet" | param != 6) {
		stop("reconstruction is only setup for morlet(6) wavelets")
	}
	if (sp1 == -1 & sp2 == -1) {
		sp1 <- min(cwtObject$periods)
		sp2 <- max(cwtObject$periods)
	}
	recon <- numeric(length(cwtObject$time))
	dt <- cwtObject$time[2] - cwtObject$time[1]
	Cdelta <- 0.776                    # for the MORLET wavelet
	selected <- sp1 <= cwtObject$periods & cwtObject$periods <= sp2
	for (i in 1:length(recon)) {
		recon[i] <- (cwtObject$dj * sqrt(dt) / (Cdelta * (pi^(-1 / 4)))) * 
		sum(Re(cwtObject$transform[selected, i]) / sqrt(cwtObject$scales[selected]))
	}
	return(recon)
}

phase <- function(cwtObject, s=NA) {
	if (length(s) == 1) {
		if (!is.na(s)) selected <- s # 1 scale
	} else if (length(s) == 2) {
		selected <- (1:length(cwtObject$periods))[
				s[1] <= cwtObject$periods & cwtObject$periods <= s[2]] # set of scales
	} else stop("ERROR: scale is not specified correctly")
	out <- numeric(length(cwtObject$x))
	for (i in 1:length(out)) {
			out[i] <- atan2(sum(Im(cwtObject$transform[selected, i])), 
					sum(Re(cwtObject$transform[selected, i])))
	}
	return(out)
}

cohPhaseDiff <- function(series1, series2, dj=(-1), 
		s=NA, dt=(-1), s0=(-1), J=(-1), ...) { # Torrence 1999 Eqn. A3
	if (dj == -1) stop("dj must be specified by the user")
	if (dt == -1) dt <- series1$t[2] - series1$t[1]
	startT <- max(c(min(series1$t), min(series2$t)))
	endT <- min(c(max(series1$t), max(series2$t)))
	T1 <- startT <= series1$t & series1$t <= endT
	T2 <- startT <= series2$t & series2$t <= endT
	if (s0 == -1) s0 <- 2 * dt
	if (J == -1) J <- floor(1 / dj * log2(sum(T1) * dt / s0))
	cwt1 <- cwt(series1$x, dt, dj, J=J, ...)
	cwt2 <- cwt(series2$x, dt, dj, J=J, ...)
	Wxy <- cwt1$transform[ , T1] * Conj(cwt2$transform[ , T2])
	sWxy <- smoothCWT(normalizePower(Wxy, cwt1$scales), 
			dt, dj, cwt1$scales)
	phaseCOH <- atan2(Im(sWxy), Re(sWxy))	
	if (length(s) == 1 & !is.na(s[1])) {
		out <- phaseCOH[s, ]
	} else if (length(s) == 2) {
		out <- apply(phaseCOH[s[1] <= cwt1$periods & cwt1$periods <= s[2], ], 
				2, mean)
	} else stop("ERROR: scale is not specified correctly")
	return(out)
}

get_phase_coh_time <- function(data, pairs_df, start_time, end_time){
  data <- filter(data, time >= start_time & time < end_time)
  avg_df <- data.frame(idx = NA, 
                       ref_prov = NA, 
                       other_prov = NA, 
                       est = NA, 
                       yr_midpoint = NA)
  
  for(i in 1:dim(pairs_df)[1]){
    temp_pair <- slice(pairs_df, i)
    temp_df  <-   filter(data, ref_prov == temp_pair$ref_prov &
                           other_prov == temp_pair$other_prov)
    # Checking if data is presented for time-range 
    if(all(is.na(temp_df$est)) == TRUE) next()
    x_est <- mean_phase(temp_df$est, na.rm = T)
    
    # Storing
    avg_df[i, 1] <- i
    avg_df[i, 2] <- temp_pair$ref_prov
    avg_df[i, 3] <- temp_pair$other_prov
    avg_df[i, 4] <- x_est
    avg_df[i, 5] <- median(c(start_time, end_time))
  }
  
  return(avg_df)
}


phaseToTime <- function(phase, p, dt) { # p=period
	phase / (2 * pi) * p * 1/dt
}

# functions to extract estimates 
extract_avg_coh_phase_coi_updated <- function(dat, scales, coi, low = 0, high = 2^12) { 
  inds <- which(scales > low & scales <= high)
  for (i in 1:length(scales)) { # dat rows = scales, cols = time
    dat[i, coi < scales[i]]  <- NA
  }
  
  avg_est = data.frame(
    est = mean_phase(dat[inds, ], na.rm = T)
  )
  colnames(avg_est) <- "est"  
  return(avg_est)
}


extract_coh_phase_time_coi_updated <- function(dat, time, scales, coi, low = 0, high = 2^12) {
  inds <- which(scales > low & scales <= high)
  dat[ , coi < low]  <- NA
  diff_avg <- data.frame(time = time,
                        est = apply(dat[inds, ], 2, mean_phase, na.rm = T))
  
 diff_avg$est[is.nan(diff_avg$est)] <- NA 
  return(diff_avg) 
}


mean_phase <- function(phase, na.rm = F) { 
  atan2(mean(sin(phase), na.rm = na.rm), mean(cos(phase), na.rm = na.rm))
}

#### Vector functions
get_bearing_est <- function(data, coord_data) {
  data <- left_join(data, coord_data, 
                    by = c("ref_prov" = "prov1", "other_prov" = "prov2"))
  #require(swfscMisc) 
  alphas <- rename(data, coh_phase = est, lat = lat1, long = long1) %>%
    select(coh_phase, lat, long, lat2, long2, ref_prov, other_prov) %>%
    mutate(
      delta_x = long2 - long,
      delta_y = lat2 - lat,
      bearing = atan(delta_x / delta_y) * 180/pi,
      bearing = ifelse(delta_x > 0 & delta_y < 0, 180 + bearing, bearing),
      bearing = ifelse(delta_x < 0 & delta_y < 0, 180 + bearing, bearing),
      bearing = ifelse(delta_x < 0 & delta_y > 0, 360 + bearing, bearing)
    )
  
  return(select(alphas, -delta_x, -delta_y, -lat2, -long2))
}

# function to estimate the deltas
get_delta_est <- function(dat) {
  deltas <- as.data.frame(dat) %>%
    rename(alpha = bearing) %>% 
    mutate(
      delta_x = ifelse(alpha > 0 &  alpha <= 90,
                       coh_phase * sin(alpha * pi/180), NA),
      delta_y = ifelse(alpha > 0 &  alpha <= 90,
                       coh_phase * sin((90 - alpha) * pi/180), NA),
      delta_x = ifelse(alpha > 90  & alpha <= 180,
                       coh_phase * sin((180 - alpha) * pi/180), delta_x),
      delta_y = ifelse(alpha > 90  & alpha <= 180,
                       -1 * coh_phase * sin((alpha - 90) * pi/180), delta_y),
      delta_x = ifelse(alpha > 180 & alpha <= 270, 
                       -1 * coh_phase * sin((alpha - 180) * pi/180), delta_x), 
      delta_y = ifelse(alpha > 180 & alpha <= 270,
                       -1 * coh_phase * sin((270 - alpha) * pi/180), delta_y), 
      delta_x = ifelse(alpha > 270 & alpha <= 360, 
                       -1 * coh_phase * sin((360 - alpha) * pi/180), delta_x), 
      delta_y = ifelse(alpha > 270 & alpha <= 360,
                       coh_phase * sin((alpha - 270) * pi/180 ), delta_y)
    )
  
  return(deltas)
}
