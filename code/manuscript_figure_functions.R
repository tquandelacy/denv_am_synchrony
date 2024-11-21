library(ggplot2)

plot_fig1 <- function(prov_data, loc_data, mlt_countries) {
  inc_df <- prov_data %>%
    dplyr::select(country, province, time, Ts) %>%
    tidyr::unnest(cols = c(time, Ts)) %>%
    left_join(loc_data, by = c("country" = "country", "province" = "co_province")) %>%
    arrange(country_order, lat) %>%
    mutate(cases = exp(Ts + 1))

  # setting up the normalized incidence data
  ann_df <- prov_data %>%
    dplyr::select(country, province, ann_wave) %>%
    tidyr::unnest(cols = c(ann_wave)) %>%
    mutate(wavelet2 = ifelse(wavelet >= 4, 4,
      ifelse(wavelet <= -4, -4, wavelet)
    )) %>%
    left_join(loc_data, by = c("country" = "country", "province" = "co_province"))

  mlt_df <- prov_data %>%
    filter(country %in% mlt_countries) %>%
    dplyr::select(country, province, mlt_wave) %>%
    tidyr::unnest(cols = c(mlt_wave)) %>%
    mutate(wavelet2 = ifelse(wavelet >= 4, 4,
      ifelse(wavelet <= -4, -4, wavelet)
    )) %>%
    left_join(loc_data, by = c("country" = "country", "province" = "co_province"))

  options(scipen = 999)
  my_breaks <- c(1, 10, 100, 1000, 10000, 10000, 100000, 1000000)


  p1 <- ggplot(inc_df, aes(y = factor(reorder(province_rank, country_order)), x = time)) +
    geom_tile(aes(fill = cases)) +
    facet_grid(reorder(country, country_order) ~ .,
      drop = T,
      space = "free", scales = "free", switch = "y"
    ) +
    geom_vline(xintercept = 1985:2019, colour = "black", linewidth = 0.85, alpha = 0.5) +
    scale_fill_distiller(
      direction = -1, palette = "Spectral", trans = "log10",
      breaks = my_breaks, labels = my_breaks, limits = c(1, 1000000)
    ) +
    labs(y = "Department/Province/State", x = " ", fill = "Dengue cases", tag = "A)") +
    scale_y_discrete(position = "left", name = "Country", expand = c(0, 0), drop = T) +
    scale_x_continuous(expand = c(0, 0), breaks = 1985:2019) +
    theme(
      panel.background = element_rect(fill = "white"),
      axis.text.y = element_text(size = 26),
      axis.ticks.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      axis.title.y.left = element_text(size = 40),
      axis.text.x = element_text(size = 20),
      axis.title.x = element_text(size = 40),
      strip.text = element_text(size = 28),
      strip.background = element_rect(
        fill = "white", linetype = "solid",
        color = NA, linewidth = 1
      ),
      legend.position = c(0.07, 0.30),
      legend.key.size = unit(2.5, "cm"),
      legend.key.width = unit(1.5, "cm"),
      legend.title = element_text(size = 30, vjust = 1),
      legend.text = element_text(size = 25),
      plot.margin = margin(0.5, 2, 0.5, 3, "cm"),
      plot.tag = element_text(face = "bold", colour = "black", size = 50)
    )

  p2 <- ggplot(ann_df, aes(y = factor(reorder(province_rank, country_order)), x = time)) +
    geom_tile(aes(fill = wavelet2)) +
    facet_grid(reorder(country, country_order) ~ .,
      drop = T,
      space = "free", scales = "free", switch = "y"
    ) +
    geom_vline(xintercept = 1985:2019, colour = "black", size = 0.85, alpha = 0.5) +
    scale_fill_distiller(direction = -1, palette = "RdBu", labels = c("\u2264 -4", "-2", "0", "2", "\u2265 4")) +
    labs(y = "Department/Province/State", x = " ", fill = "Seasonal cycle", tag = "B)") +
    scale_y_discrete(position = "left", name = "Country", expand = c(0, 0), drop = T) +
    scale_x_continuous(expand = c(0, 0), breaks = 1985:2019) +
    theme(
      panel.background = element_rect(fill = "white"),
      axis.text.y = element_text(size = 26),
      axis.ticks.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      axis.title.y.left = element_text(size = 40),
      axis.text.x = element_text(size = 20),
      axis.title.x = element_text(size = 40),
      strip.text = element_text(size = 28),
      strip.background = element_rect(
        fill = "white", linetype = "solid",
        color = NA, linewidth = 1
      ),
      legend.position = c(0.07, 0.30),
      legend.key.size = unit(2.5, "cm"),
      legend.key.width = unit(1.5, "cm"),
      legend.title = element_text(size = 30, vjust = 1),
      legend.text = element_text(size = 25, hjust = 1),
      plot.margin = margin(0.5, 2, 0.25, 3, "cm"), # t,r,b,l
      plot.tag = element_text(face = "bold", colour = "black", size = 50)
    )

  p3 <- ggplot(mlt_df, aes(y = factor(reorder(province_rank, country_order)), x = time)) +
    geom_tile(aes(fill = wavelet2)) +
    facet_grid(reorder(country, country_order) ~ .,
      drop = T,
      space = "free", scales = "free", switch = "y"
    ) +
    geom_vline(xintercept = 1985:2019, colour = "black", size = 0.85, alpha = 0.5) +
    scale_fill_distiller(direction = -1, palette = "RdBu", labels = c("\u2264 -4", "-2", "0", "2", "\u2265 4")) +
    labs(y = "Department/Province/State", x = "Time", fill = "Multiyear cycle", tag = "C)") +
    scale_y_discrete(position = "left", name = "Country", expand = c(0, 0), drop = T) +
    scale_x_continuous(expand = c(0, 0), breaks = 1985:2019) +
    theme(
      panel.background = element_rect(fill = "white"),
      axis.text.y = element_text(size = 26),
      axis.ticks.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      axis.title.y.left = element_text(size = 40),
      axis.text.x = element_text(size = 20),
      axis.title.x = element_text(size = 40),
      strip.text = element_text(size = 28),
      strip.background = element_rect(
        fill = "white",
        color = NA, linewidth = 1
      ),
      legend.position = c(0.07, 0.30),
      legend.key.size = unit(2.5, "cm"),
      legend.key.width = unit(1.5, "cm"),
      legend.title = element_text(size = 30, vjust = 1),
      legend.text = element_text(size = 25, hjust = 1),
      plot.margin = margin(0.5, 2, 0.5, 3, "cm"),
      plot.tag = element_text(face = "bold", colour = "black", size = 50)
    )

  gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
}

# Figure 2: Seasonal wavelet pattern by country
plot_fig2a <- function(data, prov_coords, caribbean_countries) {
  library(RColorBrewer)
  mycolours <- c(
    "#543005", "#814B09", "#AC6F20", # jan - mar
    "#CD9E51", "#E5CC90", "#F1E8C6", # apr - jun
    "#CBE9E1", "#93D4CA", "#57AFA5", # jul - sept
    "#22857D", "#005E55", "#003C30"
  ) # oct - dec

  mybreaks <- as.numeric(1:12)
  data$mycolourscheme <- mycolours[findInterval(data$month, vec = mybreaks)]

  adm_data <- data %>% filter(!(country %in% caribbean_countries))
  island_data <- data %>%
    filter((country %in% caribbean_countries)) %>%
    left_join(prov_coords, by = c("country" = "country", "co_province" = "co_province"))

  # Country polygons
  library(sf)
  am_country_map <- st_read(
    dsn = "data",
    layer = "am_low_res_country",
    stringsAsFactors = FALSE
  )
  am_country_map$peak_col <- "grey90"
  # am_country_map$peak_col[am_country_map$co_prov == "dr_all"] <- "#57AFA5" # SEPT
  # am_country_map$peak_col[am_country_map$co_prov == "pr_all"] <- "#22857D" # Oct
  # am_country_map$peak_col[am_country_map$co_prov == "bb_all"] <- "#005E55" # Nov

  # Administrative-level 1 locations in the study
  am_adm_map <- st_read(dsn = "data", layer = "am_low_res_province_map", stringsAsFactors = FALSE)
  # Fixing labeling issues in adm1 naming
  am_adm_map$co_prov[am_adm_map$co_prov == "br_rond^onia"] <- "br_rondonia"
  am_adm_map$co_prov[am_adm_map$co_prov == "br_s~ao_paulo"] <- "br_sao_paulo"
  am_adm_map$co_prov[am_adm_map$co_prov == "br_maranh~ao"] <- "br_maranhao"
  am_adm_map$co_prov[am_adm_map$co_prov == "gt_peten"] <- "gt_el_peten"
  am_adm_map$co_prov[am_adm_map$co_prov == "gt_quezaltenango"] <- "gt_quetzaltenango"
  am_adm_map$co_prov[am_adm_map$co_prov == "sv_caba~nas"] <- "sv_cabanas"
  am_adm_map$co_prov[am_adm_map$co_prov == "pa_ng\"obe_bugle"] <- "pa_ngobe_bugle"
  am_adm_map$co_prov[am_adm_map$co_prov == "co_nari~no"] <- "co_narino"
  am_adm_map$co_prov[am_adm_map$co_prov == "ec_ca~nar"] <- "ec_canar"

  # joining seasonality data to spatial data
  am_adm_map <- left_join(am_adm_map, adm_data, by = c("co_prov" = "co_province"))
  am_adm_map$mycolourscheme[is.na(am_adm_map$mycolourscheme)] <- "grey50"

  # points for the islands
  island_points <- sf::st_as_sf(island_data,
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )

  par(mfrow = c(1, 1), mar = c(0, 0.25, 0, 0.25), oma = c(0.5, 0.5, 0.5, 0.5))
  plot(st_geometry(am_country_map),
    bg = "transparent", col = am_country_map$peak_col, border = "black",
    lwd = 2, cex = 5, ylim = c(-50, 30), xlim = c(-115, -36)
  )
  plot(st_geometry(am_adm_map),
    bg = "transparent", lwd = 2,
    col = am_adm_map$mycolourscheme, border = "white",
    cex = 5, ylim = c(-50, 30), xlim = c(-115, -36), add = T
  )
  plot(st_geometry(am_country_map),
    bg = "transparent", col = NA, border = "grey70",
    lwd = 4, cex = 5, ylim = c(-50, 30), xlim = c(-115, -36), add = T
  )

  plot(st_geometry(island_points), add = T, pch = 19, cex = 3, col = island_points$mycolourscheme)
  # Add legend
  legend(-115, -25,
    legend = c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Excluded from analysis"
    ),
    title = "Seasonal Dengue \nAverage Peak Month",
    fill = c(mycolours, "grey50"), border = "grey90",
    pt.cex = 11, cex = 4,
    x.intersp = 0.75, y.intersp = 0.65,
    seg.len = 0.85, inset = -0.1, bty = "n"
  )
}


plot_fig2b <- function(seas_data, loc_data) {
  # Panel 1
  seas_df <- seas_data %>%
    dplyr::select(country, province, mean_wave) %>%
    left_join(loc_data, by = c("country" = "country", "province" = "co_province"))

  seas_df$month <- rep(1:12, length(unique(seas_df$province)))
  seas_df <- seas_df %>% mutate(
    co_color = ifelse(country_order == "1", "#50755d", # MX
      ifelse(country_order == "2", "#98d3cb", # GT
        ifelse(country_order == "3", "#d5adab", # SV
          ifelse(country_order == "4", "#5f8069", # DR
            ifelse(country_order == "5", "#31828a", # PR
              ifelse(country_order == "6", "#b8735c", # BB
                ifelse(country_order == "7", "#bbb4ba", # Costa Rica
                  ifelse(country_order == "8", "#91d6de", # Panama
                    ifelse(country_order == "9", "#887fa6", # Venezuela
                      ifelse(country_order == "10", "#61694e", # Colombia
                        ifelse(country_order == "11", "#ebb040", # Ecuador
                          ifelse(country_order == "12", "#e56565", # Peru
                            ifelse(country_order == "13", "#f5b176", # Brazil
                              ifelse(country_order == "14", "#89843e", "white")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ), # Argentina
    panel_name = ifelse(country == "mx", "Mexico",
      ifelse(country == "gt", "Guatemala",
        ifelse(country == "sv", "El Salvador",
          ifelse(country == "dr", "Dominican Republic",
            ifelse(country == "pr", "Puerto Rico",
              ifelse(country == "bb", "Barbados",
                ifelse(country == "cr", "Costa Rica",
                  ifelse(country == "pa", "Panama",
                    ifelse(country == "ve", "Venezuela",
                      ifelse(country == "co", "Colombia",
                        ifelse(country == "pe", "Peru",
                          ifelse(country == "ec", "Ecuador",
                            ifelse(country == "br", "Brazil",
                              ifelse(country == "ar", "Argentina", NA)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  p1 <- ggplot(seas_df, aes(y = mean_wave, x = factor(month), group = factor(province), colour = co_color)) +
    geom_point(size = 2, alpha = 0.65) +
    geom_line(alpha = 0.5, linewidth = 1, aes(colour = co_color)) +
    ylim(-2.4, 2.4) +
    geom_hline(yintercept = 0, colour = "grey70", alpha = 0.5) +
    facet_wrap(~ reorder(panel_name, country_order), ncol = 4) +
    scale_x_discrete(
      breaks = 1:12,
      labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
    ) +
    theme_bw() +
    xlab("Month") +
    ylab("Average wavelet") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 26),
      axis.title = element_text(size = 30, face = "bold"),
      strip.text.x = element_text(size = 26, face = "bold"),
      strip.background = element_rect(fill = "white", colour = "white"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    scale_colour_identity()
  # plot
  p1
}

# Plot figure 3 -Boxplot distribution of multiannual wavelets
plot_fig3 <- function(ann_data, # Annual wavelet dataset
                      mlt_data, # Multiannual wavelet dataset
                      sync_data, # 5-year average synchrony
                      temp_data, # temperature multiannual wavelet
                      ppt_data, # precipitation multiannual wavelet
                      enso_data # ENSO multiannual wavelet
) {
  # Total locations for each month for annual and multiannual time-scales
  total_ann <- ann_data %>%
    group_by(time) %>%
    summarize(n_location = n()) %>%
    mutate(time_scale = 1)

  total_mlt <- mlt_data %>%
    group_by(time) %>%
    summarize(n_location = n()) %>%
    mutate(time_scale = 2)

  total_n <- bind_rows(total_ann, total_mlt)

  p1 <- ggplot(total_n, aes(y = n_location, x = time)) +
    geom_line(aes(linetype = factor(time_scale))) +
    geom_line(data = total_mlt, aes(y = n_location, x = time), linetype = 2) +
    labs(title = "Dengue", y = "Number of\n locations", x = " ", tag = "A)") +
    scale_y_continuous(limits = c(0, 250)) +
    scale_linetype_manual(
      values = c("solid", "dashed"),
      labels = c("Annual", "Multiannual"), name = " "
    ) +
    theme_minimal() +
    scale_x_discrete(
      expand = c(0, 0),
      labels = c(
        1985, " ", " ", 1988, " ", " ", 1991, " ", " ",
        1994, " ", " ", 1997, " ", " ", 2000, " ", " ",
        2003, " ", " ", 2006, " ", " ", 2009, " ", " ",
        2012, " ", " ", 2015, " ", " ", 2018
      )
    ) +
    theme(
      panel.grid.minor = element_blank(),
      title = element_text(face = "bold", colour = "black", size = 22),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      plot.tag = element_text(face = "bold", colour = "black", size = 30),
      plot.margin = margin(0.1, 2, 0.1, 1, "cm"),
      legend.position = c(0.1, 0.90),
      legend.text = element_text(size = 16),
      legend.key.size = unit(1, "cm")
    )

  wave_max <- 8.1
  wave_min <- -1 * wave_max

  # Multiannual dengue wavelet boxplot
  p2 <- ggplot(
    mlt_data %>% filter(time <= 2018),
    aes(y = wavelet, x = time, group = time)
  ) +
    geom_boxplot(
      aes(
        ymin = min(wavelet),
        lower = quantile(wavelet, 0.25),
        middle = quantile(wavelet, 0.50),
        upper = quantile(wavelet, 0.75),
        ymax = max(wavelet)
      ),
      outlier.shape = NA,
      fill = "white", colour = "#00AFBB",
      orientation = "x"
    ) +
    scale_y_continuous(expand = c(0.05, 0.05), limits = c(-5, 5)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1985, 2018, by = 3)) +
    labs(title = " ", y = "Monthly distribution", x = "") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      title = element_text(face = "bold", colour = "black", size = 22),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      plot.tag = element_text(face = "bold", colour = "black", size = 30),
      plot.margin = margin(0.1, 2, 0.5, 1, "cm")
    ) # t,r,b,l

  # Multiannual temperature wavelet boxplot
  twave_max <- max(ceiling(temp_data$wavelet))
  twave_min <- -1 * twave_max
  p3 <- ggplot(
    temp_data %>% filter(time <= 2018),
    aes(y = wavelet, x = time, group = time)
  ) +
    geom_boxplot(
      aes(
        ymin = min(wavelet),
        lower = quantile(wavelet, 0.25),
        middle = quantile(wavelet, 0.50),
        upper = quantile(wavelet, 0.75),
        ymax = max(wavelet)
      ),
      outlier.shape = NA,
      fill = "white", colour = "#E7B800",
      orientation = "x"
    ) +
    scale_y_continuous(expand = c(0.05, 0.05), limits = c(-4, 4)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1985, 2018, by = 3)) +
    labs(
      title = "Temperature", y = "Monthly distribution", x = "",
      tag = "B)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      title = element_text(face = "bold", colour = "black", size = 22),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      plot.tag = element_text(face = "bold", colour = "black", size = 30),
      plot.margin = margin(0.5, 2, 0.5, 1, "cm")
    ) # t,r,b,l

  # Precipitation
  pwave_max <- max(ceiling(ppt_data$wavelet))
  pwave_min <- -1 * pwave_max
  p4 <- ggplot(
    ppt_data %>% filter(time <= 2018),
    aes(y = wavelet, x = time, group = time)
  ) +
    geom_boxplot(
      aes(
        ymin = min(wavelet),
        lower = quantile(wavelet, 0.25),
        middle = quantile(wavelet, 0.50),
        upper = quantile(wavelet, 0.75),
        ymax = max(wavelet)
      ),
      outlier.shape = NA,
      fill = "white", colour = "#FC4E07",
      orientation = "x"
    ) +
    scale_y_continuous(expand = c(0.05, 0.05), limits = c(-2.2, 2.2)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1985, 2018, by = 3)) +
    labs(
      title = "Rainfall", y = "Monthly distribution", x = "",
      tag = "C)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      title = element_text(face = "bold", colour = "black", size = 22),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      plot.tag = element_text(face = "bold", colour = "black", size = 30),
      plot.margin = margin(0.5, 2, 0.5, 1, "cm")
    ) # t,r,b,l

  ewave_max <- max(ceiling(enso_data$wavelet))
  ewave_min <- -1 * ewave_max
  p5 <- ggplot(enso_data %>% filter(time <= 2018)) +
    geom_line(aes(y = wavelet, x = time), colour = "steelblue", linewidth = 0.8) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1985, 2018, by = 3)) +
    scale_y_continuous(expand = c(0.05, 0.05), limits = c(-6, 6)) +
    labs(
      title = "El Niño Southern Oscillation", y = "Wavelet coefficient", x = "",
      tag = "D)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      title = element_text(face = "bold", colour = "black", size = 22),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      plot.tag = element_text(face = "bold", colour = "black", size = 30),
      plot.margin = margin(0.5, 2, 0.5, 1, "cm")
    ) # t,r,b,l

  # Synchrony over time
  sync_data <- sync_data %>%
    data.frame()

  avg_sync <- sync_data %>%
    group_by(time_scale) %>%
    summarize(mean_sync = mean(avg_est, na.rm = T))

  extra_yrs <- data.frame(
    ref_prov = rep(NA, 8),
    yr_midpoint = rep(NA, 8),
    avg_est = rep(NA, 8),
    year = rep(c(1985, 1986, 2017, 2018), 2),
    time_scale = as.character(c(1, 1, 1, 1, 2, 2, 2, 2))
  )
  sync_data <- bind_rows(sync_data, extra_yrs) %>%
    arrange(year) %>%
    mutate(year = as.character(year))


  p6 <- ggplot(sync_data, aes(y = avg_est, x = year, fill = factor(time_scale))) +
    geom_boxplot(
      aes(
        ymin = quantile(avg_est, 0.025, na.rm = T),
        lower = quantile(avg_est, 0.25, na.rm = T),
        middle = quantile(avg_est, 0.50, na.rm = T),
        upper = quantile(avg_est, 0.75, na.rm = T),
        ymax = quantile(avg_est, 0.975, na.rm = T)
      ),
      outlier.shape = NA, width = 0.5
    ) +
    geom_hline(data = avg_sync, aes(
      yintercept = mean_sync, group = factor(time_scale), linetype = c("dashed", "dashed"),
      colour = c("#3B9AB2", "#F21A00")
    ), linewidth = c(0.8, 0.8)) +
    scale_fill_manual(
      values = c("1" = "#3B9AB2", "2" = "#F21A00"),
      labels = c("Annual (8-16 mo.)", "Multiannual (17 mo.+)"),
      name = " "
    ) +
    scale_linetype_manual(values = c("dashed"), labels = "Regional average", name = "") +
    scale_y_continuous(limits = c(-0.1, 0.75), expand = c(0, 0)) +
    scale_x_discrete(
      expand = c(0, 0),
      labels = c(
        1985, " ", " ", 1988, " ", " ", 1991, " ", " ",
        1994, " ", " ", 1997, " ", " ", 2000, " ", " ",
        2003, " ", " ", 2006, " ", " ", 2009, " ", " ",
        2012, " ", " ", 2015, " ", " ", 2018
      )
    ) +
    labs(y = "Average Synchrony", x = "Year", tag = "E)") +
    theme_minimal() +
    theme(
      legend.position = c(0.2, 0.95),
      legend.key.size = unit(3, "cm"), legend.box = "horizontal",
      legend.text = element_text(size = 16),
      panel.grid.minor = element_blank(),
      title = element_text(face = "bold", colour = "black", size = 22),
      axis.title = element_text(size = 20),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      plot.tag = element_text(face = "bold", colour = "black", size = 30),
      plot.margin = margin(0.5, 2, 0.5, 1, "cm")
    ) +
    guides(fill = guide_legend(ncol = 2))

  library(cowplot)
  plot_grid(p1, p2, p3, p4, p5, p6,
    align = "v",
    nrow = 6, ncol = 1, rel_heights = c(0.75, 1, 1, 1, 1, 1)
  )
}

# Figure 4 explanatory figure
# inputs: data = prov_df
plot_fig4 <- function(data, ann_coh, mlt_coh, ann_phase, mlt_phase, prov_coords) {
  ann_df <- (data) %>%
    select(country, province, ann_wave) %>%
    tidyr::unnest(cols = c(ann_wave))

  ann_df <- left_join(ann_df, prov_coords,
    by = c("province" = "co_province")
  )

  mlt_df <- (data) %>%
    select(country, province, mlt_wave) %>%
    tidyr::unnest(cols = c(mlt_wave))

  mlt_df <- left_join(mlt_df, prov_coords,
    by = c("province" = "co_province")
  )

  # Subsetting to Puerto Rico as the reference location
  loc_ann <- ann_df %>% filter(province == "pr_all" | province == "ve_merida" |
    province == "br_sao_paulo" | province == "mx_aguascalientes")
  loc_mlt <- mlt_df %>% filter(province == "pr_all" | province == "ve_merida" | province == "br_sao_paulo" |
    province == "mx_aguascalientes")

  loc_ann_coh <- ann_coh %>%
    filter(ref_prov == "pr_all" & (other_prov == "ve_merida" |
      other_prov == "br_sao_paulo" |
      other_prov == "mx_aguascalientes")) %>%
    mutate(time_scale = "annual")
  loc_mlt_coh <- mlt_coh %>%
    filter(ref_prov == "pr_all" & (other_prov == "ve_merida" |
      other_prov == "br_sao_paulo" |
      other_prov == "mx_aguascalientes")) %>%
    mutate(time_scale = "multiannual")

  locs_coh <- bind_rows(loc_ann_coh, loc_mlt_coh)


  loc_ann_pd <- ann_phase %>%
    filter(ref_prov == "pr_all" & (other_prov == "ve_merida" |
      other_prov == "br_sao_paulo" |
      other_prov == "mx_aguascalientes")) %>%
    mutate(
      time_scale = "annual",
      abs_lag = abs(est_lag)
    )
  loc_mlt_pd <- mlt_phase %>%
    filter(ref_prov == "pr_all" & (other_prov == "ve_merida" |
      other_prov == "br_sao_paulo" |
      other_prov == "mx_aguascalientes")) %>%
    mutate(
      time_scale = "multiannual",
      abs_lag = abs(est_lag)
    )

  locs_pd <- bind_rows(loc_ann_pd, loc_mlt_pd)

  par(mfrow = c(3, 2), mar = c(2, 3, 2, 2), oma = c(1, 2, 1, 1))
  # Annual Wavelet
  plot(filter(loc_ann, time < 2016 & time >= 2005 & province == "pr_all")$time,
    filter(loc_ann, time < 2016 & time >= 2005 & province == "pr_all")$wavelet,
    col = "black", type = "l", lwd = 1.5,
    xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n",
    ylim = c(-5, 5), xlim = c(2005, 2016)
  )
  lines(filter(loc_ann, time < 2016 & time >= 2005 & province == "mx_aguascalientes")$time,
    filter(loc_ann, time < 2016 & time >= 2005 & province == "mx_aguascalientes")$wavelet,
    col = "darkgreen", type = "l", lwd = 1.5
  )
  lines(filter(loc_ann, time < 2016 & time >= 2005 & province == "ve_merida")$time,
    filter(loc_ann, time < 2016 & time >= 2005 & province == "ve_merida")$wavelet,
    col = "#CC9900", type = "l", lwd = 1.5
  )
  lines(filter(loc_ann, time < 2016 & time >= 2005 & province == "br_sao_paulo")$time,
    filter(loc_ann, time < 2016 & time >= 2005 & province == "br_sao_paulo")$wavelet,
    col = "cornflowerblue", type = "l", lwd = 1.5
  )
  mtext("Annual Wavelet", side = 2, line = 3)
  mtext("Time", side = 1, line = 2)
  box("plot", bty = "l", lwd = 2)
  ## X Axis
  axis(side = 1, lwd = 0, lwd.ticks = 2, cex.axis = 1.5)
  ## Y axis
  axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2, cex.axis = 1.5)

  # Multiannual wavelet
  plot(filter(loc_mlt, time < 2016 & time >= 2005 & province == "pr_all")$time,
    filter(loc_mlt, time < 2016 & time >= 2005 & province == "pr_all")$wavelet,
    col = "black", type = "l", lwd = 1.5, lty = 2,
    xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n",
    ylim = c(-5, 5), xlim = c(2005, 2016)
  )
  lines(filter(loc_mlt, time < 2016 & time >= 2005 & province == "mx_aguascalientes")$time,
    filter(loc_mlt, time < 2016 & time >= 2005 & province == "mx_aguascalientes")$wavelet,
    col = "darkgreen", type = "l", lwd = 1.5, lty = 2
  )
  lines(filter(loc_mlt, time < 2016 & time >= 2005 & province == "ve_merida")$time,
    filter(loc_mlt, time < 2016 & time >= 2005 & province == "ve_merida")$wavelet,
    col = "#CC9900", type = "l", lwd = 1.5, lty = 2
  )
  lines(filter(loc_mlt, time < 2016 & time >= 2005 & province == "br_sao_paulo")$time,
    filter(loc_mlt, time < 2016 & time >= 2005 & province == "br_sao_paulo")$wavelet,
    col = "cornflowerblue", type = "l", lwd = 1.5, lty = 2
  )
  mtext("Multiannual Wavelet", side = 2, line = 3)
  mtext("Time", side = 1, line = 2)
  box("plot", bty = "l", lwd = 2)
  ## X Axis
  axis(side = 1, lwd = 0, lwd.ticks = 2, cex.axis = 1.5)
  ## Y axis
  axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2, cex.axis = 1.5)

  # Coherence
  plot(filter(locs_coh, other_prov == "mx_aguascalientes" & time_scale == "annual")$time,
    filter(locs_coh, other_prov == "mx_aguascalientes" & time_scale == "annual")$est,
    col = "darkgreen", type = "l", lwd = 1.5,
    xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n",
    ylim = c(0, 1), xlim = c(2005, 2016)
  )
  lines(filter(locs_coh, other_prov == "mx_aguascalientes" & time_scale == "multiannual")$time,
    filter(locs_coh, other_prov == "mx_aguascalientes" & time_scale == "multiannual")$est,
    col = "darkgreen", type = "l", lwd = 1.5, lty = 2
  )
  lines(filter(locs_coh, other_prov == "ve_merida" & time_scale == "annual")$time,
    filter(locs_coh, other_prov == "ve_merida" & time_scale == "annual")$est,
    col = "#CC9900", type = "l", lwd = 1.5
  )
  lines(filter(locs_coh, other_prov == "ve_merida" & time_scale == "multiannual")$time,
    filter(locs_coh, other_prov == "ve_merida" & time_scale == "multiannual")$est,
    col = "#CC9900", type = "l", lwd = 1.5, lty = 2
  )
  lines(filter(locs_coh, other_prov == "br_sao_paulo" & time_scale == "annual")$time,
    filter(locs_coh, other_prov == "br_sao_paulo" & time_scale == "annual")$est,
    col = "cornflowerblue", type = "l", lwd = 1.5
  )
  lines(filter(locs_coh, other_prov == "br_sao_paulo" & time_scale == "multiannual")$time,
    filter(locs_coh, other_prov == "br_sao_paulo" & time_scale == "multiannual")$est,
    col = "cornflowerblue", type = "l", lwd = 1.5, lty = 2
  )
  mtext("Coherence", side = 2, line = 3)
  mtext("Time", side = 1, line = 2)
  box("plot", bty = "l", lwd = 2)
  ## X Axis
  axis(side = 1, lwd = 0, lwd.ticks = 2, cex.axis = 1.5)
  ## Y axis
  axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2, cex.axis = 1.5)

  # Absolute phase difference
  plot(filter(locs_pd, other_prov == "mx_aguascalientes" & time_scale == "annual")$time,
    filter(locs_pd, other_prov == "mx_aguascalientes" & time_scale == "annual")$abs_lag,
    col = "darkgreen", type = "l", lwd = 1.5,
    xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n",
    ylim = c(0, 12), xlim = c(2005, 2016)
  )
  lines(filter(locs_pd, other_prov == "mx_aguascalientes" & time_scale == "multiannual")$time,
    filter(locs_pd, other_prov == "mx_aguascalientes" & time_scale == "multiannual")$abs_lag,
    col = "darkgreen", type = "l", lwd = 1.5, lty = 2
  )
  lines(filter(locs_pd, other_prov == "ve_merida" & time_scale == "annual")$time,
    filter(locs_pd, other_prov == "ve_merida" & time_scale == "annual")$abs_lag,
    col = "#CC9900", type = "l", lwd = 1.5
  )
  lines(filter(locs_pd, other_prov == "ve_merida" & time_scale == "multiannual")$time,
    filter(locs_pd, other_prov == "ve_merida" & time_scale == "multiannual")$abs_lag,
    col = "#CC9900", type = "l", lwd = 1.5, lty = 2
  )
  lines(filter(locs_pd, other_prov == "br_sao_paulo" & time_scale == "annual")$time,
    filter(locs_pd, other_prov == "br_sao_paulo" & time_scale == "annual")$abs_lag,
    col = "cornflowerblue", type = "l", lwd = 1.5
  )
  lines(filter(locs_pd, other_prov == "br_sao_paulo" & time_scale == "multiannual")$time,
    filter(locs_pd, other_prov == "br_sao_paulo" & time_scale == "multiannual")$abs_lag,
    col = "cornflowerblue", type = "l", lwd = 1.5, lty = 2
  )
  mtext("Absolute Lag (Months)", side = 2, line = 3)
  mtext("Time", side = 1, line = 2)
  box("plot", bty = "l", lwd = 2)
  ## X Axis
  axis(side = 1, lwd = 0, lwd.ticks = 2, cex.axis = 1.5)
  ## Y axis
  axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2, cex.axis = 1.5)

  # Plot code for legend
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  plot_colors <- c("black", "darkgreen", "#CC9900", "cornflowerblue")
  legend(0.6, 1.4,
    inset = 0,
    legend = c("Puerto Rico (reference)", "Aguascalientes, Mexico", "Merida, Venezuela", "Sao Paulo, Brazil"),
    title = "Locations",
    col = plot_colors, lwd = 1.5, cex = 1.5, horiz = F, bty = "n",
    x.intersp = 0.75
  )

  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend(0.6, 1.4,
    title = "Time scale",
    legend = c("Annual", "Multiannual"),
    col = "black", lty = c(1, 2),
    lwd = 1.5, cex = 1.5, horiz = F, bty = "n",
    x.intersp = 0.75
  )
}

# Figure 5: the synchrony plot for all pairs
# Fig 5A plots panels A & B - average coherence (A) & phase difference by distance (B)
plot_fig5a <- function(ann_coh_dat, ann_phase_dat,
                       mlt_coh_dat, mlt_phase_dat,
                       uniq_prov_pairs, dist_dat) {
  # unique pairs for distance
  uniq_dist <- left_join(uniq_prov_pairs, dist_dat, by = c(
    "ref_prov" = "co_province1",
    "other_prov" = "co_province2"
  )) %>%
    rename(dist = dist_km)

  ann_coh_dat <- left_join(uniq_prov_pairs, ann_coh_dat, by = c(
    "ref_prov" = "ref_prov",
    "other_prov" = "other_prov"
  )) %>%
    rename(coherence = coh_est)
  # joining the coherence and distance datasets

  ann_coh_dat <- left_join(ann_coh_dat, uniq_dist) %>%
    arrange(ref_prov, other_prov, coherence, dist)

  require(mgcv)
  # Getting the fitted line and 95% CIs
  fit <- gam(coherence ~ s(dist), data = ann_coh_dat)
  ann_coh_dat$fit <- as.numeric(predict(fit, ann_coh_dat))
  ann_coh_dat$fit.se <- as.numeric(predict(fit, ann_coh_dat, se.fit = T)$se.fit)
  ann_coh_dat$fit.lb <- ann_coh_dat$fit + (-1 * qnorm(0.975) * ann_coh_dat$fit.se)
  ann_coh_dat$fit.ub <- ann_coh_dat$fit + (1 * qnorm(0.975) * ann_coh_dat$fit.se)
  overall_ann_coh <- round(mean(ann_coh_dat$coherence, na.rm = T), 2)
  # Ordering datasets
  ann_coh_dat <- ann_coh_dat[order(ann_coh_dat$dist, decreasing = F), ]
  # multiannual coherence
  mlt_coh_dat <- left_join(uniq_prov_pairs, mlt_coh_dat, by = c(
    "ref_prov" = "ref_prov",
    "other_prov" = "other_prov"
  )) %>%
    rename(coherence = est)
  # joining the coherence and distance datasets
  mlt_coh_dat <- left_join(mlt_coh_dat, uniq_dist) %>%
    arrange(ref_prov, other_prov, coherence, dist)
  require(mgcv)
  # Getting the fitted line and 95% CIs
  fit <- gam(coherence ~ s(dist), data = mlt_coh_dat)
  mlt_coh_dat$fit <- as.numeric(predict(fit, mlt_coh_dat))
  mlt_coh_dat$fit.se <- as.numeric(predict(fit, mlt_coh_dat, se.fit = T)$se.fit)
  mlt_coh_dat$fit.lb <- mlt_coh_dat$fit + (-1 * qnorm(0.975) * mlt_coh_dat$fit.se)
  mlt_coh_dat$fit.ub <- mlt_coh_dat$fit + (1 * qnorm(0.975) * mlt_coh_dat$fit.se)
  overall_mlt_coh <- round(mean(mlt_coh_dat$coherence, na.rm = T), 2)
  # Ordering datasets
  mlt_coh_dat <- mlt_coh_dat[order(mlt_coh_dat$dist, decreasing = F), ]

  # Phase
  ann_phase_dat <- left_join(uniq_prov_pairs, ann_phase_dat,
    by = c("ref_prov" = "ref_prov", "other_prov" = "other_prov")
  ) %>%
    mutate(abs_lag = abs(phase_lag))

  ann_phase_dat <- left_join(ann_phase_dat, uniq_dist) %>%
    arrange(ref_prov, other_prov, abs_lag, dist)

  require(mgcv)
  # Getting the fitted line and 95% CIs
  fit <- gam(abs_lag ~ s(dist), data = ann_phase_dat)
  ann_phase_dat$fit <- as.numeric(predict(fit, ann_phase_dat))
  ann_phase_dat$fit.se <- as.numeric(predict(fit, ann_phase_dat, se.fit = T)$se.fit)
  ann_phase_dat$fit.lb <- ann_phase_dat$fit + (-1 * qnorm(0.975) * ann_phase_dat$fit.se)
  ann_phase_dat$fit.ub <- ann_phase_dat$fit + (1 * qnorm(0.975) * ann_phase_dat$fit.se)
  overall_ann_phase <- round(mean(ann_phase_dat$abs_lag, na.rm = T), 2)
  # Ordering datasets
  ann_phase_dat <- ann_phase_dat[order(ann_phase_dat$dist, decreasing = F), ]
  # multiannual phase
  # Phase
  mlt_phase_dat <- left_join(uniq_prov_pairs, mlt_phase_dat,
    by = c("ref_prov" = "ref_prov", "other_prov" = "other_prov")
  ) %>%
    mutate(abs_lag = abs(phase_lag))

  mlt_phase_dat <- left_join(mlt_phase_dat, uniq_dist) %>%
    arrange(ref_prov, other_prov, abs_lag, dist)

  require(mgcv)
  # Getting the fitted line and 95% CIs
  fit <- gam(abs_lag ~ s(dist), data = mlt_phase_dat)
  mlt_phase_dat$fit <- as.numeric(predict(fit, mlt_phase_dat))
  mlt_phase_dat$fit.se <- as.numeric(predict(fit, mlt_phase_dat, se.fit = T)$se.fit)
  mlt_phase_dat$fit.lb <- mlt_phase_dat$fit + (-1 * qnorm(0.975) * mlt_phase_dat$fit.se)
  mlt_phase_dat$fit.ub <- mlt_phase_dat$fit + (1 * qnorm(0.975) * mlt_phase_dat$fit.se)
  overall_mlt_phase <- round(mean(mlt_phase_dat$abs_lag, na.rm = T), 2)
  # Ordering datasets
  mlt_phase_dat <- mlt_phase_dat[order(mlt_phase_dat$dist, decreasing = F), ]

  par(mfrow = c(2, 1), mai = c(0.8, 1.5, 0.5, 0.5))
  # plotting coherence
  plot(ann_coh_dat$dist, ann_coh_dat$fit,
    type = "l", lwd = 3,
    col = alpha("#3B9AB2", 0.9), axes = F, ylim = c(0, 1), ylab = "", xlab = ""
  )
  polygon(c(rev(ann_coh_dat$dist), ann_coh_dat$dist), c(rev(ann_coh_dat$fit.ub), ann_coh_dat$fit.lb),
    col = alpha("#3B9AB2", 0.3), border = NA
  )
  abline(h = overall_ann_coh, lty = 2, col = alpha("#3B9AB2", 0.75), lwd = 3)
  # Mlt
  polygon(c(rev(mlt_coh_dat$dist), mlt_coh_dat$dist), c(rev(mlt_coh_dat$fit.ub), mlt_coh_dat$fit.lb),
    col = alpha("#F21A00", 0.3), border = NA
  )
  lines(mlt_coh_dat$dist, mlt_coh_dat$fit, col = alpha("#F21A00", 0.9), lwd = 3)
  abline(h = overall_mlt_coh, lty = 2, col = alpha("#F21A00", 0.9), lwd = 3)
  # adding the axes
  axis(1, cex.lab = 1.3, cex.axis = 2)
  mtext("Distance (Km)", side = 1, line = 3, cex = 2, font = 2)
  axis(2, las = 2, cex.lab = 1.3, cex.axis = 2)
  mtext("Average coherence", side = 2, line = 3.5, cex = 2, font = 2)

  # Phase plots
  plot(ann_phase_dat$dist, ann_phase_dat$fit,
    type = "l", lwd = 3,
    col = alpha("#3B9AB2", 0.9), axes = F, ylim = c(0, 12), ylab = "", xlab = ""
  )
  polygon(c(rev(ann_phase_dat$dist), ann_phase_dat$dist), c(rev(ann_phase_dat$fit.ub), ann_phase_dat$fit.lb),
    col = alpha("#3B9AB2", 0.3), border = NA
  )
  abline(h = overall_ann_phase, lty = 2, col = alpha("#3B9AB2", 0.75), lwd = 3)
  # Mlt
  polygon(c(rev(mlt_phase_dat$dist), mlt_phase_dat$dist), c(rev(mlt_phase_dat$fit.ub), mlt_phase_dat$fit.lb),
    col = alpha("#F21A00", 0.3), border = NA
  )
  lines(mlt_phase_dat$dist, mlt_phase_dat$fit, col = alpha("#F21A00", 0.9), lwd = 3)
  abline(h = overall_mlt_phase, lty = 2, col = alpha("#F21A00", 0.9), lwd = 3)
  # adding the axes
  axis(1, cex.lab = 1.3, cex.axis = 2)
  mtext("Distance (Km)", side = 1, line = 3, cex = 2, font = 2)
  axis(2, las = 2, cex.lab = 1.3, cex.axis = 2)
  mtext("Average Absolute\nPhase Difference (months)", side = 2, line = 3, cex = 2, font = 2)
}

# Figure 5b: the synchrony plot for all pairs
plot_fig5b <- function(ann_coh_dat, ann_phase_dat,
                       mlt_coh_dat, mlt_phase_dat,
                       uniq_prov_pairs, dist_dat) {
  # unique pairs for distance
  uniq_dist <- left_join(uniq_prov_pairs, dist_dat, by = c(
    "ref_prov" = "co_province1",
    "other_prov" = "co_province2"
  )) %>%
    rename(dist = dist_km)

  ann_coh_dat <- left_join(uniq_prov_pairs, ann_coh_dat, by = c(
    "ref_prov" = "ref_prov",
    "other_prov" = "other_prov"
  )) %>%
    rename(coherence = coh_est)
  ann_phase_dat <- left_join(uniq_prov_pairs, ann_phase_dat, by = c(
    "ref_prov" = "ref_prov",
    "other_prov" = "other_prov"
  )) %>%
    rename(phase_est = phase_est)

  ann_vec <- left_join(ann_coh_dat, ann_phase_dat, by = c("ref_prov", "other_prov")) %>%
    mutate(
      phase_pi = (phase_est / pi),
      phase_pi2 = (1 - abs(phase_pi)),
      est = coherence * phase_pi2
    )
  # joining the coherence and distance datasets
  ann_vec <- left_join(ann_vec, uniq_dist) %>%
    arrange(ref_prov, other_prov, est, coherence, phase_pi2, dist)

  require(mgcv)
  # Getting the fitted line and 95% CIs
  fit <- gam(est ~ s(dist), data = ann_vec)
  ann_vec$fit <- as.numeric(predict(fit, ann_vec))
  ann_vec$fit.se <- as.numeric(predict(fit, ann_vec, se.fit = T)$se.fit)
  ann_vec$fit.lb <- ann_vec$fit + (-1 * qnorm(0.975) * ann_vec$fit.se)
  ann_vec$fit.ub <- ann_vec$fit + (1 * qnorm(0.975) * ann_vec$fit.se)
  overall_ann_vec <- round(mean(ann_vec$est, na.rm = T), 2)
  # Ordering datasets
  ann_vec <- ann_vec[order(ann_vec$dist, decreasing = F), ]
  # multiannual coherence
  mlt_coh_dat <- left_join(uniq_prov_pairs, mlt_coh_dat, by = c(
    "ref_prov" = "ref_prov",
    "other_prov" = "other_prov"
  )) %>%
    rename(coherence = est)
  mlt_phase_dat <- left_join(uniq_prov_pairs, mlt_phase_dat, by = c(
    "ref_prov" = "ref_prov",
    "other_prov" = "other_prov"
  )) %>%
    rename(phase_est = est)

  # Phase
  mlt_vec <- left_join(mlt_coh_dat, mlt_phase_dat, by = c("ref_prov", "other_prov")) %>%
    mutate(
      phase_pi = (phase_est / pi),
      phase_pi2 = (1 - abs(phase_pi)),
      est = coherence * phase_pi2
    )
  # joining the coherence and distance datasets
  mlt_vec <- left_join(mlt_vec, uniq_dist) %>%
    arrange(ref_prov, other_prov, est, coherence, phase_pi2, dist)
  mlt_vec <- mlt_vec[order(mlt_vec$dist, decreasing = F), ]

  require(mgcv)
  # Getting the fitted line and 95% CIs
  fit <- gam(est ~ s(dist), data = mlt_vec)
  mlt_vec$fit <- as.numeric(predict(fit, mlt_vec))
  mlt_vec$fit.se <- as.numeric(predict(fit, mlt_vec, se.fit = T)$se.fit)
  mlt_vec$fit.lb <- mlt_vec$fit + (-1 * qnorm(0.975) * mlt_vec$fit.se)
  mlt_vec$fit.ub <- mlt_vec$fit + (1 * qnorm(0.975) * mlt_vec$fit.se)
  overall_mlt_vec <- round(mean(mlt_vec$est, na.rm = T), 2)
  # Ordering datasets
  mlt_vec <- mlt_vec[order(mlt_vec$dist, decreasing = F), ]

  par(mfrow = c(1, 1), mai = c(0.8, 1.5, 0.5, 0.5))
  # plotting average synchrony
  plot(ann_vec$dist, ann_vec$fit,
    type = "l", lwd = 3,
    col = alpha("#3B9AB2", 0.9), axes = F, ylim = c(0, 1), ylab = "", xlab = ""
  )
  polygon(c(rev(ann_vec$dist), ann_vec$dist), c(rev(ann_vec$fit.ub), ann_vec$fit.lb),
    col = alpha("#3B9AB2", 0.3), border = NA
  )
  abline(h = overall_ann_vec, lty = 2, col = alpha("#3B9AB2", 0.75), lwd = 3)
  # Mlt
  polygon(c(rev(mlt_vec$dist), mlt_vec$dist), c(rev(mlt_vec$fit.ub), mlt_vec$fit.lb),
    col = alpha("#F21A00", 0.3), border = NA
  )
  lines(mlt_vec$dist, mlt_vec$fit, col = alpha("#F21A00", 0.9), lwd = 3)
  abline(h = overall_mlt_vec, lty = 2, col = alpha("#F21A00", 0.9), lwd = 3)
  axis(1, cex.lab = 1.3, cex.axis = 2)
  mtext("Distance (Km)", side = 1, line = 3, cex = 2, font = 2)
  axis(2, cex.lab = 1.3, cex.axis = 2, las = 2)
  mtext("Average Synchrony", side = 2, line = 3.5, cex = 2, font = 2)

  legend(0, 1, c("Annual (8-16 mo.)", "Multiannual (>16mo.)", "Regional average"),
    lty = c(1, 1, 2), lwd = c(3, 3, 3),
    x.intersp = 1, y.intersp = 1,
    seg.len = 2.5, col = c("#3B9AB2", "#F21A00", "grey30"),
    bty = "n", cex = 1.25, pt.lwd = 2
  )
}

#### Figure 6: Average synchrony by distance
plot_fig6 <- function(ann_data, mlt_data, prov_coords, prov_dist) {
  ann_data <- ann_data %>%
    filter(!is.na(est)) %>%
    left_join(prov_dist, by = c("ref_prov" = "co_province1", "other_prov" = "co_province2"))

  avg_ann_data <- ann_data %>%
    mutate(
      abs_est = abs(est),
      pairs_distance = ifelse(dist_km < 2000, 0,
        ifelse(dist_km >= 2000 & dist_km <= 4000, 1, 2)
      )
    ) %>%
    group_by(ref_prov, pairs_distance) %>%
    summarize(avg_synchrony = mean(abs_est, na.rm = T)) %>%
    left_join(prov_coords, by = c("ref_prov" = "co_province"))

  mlt_data <- mlt_data %>%
    filter(!is.na(est)) %>%
    left_join(prov_dist, by = c("ref_prov" = "co_province1", "other_prov" = "co_province2"))

  avg_mlt_data <- mlt_data %>%
    mutate(
      abs_est = abs(est),
      pairs_distance = ifelse(dist_km < 2000, 0,
        ifelse(dist_km >= 2000 & dist_km <= 4000, 1, 2)
      )
    ) %>%
    group_by(ref_prov, pairs_distance) %>%
    summarize(avg_synchrony = mean(abs_est, na.rm = T)) %>%
    left_join(prov_coords, by = c("ref_prov" = "co_province"))

  library(sf)
  am_country_map <- st_transform(
    st_read(
      dsn = "data",
      layer = "am_low_res_country",
      stringsAsFactors = FALSE
    ),
    crs = "+proj=longlat +datum=WGS84"
  )

  am_adm_map <- st_transform(
    st_read(
      dsn = "data",
      layer = "am_low_res_province_map",
      stringsAsFactors = FALSE
    ),
    crs = "+proj=longlat +datum=WGS84"
  )
  am_adm_map$co_prov[am_adm_map$co_prov == "br_rond^onia"] <- "br_rondonia"
  am_adm_map$co_prov[am_adm_map$co_prov == "br_s~ao_paulo"] <- "br_sao_paulo"
  am_adm_map$co_prov[am_adm_map$co_prov == "br_maranh~ao"] <- "br_maranhao"
  am_adm_map$co_prov[am_adm_map$co_prov == "gt_peten"] <- "gt_el_peten"
  am_adm_map$co_prov[am_adm_map$co_prov == "gt_quezaltenango"] <- "gt_quetzaltenango"
  am_adm_map$co_prov[am_adm_map$co_prov == "sv_caba~nas"] <- "sv_cabanas"
  am_adm_map$co_prov[am_adm_map$co_prov == "pa_ng\"obe_bugle"] <- "pa_ngobe_bugle"
  am_adm_map$co_prov[am_adm_map$co_prov == "co_nari~no"] <- "co_narino"
  am_adm_map$co_prov[am_adm_map$co_prov == "ec_ca~nar"] <- "ec_canar"

  ann_sync_lt2000_map <- left_join(am_adm_map,
    avg_ann_data %>% filter(pairs_distance == 0),
    by = c("co_prov" = "ref_prov")
  )
  ann_sync_2000_map <- left_join(am_adm_map,
    avg_ann_data %>% filter(pairs_distance == 1),
    by = c("co_prov" = "ref_prov")
  )
  ann_sync_gt4000_map <- left_join(am_adm_map, avg_ann_data %>% filter(pairs_distance == 2),
    by = c("co_prov" = "ref_prov")
  )
  mlt_sync_lt2000_map <- left_join(am_adm_map,
    avg_mlt_data %>% filter(pairs_distance == 0),
    by = c("co_prov" = "ref_prov")
  )
  mlt_sync_2000_map <- left_join(am_adm_map,
    avg_mlt_data %>% filter(pairs_distance == 1),
    by = c("co_prov" = "ref_prov")
  )
  mlt_sync_gt4000_map <- left_join(am_adm_map,
    avg_mlt_data %>% filter(pairs_distance == 2),
    by = c("co_prov" = "ref_prov")
  )
  # <2000
  ann_sync_lt2000_points <- sf::st_as_sf(
    avg_ann_data %>% filter(pairs_distance == 0 &
      (ref_prov == "pr_all" | ref_prov == "dr_all" | ref_prov == "bb_all")),
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  # 2000-4000
  ann_sync_2000_points <- sf::st_as_sf(
    avg_ann_data %>% filter(pairs_distance == 1 &
      (ref_prov == "pr_all" | ref_prov == "dr_all" | ref_prov == "bb_all")),
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  # >4000 km
  ann_sync_gt4000_points <- sf::st_as_sf(
    avg_ann_data %>% filter(pairs_distance == 2 &
      (ref_prov == "pr_all" | ref_prov == "dr_all" | ref_prov == "bb_all")),
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  # <2000
  mlt_sync_lt2000_points <- sf::st_as_sf(
    avg_ann_data %>% filter(pairs_distance == 0 &
      (ref_prov == "pr_all" | ref_prov == "dr_all" | ref_prov == "bb_all")),
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  # 2000-4000
  mlt_sync_2000_points <- sf::st_as_sf(
    avg_ann_data %>% filter(pairs_distance == 1 &
      (ref_prov == "pr_all" | ref_prov == "dr_all" | ref_prov == "bb_all")),
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  # >4000
  mlt_sync_gt4000_points <- sf::st_as_sf(
    avg_ann_data %>% filter(pairs_distance == 2 &
      (ref_prov == "pr_all" | ref_prov == "dr_all" | ref_prov == "bb_all")),
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  # PLOTTING
  p1 <- ggplot() +
    # geom_sf(data = am_country_map,
    #         color = "black", fill = "grey90") +
    geom_sf(data = ann_sync_lt2000_map, color = "white", aes(fill = avg_synchrony, geometry = geometry)) +
    geom_sf(
      data = am_country_map,
      color = "grey50", fill = "transparent", linewidth = 0.6
    ) +
    geom_point(
      data = st_centroid(ann_sync_lt2000_points),
      aes(color = avg_synchrony, geometry = geometry), size = 3,
      stat = "sf_coordinates"
    ) +
    scale_color_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = " ", guide = "none"
    ) +
    scale_fill_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = "Average Synchrony"
    ) +
    scale_size(guide = "none") +
    theme_minimal() +
    labs(y = "Annual") +
    theme(
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 26),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.75, 0.1, 0.5, 0.5), "cm"), # t, r, b, l
      legend.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1, "cm"),
      legend.box.margin = margin(-10, -10, -10, -10),
      plot.tag = element_text(face = "bold", colour = "black", size = 30)
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

  p2 <- ggplot() +
    geom_sf(
      data = am_country_map,
      color = "black", fill = "grey90"
    ) +
    geom_sf(data = ann_sync_2000_map, color = "white", aes(fill = avg_synchrony, geometry = geometry)) +
    geom_sf(
      data = am_country_map,
      color = "grey50", fill = "transparent", linewidth = 0.6
    ) +
    geom_point(
      data = st_centroid(ann_sync_2000_points),
      aes(color = avg_synchrony, geometry = geometry), size = 3,
      stat = "sf_coordinates"
    ) +
    scale_color_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = " ", guide = "none"
    ) +
    scale_fill_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = "Average Synchrony"
    ) +
    scale_size(guide = "none") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 26),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.75, 0.1, 0.5, 0.5), "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1, "cm"),
      legend.box.margin = margin(-10, -10, -10, -10),
      plot.tag = element_text(face = "bold", colour = "black", size = 30)
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

  p3 <- ggplot() +
    geom_sf(
      data = am_country_map,
      color = "black", fill = "grey90"
    ) +
    geom_sf(data = ann_sync_gt4000_map, color = "white", aes(fill = avg_synchrony, geometry = geometry)) +
    geom_sf(
      data = am_country_map,
      color = "grey50", fill = "transparent", linewidth = 0.6
    ) +
    geom_point(
      data = st_centroid(ann_sync_gt4000_points),
      aes(color = avg_synchrony, geometry = geometry), size = 3,
      stat = "sf_coordinates"
    ) +
    scale_color_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = " ", guide = "none"
    ) +
    scale_fill_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = "Average Synchrony"
    ) +
    scale_size(guide = "none") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 26),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.75, 0.1, 0.5, 0.5), "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1, "cm"),
      legend.box.margin = margin(-10, -10, -10, -10),
      plot.tag = element_text(face = "bold", colour = "black", size = 30)
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

  p4 <- ggplot() +
    geom_sf(
      data = am_country_map,
      color = "black", fill = "grey90"
    ) +
    geom_sf(data = mlt_sync_lt2000_map, color = "white", aes(fill = avg_synchrony, geometry = geometry)) +
    geom_sf(
      data = am_country_map,
      color = "grey50", fill = "transparent", linewidth = 0.6
    ) +
    geom_point(
      data = st_centroid(mlt_sync_lt2000_points),
      aes(color = avg_synchrony, geometry = geometry), size = 3,
      stat = "sf_coordinates"
    ) +
    scale_color_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = " ", guide = "none"
    ) +
    labs(y = "Multiannual") +
    scale_fill_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = "Average Synchrony"
    ) +
    scale_size(guide = "none") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 24),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.5, 0.1, 0.5, 0.5), "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1, "cm"),
      legend.box.margin = margin(-10, -10, -10, -10),
      plot.tag = element_text(face = "bold", colour = "black", size = 28)
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

  p5 <- ggplot() +
    geom_sf(
      data = am_country_map,
      color = "black", fill = "grey90"
    ) +
    geom_sf(data = mlt_sync_2000_map, color = "white", aes(fill = avg_synchrony, geometry = geometry)) +
    geom_sf(
      data = am_country_map,
      color = "grey50", fill = "transparent", linewidth = 0.6
    ) +
    geom_point(
      data = st_centroid(mlt_sync_2000_points),
      aes(color = avg_synchrony, geometry = geometry), size = 3,
      stat = "sf_coordinates"
    ) +
    scale_color_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = " ", guide = "none"
    ) +
    scale_fill_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = "Average Synchrony"
    ) +
    scale_size(guide = "none") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 24),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.5, 0.1, 0.5, 0.5), "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1, "cm"),
      legend.box.margin = margin(-10, -10, -10, -10),
      plot.tag = element_text(face = "bold", colour = "black", size = 28)
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

  p6 <- ggplot() +
    geom_sf(
      data = am_country_map,
      color = "black", fill = "grey90"
    ) +
    geom_sf(data = mlt_sync_gt4000_map, color = "white", aes(fill = avg_synchrony, geometry = geometry)) +
    geom_sf(
      data = am_country_map,
      color = "grey50", fill = "transparent", linewidth = 0.6
    ) +
    geom_point(
      data = st_centroid(mlt_sync_gt4000_points),
      aes(color = avg_synchrony, geometry = geometry), size = 3,
      stat = "sf_coordinates"
    ) +
    scale_color_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = " ", guide = "none"
    ) +
    scale_fill_distiller(
      palette = "YlGn", direction = 1,
      limits = c(0, 0.7), name = "Average Synchrony"
    ) +
    scale_size(guide = "none") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      title = element_text(face = "bold", size = 24),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.5, 0.1, 0.5, 0.5), "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1, "cm"),
      legend.box.margin = margin(-10, -10, -10, -10),
      plot.tag = element_text(face = "bold", colour = "black", size = 28)
    ) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

  gridExtra::grid.arrange(p1, p2, p3,
    p4, p5, p6,
    ncol = 3, nrow = 2
  )
}

#### Figure 7: Vector map
plot_fig7 <- function(ann_alpha_data,
                      mlt_alpha_data,
                      pop_data,
                      ymax = 31, ymin = -22,
                      xmax = -36, xmin = -115,
                      ann_arrow_scale,
                      mlt_arrow_scale) {
  # SEASONAL
  ann_deltas <- ann_alpha_data %>%
    group_by(ref_prov) %>%
    summarise(
      delta_X = mean(delta_x, na.rm = T),
      delta_Y = mean(delta_y, na.rm = T),
      mean_coh = mean(coh_phase, na.rm = T)
    )
  ann_deltas <- data.frame(ann_deltas)
  # Adding population/lat
  ann_deltas <- left_join(ann_deltas, pop_data, by = c("ref_prov" = "co_province")) %>%
    filter(!is.na(lat) | !is.nan(delta_X)) %>%
    data.frame()
  ann_deltas <- ann_deltas[!is.nan(ann_deltas$delta_X) == TRUE, ]

  # MULTIYEAR
  mlt_deltas <- mlt_alpha_data %>%
    group_by(ref_prov) %>%
    summarise(
      delta_X = mean(delta_x, na.rm = T),
      delta_Y = mean(delta_y, na.rm = T),
      mean_coh = mean(coh_phase, na.rm = T)
    )
  mlt_deltas <- data.frame(mlt_deltas)

  mlt_deltas <- left_join(mlt_deltas, pop_data, by = c("ref_prov" = "co_province")) %>%
    filter(!is.na(lat) | !is.nan(delta_X)) %>%
    data.frame()
  mlt_deltas <- mlt_deltas[!is.nan(mlt_deltas$delta_X) == TRUE, ]

  library(sf)
  am_country_map <- st_read(dsn = "data", layer = "am_low_res_country", stringsAsFactors = FALSE)
  am_adm_map <- st_read(dsn = "data", layer = "am_low_res_province_map", stringsAsFactors = FALSE)
  am_adm_map$co_prov[am_adm_map$co_prov == "br_rond^onia"] <- "br_rondonia"
  am_adm_map$co_prov[am_adm_map$co_prov == "br_s~ao_paulo"] <- "br_sao_paulo"
  am_adm_map$co_prov[am_adm_map$co_prov == "br_maranh~ao"] <- "br_maranhao"
  am_adm_map$co_prov[am_adm_map$co_prov == "gt_peten"] <- "gt_el_peten"
  am_adm_map$co_prov[am_adm_map$co_prov == "gt_quezaltenango"] <- "gt_quetzaltenango"
  am_adm_map$co_prov[am_adm_map$co_prov == "sv_caba~nas"] <- "sv_cabanas"
  am_adm_map$co_prov[am_adm_map$co_prov == "pa_ng\"obe_bugle"] <- "pa_ngobe_bugle"
  am_adm_map$co_prov[am_adm_map$co_prov == "co_nari~no"] <- "co_narino"
  am_adm_map$co_prov[am_adm_map$co_prov == "ec_ca~nar"] <- "ec_canar"

  ann_points <- sf::st_as_sf(ann_deltas,
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )
  mlt_points <- sf::st_as_sf(mlt_deltas,
    coords = c("long", "lat"),
    crs = "+proj=longlat +datum=WGS84"
  )

  par(mfrow = c(1, 2), oma = c(0, 0.25, 0, 0.25), mar = c(0.5, 1.5, 0.5, 2))
  # Seasonal
  plot(st_geometry(am_country_map),
    bg = "transparent", col = "grey90", border = "black",
    lwd = 1.5, cex = 5, ylim = c(-50, 30), xlim = c(-115, -36)
  )
  plot(st_geometry(am_adm_map),
    bg = "transparent", lwd = 1,
    col = "white", border = "grey80",
    cex = 5, ylim = c(-50, 30), xlim = c(-115, -36), add = T
  )
  plot(st_geometry(am_country_map),
    bg = "transparent", col = "transparent", border = "grey50",
    lwd = 1.5, cex = 5, ylim = c(-50, 30), xlim = c(-115, -36), add = T
  )
  # points
  plot(st_geometry(ann_points), add = T, pch = 19, cex = 0.75, col = "grey50")
  require(IDPmisc)
  # Plotting the positive phase differences
  for (i in 1:nrow(ann_deltas)) {
    arrows(
      x0 = ann_deltas$long[i], x1 = ann_deltas$long[i] + ann_deltas$delta_X[i] * ann_arrow_scale,
      y0 = ann_deltas$lat[i], y1 = ann_deltas$lat[i] + ann_deltas$delta_Y[i] * ann_arrow_scale,
      code = 2,
      lwd = 3.25,
      length = 0.1,
      col = alpha("#3B9AB2", 0.5)
    )
  }

  par(font = 5) # change font to get arrows
  legend("bottomright",
    legend = c(NA, NA, NA), pch = c(NA, NA, 174),
    lwd = 3, col = c("#3B9AB2", "#F21A00", "black"),
    lty = c(1, 1, NA), bty = "n", cex = 2.5, y.intersp = 0.5
  )
  par(font = 1)

  # Multiannual
  plot(st_geometry(am_country_map),
    bg = "transparent", col = "grey90", border = "black",
    lwd = 1.5, cex = 5, ylim = c(-50, 30), xlim = c(-115, -30)
  )
  plot(st_geometry(am_adm_map),
    bg = "transparent", lwd = 1,
    col = "white", border = "grey80",
    cex = 5, ylim = c(-50, 30), xlim = c(-115, -36), add = T
  )
  plot(st_geometry(am_country_map),
    bg = "transparent", col = "transparent", border = "grey50",
    lwd = 1.5, cex = 5, ylim = c(-50, 30), xlim = c(-115, -30), add = T
  )
  # points
  plot(st_geometry(mlt_points), add = T, pch = 19, cex = 1, col = "grey50")
  # Adding the arrows
  require(IDPmisc)
  # Plotting the positive phase differences
  for (i in 1:nrow(mlt_deltas)) {
    arrows(
      x0 = mlt_deltas$long[i], x1 = mlt_deltas$long[i] + mlt_deltas$delta_X[i] * mlt_arrow_scale,
      y0 = mlt_deltas$lat[i], y1 = mlt_deltas$lat[i] + mlt_deltas$delta_Y[i] * mlt_arrow_scale,
      code = 2,
      lwd = 3.25,
      length = 0.1,
      col = alpha("#F21A00", 0.5)
    ) # changing the color based on correlation
  }
}
