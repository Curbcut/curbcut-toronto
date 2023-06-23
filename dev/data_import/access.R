## BUILD AND APPEND ACCESS DATA ################################################

build_and_append_access <- function(scales_variables_modules, traveltimes, DA_table, crs) {

  # Read and prepare data ---------------------------------------------------
  
  # gj <- list.files("dev/data/green_space", full.names = TRUE)
  # gj <- gj[grepl("geojson$", gj)]
  # data <- lapply(gj, geojsonsf::geojson_sf)
  # 
  # data <- lapply(data, \(x) x["geometry"])
  # data <- Reduce(rbind, data)
  # data$ID <- seq_along(data$geometry)
  # data <- sf::st_cast(data, "MULTIPOLYGON")
  # 
  # keep_index <- sapply(seq_along(data$geometry), \(x) sf::st_is(data[x,], "POLYGON") | 
  #          sf::st_is(data[x,], "MULTIPOLYGON"))
  # data <- data[keep_index, ]
  # data <- sf::st_cast(data, "MULTIPOLYGON")
  # 
  # data <- sf::st_read("dev/data/green_space/green_space_GTA_da.shp")
  # data <- data[c("DAUID", "GREENAR")]
  # data <- sf::st_drop_geometry(data)
  # names(data) <- c("DA_ID", "green_space_sqkm_2023")
  # data <- tibble::as_tibble(data)
  # 
  # # # Add point data to the access module. First step using a point data df
  # # point_DA <- accessibility_point_per_DA(point_data = list(daycarespots_2023 = daycares),
  # #                                        DA_table = census_scales$DA,
  # #                                        crs = crs)
  # 
  # access_time <- accessibility_add_intervals(point_per_DA = data,
  #                                            traveltimes = traveltimes,
  #                                            region_DA_IDs = data$DA_ID)
  # 
  # qs::qsave(access_time, "dev/data/built/access_time.qs")
  access_time <- qs::qread("dev/data/built/access_time.qs")
  

  # Get list of data variables ----------------------------------------------

  # Build a character vector of all data variables that will be added to all
  # scales. Average and additive vars are for interpolation. A count variable
  # like number of households is additive. The percentage of tenants is average.
  average_vars <- names(access_time)[!grepl("ID$", names(access_time))]
  additive_vars <- c()
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.

  data_interpolated <-
    interpolate_from_census_geo(
      data = access_time,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )


  # Make a types named list -------------------------------------------------

  unique_vars <- gsub("_\\d{4}$", "", vars)
  
  # Calculate breaks ONCE for 30 minutes. Use those breaks on all variables
  breaks_base <- sapply(unique_vars, paste, simplify = FALSE, USE.NAMES = TRUE)
  breaks_base <- lapply(breaks_base, \(x) gsub("_\\d{2}_", "_30_", x))
  
  types <- rep(list("avg"), length(unique_vars))
  names(types) <- unique_vars
  
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = average_vars,
      types = types, 
      use_quintiles = TRUE
    )

  # Get the variables values per regions ------------------------------------

  # Parent strings
  vars <- gsub("_2023$", "", average_vars)
  parent_strings <- rep(list("population"), length(vars))
  names(parent_strings) <- vars
  
  types <- rep(list("avg"), length(vars))
  names(types) <- vars

  region_vals <- variables_get_region_vals(
    scales = data_interpolated$scales,
    vars = vars,
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table,
    round_closest_5 = FALSE)
  
  
  # Variable measurements ----------------------------------------------------
  
  var_measurement <- data.frame(
    df = data_interpolated$avail_df,
    measurement = rep("scalar", length(data_interpolated$avail_df)))
  
  var_measurement$measurement[grepl("_DA$", var_measurement$df)] <-
    rep("ordinal", length(var_measurement$measurement[grepl("_DA$", var_measurement$df)]))


  # Variables table ---------------------------------------------------------

  new_variables <- lapply(vars, \(var) {
    
    theme <- "square kilometers of green space"
    
    mode <- (\(x) {
      if (grepl("_car_", var)) return("car")
      if (grepl("_foot_", var)) return("walking")
      if (grepl("_bicycle_", var)) return("bicycle")
      if (grepl("_transit_opwe_", var)) return("public transit on off-peak weekend days")
      if (grepl("_transit_pwe_", var)) return("public transit on peak weekend days")
      if (grepl("_transit_nwd_", var)) return("public transit on weekdays at night")
      if (grepl("_transit_nwe_", var)) return("public transit on weekends at night")
      if (grepl("_transit_opwd_", var)) return("public transit on off-peak weekdays")
      if (grepl("_transit_pwd_", var)) return("public transit on peak weekdays")
    })()
    
    time <- gsub("_", "", stringr::str_extract(var, "_\\d*_"))
    
    var_title <- stringr::str_to_sentence(paste0(theme, " accessible by ", mode))
    var_short <- "Green space"
    
    
    explanation <- paste0(
      "the number of ", tolower(theme),
      " an average resident can reach within ", time, " minutes by ", mode
    )
    exp_q5 <- paste0(
      "the average resident has access to _X_ ", tolower(theme), " within ", time,
      " minutes by ", mode
    )
    
    # Cut timing out of the mode
    mode <- stringr::str_extract(mode, "(^car$)|(^walking$)|(^bicycle$)|(^public transit)")
    
    group_name <- paste("Access to", theme)
    group_diff <- list("Mode of transport" = stringr::str_to_sentence(mode),
                       "Transportation time" = time)
    
    if (grepl("_transit_", var)) {
      timing <- (\(x) {
        if (grepl("_transit_opwe_", var)) return("Weekend traffic off-peak")
        if (grepl("_transit_pwe_", var)) return("Weekend traffic peak")
        if (grepl("_transit_nwd_", var)) return("Weekday night")
        if (grepl("_transit_nwe_", var)) return("Weekend night")
        if (grepl("_transit_opwd_", var)) return("Weekday traffic off-peak")
        if (grepl("_transit_pwd_", var)) return("Weekday traffic peak")
      })()
      group_diff <- c(group_diff, list("Timing" = timing))
    }
    
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = "avg",
      var_title = var_title,
      var_short = var_short,
      explanation = explanation,
      exp_q5 = exp_q5,
      group_name = group_name,
      group_diff = group_diff,
      parent_vec = "population",
      theme = "Transport",
      private = FALSE,
      pe_include = var %in% c("access_foot_20_green_space_sqkm_2023",
                              "access_bike_20_green_space_sqkm_2023"),
      region_values = region_vals[[var]],
      dates = with_breaks$avail_dates[[var]],
      avail_df = data_interpolated$avail_df,
      breaks_q3 = with_breaks$q3_breaks_table[[var]],
      breaks_q5 = with_breaks$q5_breaks_table[[var]],
      source = "OSM",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense"),
      var_measurement = var_measurement
    ) |>
      (\(x) x[nrow(x), ])()
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))
  

  # Modules table -----------------------------------------------------------

  scales_variables_modules$modules$var_left[
    scales_variables_modules$modules$id == "access"
  ] <- list(variables[grepl("^access_", variables$var_code),
                      c("var_code", "group_name", "group_diff")])
  
  modules <- scales_variables_modules$modules


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules
  ))

}

