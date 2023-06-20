## BUILD AND APPEND TREE DATA ##################################################

build_and_append_tree_count <- function(scales_variables_modules, DA_table, crs) {

  # Read and prepare data ---------------------------------------------------

  # Read the data placed in a folder in `dev/data/`
  data <- sf::read_sf("dev/data/tree/toronto_tree_by_da.shp")
  data <- sf::st_drop_geometry(data)
  data <- data[, c("DAUID", "count")]
  names(data) <- c("ID", "tree_count")
  
  # Add tree per population
  data <- merge(data, sf::st_drop_geometry(DA_table[c("ID", "population")]))
  data$tree_per1k <- data$tree_count / data$population * 1000
  data$tree_per1k[data$tree_per1k == Inf] <- NA
  data$tree_ppo <- data$population / data$tree_count
  data$tree_ppo[is.infinite(data$tree_ppo)] <- NA

  names(data)[1] <- "DA_ID"
  
  data <- data[c("DA_ID", "tree_count", "tree_per1k", "tree_ppo")]
  names(data)[2:4] <- paste0(names(data)[2:4], "_2019")
  
  # Get list of data variables ----------------------------------------------

  average_vars <- c("tree_per1k_2019", "tree_ppo_2019")
  additive_vars <- c("tree_count_2019")
  vars <- c(average_vars, additive_vars)

  
  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      only_regions = "city",
      weight_by = "population",
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )

  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$df == "city_DA"
  ] <- "tree point" 
  
  
  # Make a types named list -------------------------------------------------
  
  types <- list(tree_per1k = "per1k",
                tree_count = "count",
                tree_ppo = "ppo")
  
  
  # Calculate breaks --------------------------------------------------------

  # Calculate breaks using the `calculate_breaks` function.
  with_breaks <-
    calculate_breaks(
      all_scales = data_interpolated$scales,
      vars = vars,
      types = types
    )
  
  
  # Get the variables values per regions ------------------------------------
  
  # Make a parent string the same way as the types
  parent_strings <- list(tree_per1k = "population",
                         tree_count = "count",
                         tree_ppo = "population")
  
  region_vals <- variables_get_region_vals(
    scales = data_interpolated$scales,
    vars = c("tree_count", "tree_per1k", "tree_ppo"),
    types = types,
    parent_strings = parent_strings,
    breaks = with_breaks$q5_breaks_table)
  

  # Variables table ---------------------------------------------------------

  # For more information on how to append the information, read the
  # documentation of `add_variable`. Every variable needs to have its own entry
  # in the variables table. The following is an example.
  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "tree_count",
      type = "count",
      var_title = "Tree count",
      var_short = "Trees",
      explanation = "the count of trees", 
      exp_q5 = "populate the urban landscape", 
      parent_vec = "trees", 
      theme = "Environment", 
      avail_df = data_interpolated$avail_df, 
      pe_include = TRUE,
      region_values = region_vals$tree_count,
      private = FALSE,
      dates = with_breaks$avail_dates[["tree_count"]], 
      breaks_q3 = with_breaks$q3_breaks_table[["tree_count"]],
      breaks_q5 = with_breaks$q5_breaks_table[["tree_count"]],
      source = "City of Toronto's open data portal",
      interpolated = data_interpolated$interpolated_ref
    )
  
  variables <-
    add_variable(
      variables = variables,
      var_code = "tree_per1k",
      type = "per1k",
      var_title = "Trees per 1,000 residents",
      var_short = "Trees 1k",
      explanation = "the density of trees measured by 1000 residents",
      exp_q5 = "the density of trees is _X_ per 1,000 residents",
      theme = "Environment",
      parent_vec = NA, 
      avail_df = data_interpolated$avail_df, 
      pe_include = TRUE,
      region_values = region_vals$tree_per1k,
      private = FALSE,
      dates = with_breaks$avail_dates[["tree_per1k"]],
      breaks_q3 = with_breaks$q3_breaks_table[["tree_per1k"]],
      breaks_q5 = with_breaks$q5_breaks_table[["tree_per1k"]],
      source = "City of Toronto's open data portal",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense")
    )
  
  variables <-
    add_variable(
      variables = variables,
      var_code = "tree_ppo",
      type = "ppo",
      var_title = "People per tree",
      var_short = "PPT",
      explanation = "the number of people per tree",
      exp_q5 = "tree",
      theme = "Environment",
      parent_vec = NA, 
      avail_df = data_interpolated$avail_df, 
      pe_include = TRUE,
      region_values = region_vals$tree_ppo,
      private = FALSE,
      dates = with_breaks$avail_dates[["tree_ppo"]],
      breaks_q3 = with_breaks$q3_breaks_table[["tree_ppo"]],
      breaks_q5 = with_breaks$q5_breaks_table[["tree_ppo"]],
      source = "City of Toronto's open data portal",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense")
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = scales_variables_modules$modules
  ))

}

