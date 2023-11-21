## BUILD AND APPEND TREE_SQKM DATA #############################################

build_and_append_tree_sqkm <- function(scales_variables_modules, DA_table, crs) {
  
  # Read and prepare data ---------------------------------------------------
  data <- sf::read_sf("dev/data/tree/toronto_tree_by_da.shp")
  data <- sf::st_drop_geometry(data)
  data <- data[, c("DAUID", "count")]
  names(data) <- c("ID", "tree_count")
  
  # Add tree per population
  data <- merge(data, DA_table[c("ID", "population")])
  names(data)[1] <- "DA_ID"
  
  data$area <- get_area(data)
  data$tree_sqkm <- data$tree_count / data$area * 1000000

  data <- data[c("DA_ID", "tree_sqkm")]
  names(data)[2] <- paste0(names(data)[2], "_2019")

  
  # Get list of data variables ----------------------------------------------

  average_vars <- c("tree_sqkm_2019")
  additive_vars <- c()
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  data_interpolated <-
    interpolate_custom_geo(
    data = data,
    all_scales = scales_variables_modules$scales,
    crs = crs,
    average_vars = average_vars,
    additive_vars = c(),
    name_interpolate_from = "DA"
    )
  
  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$scale == "DA"
  ] <- "tree point" 
  
  
  # Make a types named list -------------------------------------------------
  
  types <- list(tree_sqkm = "sqkm")
  
  
  # Get the variables values per regions ------------------------------------
  
  # Make a parent string the same way as the types
  parent_strings <- list(tree_sqkm = NA)
  
  # Data tibble -------------------------------------------------------------
  
  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = data_interpolated$scales,
                         unique_var = "tree_sqkm",
                         time_regex = "_\\d{4}$")

  
  # Variables table ---------------------------------------------------------

  # For more information on how to append the information, read the
  # documentation of `add_variable`. Every variable needs to have its own entry
  # in the variables table. The following is an example.
  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "tree_sqkm",
      type = "sqkm",
      var_title = "Trees per square kilometre",
      var_short = "Trees km2",
      explanation = "the density of trees measured by square kilometres",
      exp_q5 = "the density of trees is _X_ per square kilometres",
      theme = "Ecology",
      private = FALSE,
      pe_include = TRUE,
      parent_vec = NA,
      dates = 2019,
      avail_scale = data_interpolated$avail_scale, 
      source = "City of Toronto's open data portal",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense")
    )
  
  # Possible sequences ------------------------------------------------------
  
  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)


  # Modules table -----------------------------------------------------------
  
  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "tree",
      theme = "Ecology",
      nav_title = "Tree coverage",
      title_text_title = "Toronto tree coverage",
      title_text_main = paste0(
        "<p><a href='https://www.greenbelt.ca/cooling_corridors'>Research</a> ",
        "has shown that city's tree canopy contributes to",
        " environmental benefits as well as to our physical and emotional well-being.",
        " However, it is also recognized that trees are <a href='https://tellingstorieswithdata.com/inputs/pdfs/paper_one-2022-ethan_sansom.pdf'>",
        "unevenly distributed</a> relative to social",
        " and economic factors.",
        " This module provides an estimation of",
        " tree counts, density and distribution in Toronto."
      ),
      title_text_extra = paste0(
        "<p>Data was obtained through <a href='https://open.toron",
        "to.ca/dataset/topographic-mapping-physical-location-of-trees/'>Toronto</a>",
        " Open Data Porta. Data comes in as a point layer",
        " representing the physical location of trees derived",
        " from high resolution aerial photography last refreshed in Oct 2",
        "019. These point data were then aggregated by census geographic bounda",
        "ries of dissemination areas in Toronto in order to calculate total cou",
        "nts and density by area and population."
      ),
      metadata = TRUE,
      dataset_info = paste0(""), 
      var_left = c("tree_count", "tree_per1k", "tree_sqkm", "tree_ppo"), 
      dates = "2019", 
      main_dropdown_title = "Tree coverage indicator", 
      var_right = variables$var_code[variables$source == "Canadian census" &
                                       !is.na(variables$parent_vec)],
      default_var = "tree_count",
      avail_scale_combinations = avail_scale_combinations
    )
  

  
  # Return ------------------------------------------------------------------

  return(list(
    scales = data_interpolated$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules,
    data = data
  ))

}
