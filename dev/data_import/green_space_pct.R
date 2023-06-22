## BUILD AND APPEND GREEN_SPACE_PCT DATA #######################################

build_and_append_green_space_pct <- function(scales_variables_modules, DA_table, crs) {

  # Read and prepare data ---------------------------------------------------

  # Read the data placed in a folder in `dev/data/`
  data <- sf::read_sf("dev/data/green_space_pct/green_space_GTA_da.shp")
  data <- sf::st_drop_geometry(data) #should not drop geometry
  data <- data[, c("DAUID", "GreenDn")]
  names(data) <- c("ID", "green_space_pct")
  data$green_space_pct <- data$green_space_pct*100
  
  data <- merge(data, DA_table[c("ID")])
  names(data)[1] <- "DA_ID"
  
  #data <- data[c("DA_ID", "green_space_pct")]
  names(data)[2] <- paste0(names(data)[2], "_2023")


  # Get list of data variables ----------------------------------------------

  # Build a character vector of all data variables that will be added to all
  # scales. Average and additive vars are for interpolation. A count variable
  # like number of households is additive. The percentage of tenants is average.
  average_vars <- c("green_space_pct_2023") # names(data)[!grepl("ID$", names(data))]
  additive_vars <- c()
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  data_interpolated <-
    interpolate_custom_geo(
      data = data,
      name_interpolate_from = "DA",
      #base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      #weight_by = "households",
      #only_regions = "city",
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs
    )

  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$df == "CMA_DA"
  ] <- "green space polygons" #check text: telling user how data is interpolated
  
  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$df == "city_DA"
  ] <- "green space polygons"
  
  # Make a types named list -------------------------------------------------

  # This will be used to inform which methods to use to calculate breaks and
  # the region values. Percentages, dollars, index, ... get treated differently.
  # See the `add_variable`'s documentation to see possible types.
  types <- list(green_space_pct = "sqkm")


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
  parent_strings <- list(green_space_pct = NA)

  region_vals <- variables_get_region_vals( #might got NA here!!!
    scales = data_interpolated$scales,
    vars = c("green_space_pct"),
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
      var_code = "green_space_pct",
      type = "sqkm",
      var_title = "Green space (%)",
      var_short = "Green space (%)",
      explanation = "the percentage of green space",
      exp_q5 = "_X_% of the area is covered by green space",
      theme = "Environment",
      private = FALSE,
      pe_include = TRUE,
      parent_vec = NA,
      region_values = region_vals$green_space_pct,
      dates = with_breaks$avail_dates[["green_space_pct"]],
      avail_df = data_interpolated$avail_df, 
      breaks_q3 = with_breaks$q3_breaks_table[["green_space_pct"]],
      breaks_q5 = with_breaks$q5_breaks_table[["green_space_pct"]],
      source = "OpenStreetMap",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c("exceptionally sparse", "unusually sparse",
                       "just about average", "unusually dense",
                       "exceptionally dense")
    )


  # Modules table -----------------------------------------------------------

  # Facultative. If a page is to be added accompanying this data, add modules
  # description. Read the documentation of `add_module`. If no module is to be
  # created, assign `scales_variables_modules$modules` to modules.
  modules <- scales_variables_modules$modules

  # modules <-
  #   scales_variables_modules$modules |>
  #   add_module(
  #     id = "greenspace",
  #     theme = "Environment",
  #     nav_title = "Green Space",
  #     title_text_title = "Toronto Green Space",
  #     title_text_main = paste0(
  #       "<p>Need work: Access to green space has critical impact on urban en",
  #       "vironmental and health benefits. This module provides an estimation of",
  #       " density or percentage of green spaces in Toronto."
  #     ),
  #     title_text_extra = paste0(
  #       "<p>Need work: Data was obtained through OpenStreetMap."
  #     ),
  #     regions = unique(data_interpolated$regions),
  #     metadata = TRUE,
  #     dataset_info = paste0(),
  #     var_left = "green_space_pct",
  #     dates = "2023",
  #     main_dropdown_title = "Data grouping", #could change to specific names
  #     var_right = variables$var_code[variables$source == "Canadian census" &
  #                                      !is.na(variables$parent_vec)]
  #   )
  
  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "greenspace",
      theme = "Environment",
      nav_title = "Green Space",
      title_text_title = "Green Space",
      title_text_main = paste0(
        "<p>There are many types of green spaces such as parks, forest, playground, ",
        "and community gardens etc. These different types of green spaces all ",
        "contributes to <a href='https://pubmed.ncbi.nlm.nih.gov/26185745/'>health</a> ",
        "and <a href='https://pubmed.ncbi.nlm.nih.gov/25016465/'>environmental</a> benefits. ",
        "Health benefit includes promoting physical activity and social interaction; ",
        "environmental benefit includes improving air quality and combating flooding. ",
        "This module provides an estimation of green spaces, its distribution", 
        " and density in Toronto."
      ),
      title_text_extra = paste0(
        "<p>Data was obtained through querying <a href='https://www.openstreetmap.org/'>",
        "OpenStreetMap (OSM)</a> using <a href='https://overpass-turbo.eu/'>overpass turbo</a>. ",
        "Data comes in as polygons representing the location ana area of green",
        "spaces in Toronto. OSM data comes in as key and value pairs, and green space data",
        " in this module comes from a wide variety of key categories including natural, ",
        "landuse, leisure, menity and boundary. ",
        "<br>Although OSM is the leading platform to provide geographic",
        " information, there are several limitations with using OSM data. ",
        "Firstly, spatial coverage in OSM varies by location and depends on volunteers.",
        " Some regions are more regularly updated compared to others. ",
        "Secondly, Green spaces are complex to define and query because there ",
        "is a wide variety of tags associated with a wide variety of features. Lastly, ",
        "even though some areas are queried as green spaces, such as garden ",
        "and golf course, they might not be publicly accessible."
      ),
      regions = unique(data_interpolated$regions),
      metadata = TRUE,
      dataset_info = paste0(""), 
      var_left = c("green_space_pct"), 
      dates = "2023", 
      main_dropdown_title = "Data grouping", 
      var_right = variables$var_code[variables$source == "Canadian census" &
                                       !is.na(variables$parent_vec)]
    )

  # Return ------------------------------------------------------------------

  return(list(
    scales = with_breaks$scales,
    variables = variables,
    modules = modules
  ))

}
