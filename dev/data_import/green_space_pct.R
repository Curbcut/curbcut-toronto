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
  data <- sf::st_as_sf(data)


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
    data_interpolated$interpolated_ref$scale == "DA"
  ] <- "green space polygons" #check text: telling user how data is interpolated
  
  data_interpolated$interpolated_ref$interpolated_from[
    data_interpolated$interpolated_ref$scale == "DA"
  ] <- "green space polygons"
  
  # Data tibble -------------------------------------------------------------
  
  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = data_interpolated$scales,
                         unique_var = "green_space_pct",
                         time_regex = "_\\d{4}$")

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
      theme = "Ecology",
      private = FALSE,
      pe_include = TRUE,
      parent_vec = NA,
      dates = 2023,
      avail_scale = data_interpolated$avail_scale, 
      source = "OpenStreetMap",
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
      id = "greenspace",
      theme = "Ecology",
      nav_title = "Green space",
      title_text_title = "Green space",
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
      metadata = TRUE,
      dataset_info = paste0(""), 
      var_left = c("green_space_pct"), 
      dates = "2023", 
      main_dropdown_title = "Data grouping", 
      var_right = variables$var_code[variables$source == "Canadian census" &
                                       !is.na(variables$parent_vec)],
      default_var = "green_space_pct",
      avail_scale_combinations = avail_scale_combinations
    )

  # Return ------------------------------------------------------------------

  return(list(
    scales = data_interpolated$scales,
    variables = variables,
    modules = modules,
    data = data
  ))

}
