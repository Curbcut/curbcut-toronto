#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
library(sf)
x <- lapply(list.files("dev/data_import", full.names = TRUE), source, verbose = FALSE)


# Base of the study region and dictionaries -------------------------------

# Possible sequences for autozooms. Every module must have one or multiple of these
# possible scale sequences.
scales_sequences <- list(c("wardCSD", "CT", "DA", "building"),
                         c("CSD", "CT", "DA", "building"),
                         c("ward", "CT", "DA", "building"),
                         c("cmhczone", "CT", "DA", "building"),
                         c("cmhczone"))


# List all the regions geometries to create the master polygon
cancensus_cma_code <- 35535
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 3520005))

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_regions = all_regions,
    region = c("CMA", "city"),
    name = c(CMA = "Metropolitan Area",
             city = "City of Toronto"),
    to_compare = c(CMA = "in the Toronto region",
                   city = "in the City of Toronto"),
    to_compare_determ = c(CMA = "the Toronto region",
                          city = "the City of Toronto"),
    to_compare_short = c(CMA = "in the region",
                         city = "in the City"),
    pickable = c(CMA = TRUE,
                 city = TRUE))


# Build census scales -----------------------------------------------------

census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      crs = crs,
                      DA_carto = base_polygons$DA_carto)

# Create the census scale dictionary
scales_dictionary <- census_scales_dictionary(census_scales)


# Add a Ward scale -----------------------------------------------------

ward <- sf::st_read("dev/data/geometry/WARD_WGS84.shp")
ward <- ward[c("AREA_NAME")]
names(ward)[1] <- "name"
ward <- tibble::as_tibble(ward) |> sf::st_as_sf()

ward <- additional_scale(additional_table = ward,
                          DA_table = census_scales$DA,
                          ID_prefix = "ward",
                          name_2 = "Ward",
                          crs = crs,
                          DA_carto = base_polygons$DA_carto)

# Update CSD naming for Montreal
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "ward",
  sing = "ward",
  sing_with_article = "the ward",
  plur = "wards",
  slider_title = "Ward",
  place_heading = "Ward of {name}",
  place_name = "{name}")

# Merge boroughs with CSDs for the boroughCSD scale
wardCSD <- sf::st_read("dev/data/geometry/WARD_WGS84.shp")
wardCSD <- wardCSD[c("AREA_NAME")]
names(wardCSD)[1] <- "name"
wardCSD$type <- "Ward"
wardCSD <- tibble::as_tibble(wardCSD) |> sf::st_as_sf()

wardCSD <- split_scale(destination = census_scales$CSD,
                       cutting_layer = wardCSD,
                       DA_table = census_scales$DA,
                       crs = crs,
                       DA_carto = base_polygons$DA_carto)

wardCSD$ID <- paste0("ward_", wardCSD$ID)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "wardCSD",
  sing = "ward/city",
  sing_with_article = "the ward/city",
  plur = "wards or cities",
  slider_title = "Ward/City",
  place_heading = "{name_2} of {name}",
  place_name = "{name}")

# Reorder to place boroughCSD first
scales_dictionary <- scales_dictionary[c(5, 1:(nrow(scales_dictionary)-1)), ]


### Build building scale
# # From MySQL
# building <- cc.data::db_read_long_table(table = "buildings",
#                                          DA_ID = census_scales$DA$ID)
# qs::qsave(building, file = "dev/data/built/building.qs")
# # From Local
# building <- qs::qread("dev/data/canada_buildings.qs")
# building <- building[building$DA_ID %in% census_scales$DA$ID, ]
# building <- qs::qsave(building, "dev/data/built/building.qs")
building <- qs::qread("dev/data/built/building.qs")

# Add building scale to the dictionary
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "building",
                             sing = "building",
                             sing_with_article = "the building",
                             plur = "buildings",
                             slider_title = "Building",
                             place_heading = "{name}",
                             place_name = "{name}")

### Build CMHC scale
cmhczone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhczone <- additional_scale(additional_table = cmhczone,
                             DA_table = census_scales$DA,
                             ID_prefix = "cmhc",
                             name_2 = "CMHC zone",
                             crs = crs,
                             DA_carto = base_polygons$DA_carto)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhczone",
                             sing = "CMHC zone",
                             sing_with_article = "the CMHC zone",
                             plur = "CMHC zones",
                             slider_title = "CMHC zone",
                             place_heading = "{name}",
                             place_name = "{name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(building = building),
                list(ward = ward),
                list(wardCSD = wardCSD),
                list(cmhczone = cmhczone))

scales_consolidated <- consolidate_scales(scales_sequences = scales_sequences,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs)

regions_dictionary <- regions_dictionary_add_scales(
  regions_dictionary = regions_dictionary,
  region_dict_scales = scales_consolidated$for_region_dict_scales)

scales_dictionary <- add_regions_to_scales_dictionary(
  scales_dictionary = scales_dictionary, regions = base_polygons$regions,
  scales_consolidated = scales_consolidated,
  DA_carto = base_polygons$DA_carto)


# Verify conformity -------------------------------------------------------

verify_dictionaries(scales = scales_consolidated$scales,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated$scales)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)
scales_variables_modules$data <- lapply(scales_consolidated$scales, \(x) list())

qs::qsavem(census_scales, scales_variables_modules, crs,
           scales_dictionary, regions_dictionary, base_polygons,
           cancensus_cma_code, scales_consolidated, all_scales, scales_sequences,
           file = "dev/data/built/empty_scales_variables_modules.qsm")
qs::qload("dev/data/built/empty_scales_variables_modules.qsm")


# Build the datasets ------------------------------------------------------

future::plan(future::multisession, workers = 4)

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE,
                 scales_sequences = scales_sequences)
census_variables <- get_census_vectors_details()

save.image("dev/data/built/pre_census.RData")
load("dev/data/built/pre_census.RData")

future::plan(future::multisession(), workers = 6)
scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              scales_sequences = scales_sequences,
              crs = crs, geo_uid = cancensus_cma_code,
              approximate_name_match = FALSE)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         scales_sequences = scales_sequences,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             scales_sequences = scales_sequences,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         scales_sequences = scales_sequences,
         crs = crs)

save.image("dev/data/built/pre_ndvi.RData")
load("dev/data/built/pre_ndvi.RData")

scales_variables_modules <-
  ba_ndvi(scales_variables_modules = scales_variables_modules,
          master_polygon = base_polygons$master_polygon,
          all_scales = all_scales,
          skip_scales = c("grd25", "grd50", "grd100", "grd250"),
          scales_sequences = scales_sequences,
          crs = crs)

# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

future::plan(future::multisession(), workers = 3)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs)

save.image("dev/data/built/svm_after_access.qs")
load("dev/data/built/svm_after_access.qs")


invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))


scales_variables_modules <-
  build_and_append_access(scales_variables_modules = scales_variables_modules,
                          DA_table = census_scales$DA,
                          traveltimes = traveltimes,
                          crs = crs)

# Toronto-specific pages
scales_variables_modules <- 
  build_and_append_tree_sqkm(scales_variables_modules = scales_variables_modules,
                             DA_table = census_scales$DA,
                             crs = crs)
scales_variables_modules <- 
  build_and_append_tree_count(scales_variables_modules = scales_variables_modules,
                              DA_table = census_scales$DA,
                              crs = crs)

scales_variables_modules <- 
  build_and_append_green_space_pct(scales_variables_modules = scales_variables_modules,
                             DA_table = census_scales$DA,
                             crs = crs)


# Post process
scales_variables_modules$scales <- 
  cc.buildr::post_processing(scales = scales_variables_modules$scales)

qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           scales_dictionary, regions_dictionary, base_polygons,
           all_scales, scales_sequences,
           file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")



# # Postal codes ------------------------------------------------------------
# 
# postal_codes <- build_postal_codes(census_scales$DA$ID)
# postal_codes <- sf::st_drop_geometry(postal_codes)
# qs::qsave(postal_codes, "data/postal_codes.qs")
# 
# 
# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  scales_sequences = scales_sequences,
  zoom_levels = list(first = 0, CT = 10, DA = 12, building = 16))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# # Tilesets ----------------------------------------------------------------
# 
# tileset_upload_all(all_scales = scales_variables_modules$scales,
#                    map_zoom_levels = map_zoom_levels,
#                    prefix = "to",
#                    username = "curbcut",
#                    access_token = .cc_mb_token)
# 
# 
# Add possible regions to modules -----------------------------------------

scales_variables_modules <- pages_regions(svm = scales_variables_modules,
                                          regions_dictionary = regions_dictionary)


# Place explorer page ----------------------------------------------------

# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = "Explorer",
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "Select a location by entering a postal code or clicking on the map to ",
               "see how it compares to the rest of the region across a variety of sust",
               "ainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             avail_scale_combinations = sapply(scales_sequences, paste0, collapse = "_"),
             regions = regions_dictionary$region)


# Produce colours ---------------------------------------------------------

colours_dfs <- cc.buildr::build_colours()
qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# stories <- build_stories()
# qs::qsave(stories, file = "data/stories.qs")
# stories_create_tileset(stories = stories,
#                        prefix = "to",
#                        username = "curbcut",
#                        access_token = .cc_mb_token)


# Add Montréal stories
scales_variables_modules$modules <-
  scales_variables_modules$modules |>
  add_module(
    id = "stories",
    theme = "Urban life",
    nav_title = "Toronto stories",
    title_text_title = "Toronto stories",
    title_text_main = paste0(
      "Explore narrative case studies about specific urban sustainability and ",
      "planning issues in the Toronto region."),
    title_text_extra = paste0(
      "<p>These narrative case studies are written by the Curbcut team and its contributors."),
    metadata = FALSE,
    dataset_info = ""
  )

# Create DYKs -------------------------------------------------------------

library(tidyverse)
vars_dyk <- dyk_prep(svm = scales_variables_modules, scales_dictionary = scales_dictionary)
variables <- scales_variables_modules$variables
dyk <- dyk_uni(vars_dyk,
               svm = scales_variables_modules,
               langs = "en",
               scales_dictionary = scales_dictionary)
# dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
# dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")


# Home page ---------------------------------------------------------------

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
home_page(modules = scales_variables_modules$modules, 
          stories = qs::qread("data/stories.qs"), 
          translation_df = NULL)


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                   svm_data = scales_variables_modules$data)


# Save .qsm ---------------------------------------------------------------

save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales,
                     skip_scales = c("building"))
save_geometry_export(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)


# Save large dfs as sqlite ------------------------------------------------

save_bslike_sqlite("building", all_scales = scales_variables_modules$scales)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")

# For compare, only keep the large brackets of age
scales_variables_modules$modules$var_right <- lapply(
  scales_variables_modules$modules$var_right, \(x) {
    if (is.null(x)) return(NULL)
    not_age <- x[!grepl("^age_", x)]
    age <- x[grepl("^age_", x)]
    
    age_keep <- age[age %in% c("age_0_14", "age_15_64", "age_65_plus")]
    
    c(not_age, age_keep)
  })

qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")
tictoc::toc()


# Place explorer content creation -----------------------------------------

# # Should be done once the data is saved
# future::plan(future::multisession(), workers = 4)
# 
# pe_main_card_data <- placeex_main_card_data(scales = scales_variables_modules$scales,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary)
# 
# # ONLY KEEP FIRST SCALE
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

placeex_main_card_rmd(scales_variables_modules = scales_variables_modules,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "en",
                      tileset_prefix = "to",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE,
                      scales_sequences = scales_sequences,
                      data_path = here::here("data/") |> paste0("/"))

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")


# Write the data to the bucket --------------------------------------------

cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.toronto.data")


# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-toronto") # Production
# heroku_deploy("cc-toronto-2") # Dev
