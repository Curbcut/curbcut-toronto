#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))


# Base of the study region and dictionaries -------------------------------

# All regions
all_tables <-
  list("CMA" = c("CSD", "CT", "DA", "building"),
       "city" = c("CSD", "CT", "DA", "building"),
       "cmhc" = c("cmhczone"))


# List all the regions geometries to create the master polygon
cancensus_cma_code <- 35535
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 3520005),
                    cmhc = get_cmhc_zones(list(CMA = cancensus_cma_code)))

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_tables = all_tables,
    region = c("CMA", "city", "cmhc"),
    name = c(CMA = "Metropolitan Area",
             city = "City of Toronto",
             cmhc = "Canada Mortgage and Housing Corporation zones"),
    to_compare = c(CMA = "in the Toronto region",
                   city = "in the City of Toronto",
                   cmhc = "in the Toronto region"),
    to_compare_determ = c(CMA = "the Toronto region",
                          city = "the City of Toronto",
                          cmhc = "the Toronto region"),
    to_compare_short = c(CMA = "in the region",
                         city = "in the City",
                         cmhc = "in the region"),
    pickable = c(CMA = TRUE,
                 city = TRUE,
                 cmhc = FALSE))


# Build scales ------------------------------------------------------------

### Build census scales
census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      levels = c("CSD", "CT", "DA"),
                      crs = crs)

# Switch the City of Toronto for the Wards
wards <- sf::st_read("dev/data/geometry/WARD_WGS84.shp")
wards <- wards[c("AREA_NAME")]
names(wards)[1] <- "name"
wards$type <- "Ward"
wards <- tibble::as_tibble(wards) |> sf::st_as_sf()

census_scales$CSD <- split_scale(destination = census_scales$CSD,
                                 cutting_layer = wards,
                                 DA_table = census_scales$DA,
                                 crs = crs)

# Create the census scales dictionary
scales_dictionary <- census_scales_dictionary(census_scales)
# Switch the CSD scale for borough/city
scales_dictionary[1, ] <- list(scale = "CSD",
                               sing = "ward/city",
                               plur = "wards or cities",
                               slider_title = "Ward/City",
                               place_heading = "{name_2} of {name}",
                               place_name = "{name}")


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
                             crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhczone",
                             sing = "CMHC zone",
                             plur = "CMHC zones",
                             slider_title = "CMHC zone",
                             place_heading = "{name}",
                             place_name = "{name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(building = building),
                list(cmhczone = cmhczone))

scales_consolidated <- consolidate_scales(all_tables = all_tables,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs)


# Verify conformity -------------------------------------------------------

verify_dictionaries(all_tables = all_tables,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)


save.image("dev/data/built/empty_scales.RData")
load("dev/data/built/empty_scales.RData")
# Build the datasets ------------------------------------------------------

future::plan(list(future::tweak(future::multisession,
                                workers = 2),
                  future::tweak(future::multisession,
                                workers = length(cc.data::census_years))))

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE)
census_variables <- get_census_vectors_details()

scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              crs = crs, 
              geo_uid = cancensus_cma_code,
              approximate_name_match = FALSE)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID)

# # Add access to amenities module
# traveltimes <-
# accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

future::plan(future::multisession(), workers = 3)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes, 
                          crs = crs)

save.image("dev/data/built/scales_variables_modules.RData")
load("dev/data/built/scales_variables_modules.RData")
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


# Postal codes ------------------------------------------------------------

# postal_codes <- build_postal_codes(census_scales$DA$ID)
# qs::qsave(postal_codes, "data/postal_codes.qs")


# Map zoom levels ---------------------------------------------------------

#map_zoom_levels <- map_zoom_levels_create_all(all_tables = all_tables)

#map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# Tilesets ----------------------------------------------------------------

# # street <- cc.data::db_read_data(table = "streets",
# #                                 column_to_select = "DA_ID",
# #                                 IDs = census_scales$DA$ID)
# # qs::qsave(street, "dev/data/built/street.qs")
# street <- qs::qread("dev/data/built/street.qs")
# 
# tileset_streets(master_polygon = base_polygons$master_polygon,
#                 street = street,
#                 crs = crs,
#                 prefix = "to",
#                 username = "sus-mcgill",
#                 access_token = .cc_mb_token)
# 
# tileset_upload_all(all_scales = scales_variables_modules$scales,
#                    map_zoom_levels = map_zoom_levels,
#                    street = street,
#                    prefix = "to",
#                    tweak_max_zoom = tibble::tibble(),
#                    username = "sus-mcgill",
#                    access_token = .cc_mb_token)
# 
# tileset_labels(
#   scales = scales_variables_modules$scales,
#   crs = crs,
#   prefix = "to",
#   username = "sus-mcgill",
#   access_token = .cc_mb_token)


# Did you know ------------------------------------------------------------

# variables <- scales_variables_modules$variables
# source("dev/other/dyk.R")
# qs::qsave(dyk, "data/dyk.qs")


# Produce colours ---------------------------------------------------------

#colours_dfs <- cc.buildr::build_colours()

#qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# stories <- build_stories()
# stories_mapping <- stories$stories_mapping
# stories <- stories$stories
# qs::qsavem(stories, stories_mapping, file = "data/stories.qsm")
# stories_create_tileset(stories = stories,
#                        prefix = "to",
#                        username = "sus-mcgill",
#                        access_token = .cc_mb_token)

scales_variables_modules$modules <-
  scales_variables_modules$modules |>
  add_module(
    id = "stories",
    theme = NA,
    nav_title = "Toronto stories",
    title_text_title = "Toronto stories",
    title_text_main = paste0(
      "<p>Explore stories about urban sustainability and planning in Toronto. Learn ",
      "about stories rooted in specific geographic locations or those that ",
      "have an impact on the whole city."),
    title_text_extra = paste0(
      "<p>These narrative case studies are written by Curbcut contributors."),
    metadata = FALSE,
    dataset_info = ""
  )

# Place explorer page ----------------------------------------------------

# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = NA,
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "<p>Select a location by entering a postal code or clicking on the ",
               "map and see how it compares to the rest of the Greater Toronto Area ",
               "across a variety of sustainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "two exceptions: <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a> and <a href = 'https://www.canueda",
               "ta.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf'>green space</a> data are ta",
               "ken from <a href = 'https://www.canuedata.ca'>CANUE</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             regions = regions_dictionary$region[regions_dictionary$pickable])


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                   all_scales = scales_variables_modules$scales,
                   variables = scales_variables_modules$variables)


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
qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")
tictoc::toc()

# Place explorer content creation -----------------------------------------

# Should be done once the data is saved

future::plan(future::multisession(), workers = 4)

# pe_main_card_data <- placeex_main_card_data(scales = scales_variables_modules$scales,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary)
# 
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")


# Write data to bucket ----------------------------------------------------

cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.toronto.data")


# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-toronto") # Production
# heroku_deploy("cc-toronto-2") # Dev
