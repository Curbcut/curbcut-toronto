## HOME PAGE TIBBLES ###########################################################

home_page <- function(modules, stories, translation_df, data_path = "data/") {
  ### READ ?cc.landing::landing_input DOCUMENTATION TO CONSTRUCT CORRECTLY
  # EACH OF THESE OBJECTS.
  
  # Encode images to base64 for the input
  base64 <- function(x) {
    # Read the JPG image as raw binary data
    image_data <- readBin(x, "raw", file.info(x)$size)
    
    # Encode the image data to base64
    paste0("data:image/jpeg;base64,", base64enc::base64encode(image_data))
  }
  
  
  # Path to the top-left corner SVG image of  -------------------------------
  c_city_svg <- "www/landing/c-toronto.svg"
  
  
  # Tibble for the news section ---------------------------------------------
  
  news_cards <- tibble::tibble(id = character(),
                               icon = character(),
                               title_en = character(),
                               title_fr = character(),
                               text_en = character(),
                               text_fr = character(),
                               link = character())
  
  news_cards <- 
    news_cards |> 
  tibble::add_row(id = "greenness", 
                  icon = "ecology", 
                  title_en = "Explore urban greenery", 
                  title_fr = "Explorez la verdure urbaine", 
                  text_en = paste0(
                    "How green is your neighbourhood? Curbcut’s new Vegetation page uses the",
                    " Normalized Difference Vegetation Index (NDVI) to explore plant health",
                    " and density in urban areas. NDVI plays a significant role in various ",
                    "applications, including analyzing urban greenness, monitoring agricult",
                    "ural growth, and assessing wildfire risks."
                  ), 
                  text_fr = paste0(
                    "À quel point votre quartier est-il vert ? La nouvelle page Végétation ",
                    "de Curbcut utilise l'indice de végétation par différence normalisée (N",
                    "DVI) pour étudier la santé et la densité des plantes dans les zones ur",
                    "baines. Le NDVI joue un rôle important dans diverses applications, not",
                    "amment l'analyse de la verdure urbaine, le suivi de la croissance agri",
                    "cole et l'évaluation des risques d'incendie de forêt."
                  ),
                  link = "ndvi") |> 
    tibble::add_row(id = "lst", 
                    icon = "climat", 
                    title_en = "Land surface temperature", 
                    title_fr = "Température au sol", 
                    text_en = paste0(
                      "Curbcut now has land surface temperature (LST) analytics! LST measures",
                      " the maximum mean warm-season temperature at a specific location, and ",
                      "it is a crucial indicator of urban heat islands and ecological balance",
                      " within a region."
                    ), 
                    text_fr = paste0(
                      "Curbcut propose désormais des analyses de la température au sol ! La t",
                      "empérature au sol mesure la température moyenne maximale en saison cha",
                      "ude à un endroit précis et constitue un indicateur essentiel des îlots",
                      " de chaleur urbains et de l'équilibre écologique au sein d'une région."
                    ),
                    link = "lst") |> 
    tibble::add_row(id = "alp", 
                    icon = "health", 
                    title_en = "Active living potential", 
                    title_fr = "Potentiel de vie active", 
                    text_en = paste0(
                      "Curbcut has developed its own Active Living Potential index. This inde",
                      "x quantifies which areas provide walkable environments to their reside",
                      "nts based on street connectivity, building density and points of inter",
                      "est. We developed the index by building a travel time matrix for the e",
                      "ntire country, using a 15-minute walk buffer on the street network."
                    ), 
                    text_fr = paste0(
                      "Curbcut a développé son propre indice de potentiel de vie active. Cet ",
                      "indice quantifie les zones qui offrent des environnements propices à l",
                      "a marche à leurs résidents en fonction de la connectivité des rues, de",
                      " la densité des bâtiments et des points d'intérêt. Nous avons développ",
                      "é cet indice à l'aide de notre matrice interne de données sur les temp",
                      "s de déplacement, qui utilise un tampon de 15 minutes de marche sur le",
                      " réseau de rues."
                    ),
                    link = "alp") 
  
  
  # Tibble for the discover section -----------------------------------------
  
  # Function to remove all HTML tags from a given string
  remove_html_tags <- function(input_vector) {
    # Use gsub to replace all HTML tags with an empty string
    output_vector <- gsub("<[^>]*>", "", input_vector)
    return(output_vector)
  }
  
  # Pages from the modules
  disc_modules <- modules[c("id", "title_text_title", "title_text_main", "theme")]
  names(disc_modules) <- c("id", "en", "preview_en", "theme")
  disc_modules$img <- sprintf("%s.png", disc_modules$id)
  disc_modules$theme <- gsub(" .*", "", disc_modules$theme) |> tolower()
  disc_modules$preview_fr <- ""
  disc_modules$preview_fr <- ""
  disc_modules$preview_en <- remove_html_tags(disc_modules$preview_en)
  disc_modules$fr <- sapply(disc_modules$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_modules <- disc_modules[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr")]
  disc_modules$type <- "page"
  disc_modules$select_id <- NA_character_
  disc_modules$var_left <- NA
  disc_modules$var_right <- NA
  disc_modules$page <- NA
  disc_modules$date <- NA
  disc_modules$scale <- NA
  
  # Stories formatting for the discover_cards
  disc_stories <- stories[c("name_id", "short_title", "preview_en", "preview_fr", "ID")]
  names(disc_stories) <- c("id", "en", "preview_en", "preview_fr", "select_id")
  disc_stories$select_id <- as.character(disc_stories$select_id)
  disc_stories$img <- sprintf("%s.png", disc_stories$id)
  disc_stories$theme <- "urban"
  disc_stories$fr <- sapply(disc_stories$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_stories$type <- "stories"
  disc_stories$var_left <- NA
  disc_stories$var_right <- NA
  disc_stories$page <- NA
  disc_stories$date <- NA
  disc_stories$scale <- NA
  # disc_stories <- disc_stories[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr", "type", "select_id")]
  
  # DYK for discovers
  dyk_discover_fun <- function(theme, page, var_right = " ", var_left = NULL, 
                               date, type, region = NULL, scale = NULL, en, fr) {
    
    # Filter page, var_right, type
    this <- dyk[dyk$module == page & dyk$var_right == var_right & dyk$dyk_type == type, ]
    
    # Filter var_left if supplied
    if (!is.null(var_left)) {
      this <- dyk[dyk$var_left == var_left, ]
    }
    
    # Filter date over the list column
    this <- this[unlist(sapply(this$date, identical, as.character(date))), ]
    
    # Filter region and scale. Use of `identical` for when scale is NA.
    if (is.null(region) & is.null(scale)) {
      this <- this[this$region == this$region[1] & unlist(sapply(this$scale, identical, this$scale[1])), ]
    } else {
      this <- this[this$region == region & this$scale == scale, ]
    }
    
    # Grab last row
    this <- this[nrow(this), ]
    
    # Discover columns
    this$id <- sprintf("%s_dyk1", page)
    this$img <- sprintf("%s.png", this$id)
    this$theme <- theme
    this$en <- en
    this$fr <- fr
    names(this)[names(this) == "dyk_text_en"] <- "preview_en"
    this$preview_fr <- ""
    names(this)[names(this) == "module"] <- "page"
    names(this)[names(this) == "select_ID"] <- "select_id"
    this$type <- "dyk"
    
    # Return
    this[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr", "type",
           "page", "var_left", "var_right", "select_id", "date", "scale")]
  }
  
  disc_dyk <-
    tibble::tibble() |>
    rbind(dyk_discover_fun(theme = "health",
                           page = "alp",
                           var_right = "housing_single_detached",
                           date = 2021,
                           type = "compare",
                           en = "Dense, walkable neighbourhoods",
                           fr = "Quartiers denses et accessibles à pied")) |>
    rbind(dyk_discover_fun(theme = "climate",
                           page = "lst",
                           var_right = " ",
                           date = 2021,
                           type = "lowest",
                           region = "CMA",
                           scale = "wardCSD",
                           en = "The coolest town in the region",
                           fr = "La ville la plus fraîche de la région")) |>
    rbind(dyk_discover_fun(theme = "ecology",
                           page = "ndvi",
                           var_right = "housing_tenant",
                           date = 2021,
                           type = "compare",
                           en = "Tenants lack green space",
                           fr = "Les locataires manquent d'espaces verts")) |>
    rbind(dyk_discover_fun(theme = "housing",
                           page = "housing",
                           var_left = "housing_rent",
                           date = c("1996", "2021"),
                           type = "change",
                           en = "Skyrocketing housing costs",
                           fr = "La flambée des prix du logement")) |>
    rbind(dyk_discover_fun(theme = "transport",
                           page = "canbics",
                           var_right = " ",
                           date = c("2021"),
                           type = "highest",
                           region = "CMA",
                           scale = "wardCSD",
                           en = "The best bikelanes",
                           fr = "Les meilleures pistes cyclables"))
  
  
  # Bindthe modules with the stories and the DYK
  discover_cards <- rbind(disc_modules, disc_stories, disc_dyk)
  
  # Filter out missing photos and warn!
  present_img <- discover_cards$img %in% list.files("www/landing/discover/")
  missing_img <- discover_cards[!present_img, ]
  if (nrow(missing_img) > 0){
    warning(paste0("Missing images for ", missing_img$id, "\n"))
  }
  
  discover_cards <- discover_cards[present_img, ]
  discover_cards$img <- paste0("www/landing/discover/", discover_cards$img)
  
  if (length(unique(discover_cards$id)) != nrow(discover_cards)) {
    stop("Discover cards do not have unique ids")
  }
  
  discover_cards$img <- sapply(discover_cards$img, base64)
  
  # Tibble for team members -------------------------------------------------
  team_cards <- tibble::tibble(
    id = c("davidw", "kevinm", "maxbdb", "dominiqueb", "christophb", "eshtab", "hanq"),
    img = c(
      "www/landing/team/david_wachsmuth.jpeg",
      "www/landing/team/kevin_manaugh.jpg",
      "www/landing/team/maxime_belanger_de_blois.jpg",
      "www/landing/team/dominique_boulet.jpg",
      "www/landing/team/christoph_becker.jpg",
      "www/landing/team/eshta_bhardwaj.jpeg",
      "www/landing/team/han_qiao.jpg"
    ),
    name = c("David Wachsmuth", 
             "Kevin Manaugh", 
             "Maxime Bélanger De Blois", 
             "Dominique Boulet",
             "Christoph Becker",
             "Eshta Bhardwaj",
             "Han Qiao"),
    role_en = c("Co-founder & Co-CEO", 
                "Co-founder & Co-CEO", 
                "Head of Technology and Data", 
                "Qualitative Research Lead",
                "Toronto team",
                "Toronto team",
                "Toronto team"),
    role_fr = c("Co-fondateur et co-PDG", 
                "Co-fondateur et co-PDG", 
                "Responsable technologie et données", 
                "Responsable de la recherche qualitative",
                "Équipe Toronto",
                "Équipe Toronto",
                "Équipe Toronto"),
    theme = c("housing", "transport", "health", "urban", "climate", "climate", "climate")
  )
  
  team_cards$img <- sapply(team_cards$img, base64)
  
  # Character vector for contributors ---------------------------------------
  contributors <- c(""
  )
  
  
  # Tibble for collaborators ------------------------------------------------
  collabs <- tibble::tibble(
    id = c("MSSI", "centraide"),
    img = c(
      "www/landing/collab/mcgill-logo.png",
      "www/landing/collab/uoft-logo.png"
    ),
    name = c("The McGill Sustainability Systems Initiative", 
             "Digital Curation Institute")
  )
  
  collabs$img <- sapply(collabs$img, base64)
  
  
  # Save home page information as qsm ---------------------------------------
  if (!exists("data_path")) data_path <- "data/"
  
  qs::qsavem(c_city_svg, news_cards, discover_cards,
             team_cards, contributors, collabs,
             file = paste0(data_path, "home_page.qsm")
  )
}
