## BUILD STORIES ###############################################################

build_stories <- function() {
  
  # Build empty table -------------------------------------------------------
  
  stories <- stories_empty_table()
  
  # Add every story ---------------------------------------------------------
  
  stories <- 
    stories |> 
    stories_add_story(
      name_id = "TKTK",
      title = paste0("TKTK"),
      short_title = "TKTK",
      preview = paste0("TKTK"),
      themes = c("Environmental racism", "Green gentrification", 
                 "Community activism", "Urban renewal", "Neighbourhood history"),
      lon = -73.574962, 
      lat = 45.479311)
  
  
  # Create images and mapping -----------------------------------------------
  
  stories_mapping <- stories_atlas_mapping(stories = stories)
  
  
  # Knit all stories Rmds ---------------------------------------------------
  
  library(here)
  cc.buildr::stories_knit_all()
  
  
  # Return ------------------------------------------------------------------
  
  return(list(stories = stories,
              stories_mapping = stories_mapping))
  
}
