## BUILD STORIES ###############################################################

build_stories <- function() {
  
  # Build empty table -------------------------------------------------------
  
  stories <- stories_empty_table()
  
  # Add every story ---------------------------------------------------------
  
  stories <- 
    stories |> 
    stories_add_story(
      name_id = "highpark",
      title = paste0("Community Activism for a Car-Free High Park"),
      short_title = "Car Free High Park",
      preview_en = paste0("High Park is a paradise for residents in Toronto to enjoy ",
                        "outdoor activities, but at the same time, it is ",
                       "also a battleground that carries conflict between cars, ",
                       "cyclists and pedestrians on who gets access to the park."),
      preview_fr = "",
      themes = c("Community activism", "Transporation"),
      lon = -79.463064, 
      lat = 43.646756)
  
  
  # Create images and mapping -----------------------------------------------
  
  stories <- stories_map_images(stories = stories)
  
  
  # Knit all stories Rmds ---------------------------------------------------
  
  library(here)
  cc.buildr::stories_knit_all()
  
  
  # Return ------------------------------------------------------------------
  
  return(stories)
  
}
