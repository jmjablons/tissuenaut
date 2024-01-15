
library(imager)
require(dplyr)

# in and out --------------------------------------------------------------

source("settings_scaledown_images.R") #temp_files #out_dir

# variables ---------------------------------------------------------------

m_dream_height <- 300

# wrap --------------------------------------------------------------------

purrr::map(1:length(temp_files), function(x){
  temp_image_path <- temp_files[x]
  temp_image_id <- stringr::str_match(string = temp_image_path, pattern = ".{22}$") %>%
    gsub(pattern = "\\/", replacement = "") %>%
    gsub(pattern = ".ome.tif.png", replacement = "")
  temp_image <- imager::load.image(temp_image_path)
  #plot(temp_image)
  m_factor = m_dream_height/imager::height(temp_image)
  temp_dream_width = ceiling(imager::width(temp_image) * m_factor)
  temp_image_downscaled <- resize(temp_image, temp_dream_width, m_dream_height, interpolation_type = 2)
  #plot(temp_image_downscaled)
  imager::save.image(temp_image_downscaled, paste0(c(out_dir, temp_image_id, ".png"), collapse = ""))
})

