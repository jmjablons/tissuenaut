
library(readr)
source("main_init.R")

# value -------------------------------------------------------------------
source("hip_analysis/hip_temp_linked.R")

# import  -----------------------------------------------------------------

d_slit <- read_delim(temp$source_new, 
             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
             col_types = cols(...1 = col_skip()), trim_ws = TRUE) %>%
  filter(class %in% c("piramid", "just_cell", "maybe_cell", "light_cell"))%>%
  mutate(image = as.character(image)) %>%
  filter(image %in% temp$image_string, position_rostral %in% temp$position_rostral) %>%
  mutate(image = as.character(image), 
         position_rostral = as.character(position_rostral))

# manual ------------------------------------------------------------------

d_man <- read_delim(temp$source_manual, 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) %>%
  dplyr::rename_with(tolower) %>%
  tidyr::separate_wider_delim(image,"_",names = c("image","slice")) %>%
  mutate(slice = gsub(x = slice, pattern = "\\.ome.tif", replacement = "")) %>%
  rename(structure = parent) %>%
  dplyr::rename_with( ~ stringr::str_replace_all(., "\\s+", "_")) %>%
  dplyr::rename_with( ~ stringr::str_replace_all(., "\\:", "")) %>%
  tidyr::separate_wider_regex(cols = slice, 
                              c(position_rostral = "\\d", hemi = "[a-z]{1,}"), 
                              too_few = "align_start")

# shape dataset -----------------------------------------------------------

d_ref_fixed <- d_man %>%
  as_tibble() %>%
  mutate(r_ref = sqrt((100/pi))) %>%
  mutate(x = `centroid_x_µm` * val$pixel_width, #* val$pixel_width
         y = `centroid_y_µm` * val$pixel_width) %>%
  mutate(source_ref = "manual",
         key_ref = row_number()) %>%
  rename(x_ref = x, 
         y_ref = y,
         class_ref = class)

d_new_fixed <- d_slit %>%
  mutate(x = `centroid_x_µm` * val$pixel_width,
         y = `centroid_y_µm` * val$pixel_width) %>%
  select(x_new = x,
         y_new = y,
         max_caliper_new = `nucleus_max_caliper`,
         class_new = class,
         area_new = `nucleus_area`) %>%
  mutate(r_new = sqrt((area_new/pi))) %>%
  mutate(source_new = "classif",
         key_new = row_number())

# quick plot --------------------------------------------------------------

d_new_fixed %>%
  ggplot(aes(x0 = x_new, y0 = -y_new, r = r_new, fill = source_new))+
  geom_circle(alpha = 0.5, colour = NA)+
  geom_circle(aes(x0 = x_ref, y0 = -y_ref, r = r_ref, fill = source_ref),
              alpha = 0.5, colour = NA, data = d_ref_fixed)

# compare -----------------------------------------------------------------

source("points_analysis/protagonist_real.R")

r_summary %>% writexl::write_xlsx("./hip_analysis/result_manual_check.xlsx")

#source("points_analysis/plot_code/plot_pairs_colored.R")
