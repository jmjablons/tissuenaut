
# continue after 'blob_init.R'

source("main_init.R")
source("blobs_analysis/blob_source_data.R") #inform source files
source("blobs_analysis/name.R")

# variables ---------------------------------------------------------------

temp$classes_in = c("sharp", "smooth", 
                    "strong", "weak", 
                    "weak_stain2", "strong_stain2",
                    "strong2", "weak2")
temp$classes_out = c("background", "artifact", 
                     "artifact2", "noise", "Ignore*")

# read --------------------------------------------------------------------

d_ref <- util$read_csv(temp$ref_file) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo
d_clasif <- util$read_csv(temp$test_file) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# filter by sample included -----------------------------------------------

temp$ref_key_df <- d_ref %>%
  summarise(if_in = T, .by = name$sample_identif)

d_clasif = d_clasif %>%
  filter(class %!in% temp$classes_out) %>%
  left_join(temp$ref_key_df) %>%
  filter(if_in == TRUE)

# roi coordinates ---------------------------------------------------------

d_frame <- d_ref %>%
  filter(class %in% name$roi_class) %>%
  mutate(across(sprintf(name_roi$roi_position_columns), 
                .names = "{.col}_um", 
                ~.x*val$pixel_width)) %>% #equals pixel_height
  rowwise() %>% 
  mutate(endx_um = sum(across(c(name_roi$roi_position_um_columns[["x0"]], 
                                name_roi$roi_position_um_columns[["dx"]]))), 
         endy_um = sum(across(c(name_roi$roi_position_um_columns[["y0"]], 
                                name_roi$roi_position_um_columns[["dy"]])))) %>%
  ungroup() %>%
  select(name$sample_identif, 
         startx_um = name_roi$roi_position_um_columns[["x0"]], endx_um,
         starty_um = name_roi$roi_position_um_columns[["y0"]], endy_um)


# prepare blobs -----------------------------------------------------------
# JIC filter out by ROI

d_blobs = 
  d_ref %>%
  filter(class %!in% c(name$roi_class, name$marking_class)) %>%
  left_join(d_frame) %>%
  filter(centroid_x_um < endx_um) %>% 
  filter(centroid_y_um < endy_um) %>%
  filter(centroid_x_um > startx_um) %>%
  filter(centroid_y_um > starty_um)

# prepare cells -----------------------------------------------------------

d_cells = d_clasif %>%
  filter(class %in% temp$classes_in) %>%
  left_join(d_frame) %>%
  filter(centroid_x_µm < endx_um) %>% 
  filter(centroid_y_µm < endy_um) %>%
  filter(centroid_x_µm > startx_um) %>%
  filter(centroid_y_µm > starty_um)

# save --------------------------------------------------------------------

write.csv2(d_blobs, file = paste("blobs_analysis/",
                                 "data/data_blobs_preprocessed", "_", 
                                  util$today(), 
                                  sep = ""))

write.csv2(d_cells, file = paste("blobs_analysis/", 
                                 "data/data_cells_preprocessed", "_", 
                                 temp_classif_short,"_", 
                                 util$today(), 
                                 sep = ""))
