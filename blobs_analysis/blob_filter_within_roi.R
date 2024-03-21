
# continue after 'blob_init_blobs.R'

source("main_init.R")
source("blobs_analysis/blob_links.R") #inform source files
source("blobs_analysis/value.R")

# read --------------------------------------------------------------------

d_blobs_raw <- util$read_csv(link$d_blobs_raw) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo
d_clasif <- util$read_csv(link$test_file) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# filter by sample included -----------------------------------------------

temp$ref_key_df <- d_blobs_raw %>%
  summarise(if_in = T, .by = name$sample_identif) %>%
  mutate(image = as.numeric(image),
         position_rostral = as.numeric(position_rostral))

d_clasif = d_clasif %>%
  filter(class %!in% temp$classes_out) %>%
  left_join(temp$ref_key_df, by = join_by(image, position_rostral, hemi)) %>%
  filter(if_in == TRUE)

# roi coordinates ---------------------------------------------------------

guide_frame <- d_blobs_raw %>%
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
  d_blobs_raw %>%
  filter(class %!in% c(name$roi_class, name$marking_class)) %>%
  left_join(guide_frame) %>%
  filter(centroid_x_um < endx_um) %>% 
  filter(centroid_y_um < endy_um) %>%
  filter(centroid_x_um > startx_um) %>%
  filter(centroid_y_um > starty_um)

# prepare cells -----------------------------------------------------------

d_cells = d_clasif %>%
  filter(class %in% temp$classes_in) %>%
  rename_with(~stringr::str_replace(.,"µ", "u")) %>%
  left_join(guide_frame) %>%
  filter(centroid_x_um < endx_um) %>% 
  filter(centroid_y_um < endy_um) %>%
  filter(centroid_x_um > startx_um) %>%
  filter(centroid_y_um > starty_um)

# save --------------------------------------------------------------------

write.csv2(d_blobs, file = paste("blobs_analysis/",
                                 "data/data_blobs_roi",
                                  sep = ""))

link$d_blobs_raw %>%
  write.table(., file = paste(link$dir_path, "data/data_blobs_roi",
                    "_info_source.txt", sep = ""))

write.csv2(d_cells, file = paste("blobs_analysis/", 
                                 "data/data_cells_roi", "_", 
                                 temp_classif_short,
                                 sep = ""))
link$test_file %>%
  write.table(., file = paste(link$dir_path, "data/data_cells_roi", "_", 
                    temp_classif_short, "_info_source.txt", sep = ""))
