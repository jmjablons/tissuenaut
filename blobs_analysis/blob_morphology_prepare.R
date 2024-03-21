
source("main_init.R")
source("blobs_analysis/blob_links.R")
source("blobs_analysis/value.R")

# read data ---------------------------------------------------------------

d_blobs <- util$read_csv(link$d_blobs) %>%
  select(-any_of(c('...1'))) #may silence the name repair

d_cells <- util$read_csv(link$d_cells) %>%
  select(-any_of(c('...1'))) #may silence the name repair

# combine -----------------------------------------------------------------

d_morphology_blobs <- d_blobs %>%
  rename(
    maxcaliper = max_diameter_um,
    mincaliper = min_diameter_um,
    area = `area_um^2`,
    circularity = circularity) %>%
  select(name$sample_identif, object_id, class,
         centroid_x_um, centroid_y_um,
         all_of(name$morphology_measures)) %>%
  mutate(class = "painter",
         type = "blob")

d_morphology_cells <- d_cells %>%
  rename(
    maxcaliper = nucleus_max_caliper,
    mincaliper = nucleus_min_caliper,
    area = nucleus_area,
    circularity = nucleus_circularity) %>%
  select(name$sample_identif, object_id, class,
         centroid_x_um, centroid_y_um,
         all_of(name$morphology_measures)) %>%
  mutate(type = "cell")

bind_rows(d_morphology_blobs, d_morphology_cells) %>%
  group_by(image, position_rostral, hemi, class, type) %>%
  summarise(n = n(), median(maxcaliper), IQR(maxcaliper))

# save --------------------------------------------------------------------

write.csv2(d_morphology_blobs, file = paste("blobs_analysis/",
                                      "data/data_morphology_blobs",
                                      sep = ""))

tibble(blobs = link$d_blobs) %>% 
  write.table(., file = paste(link$dir_path, "data/data_morphology_blobs",
                              "_info_source.txt", sep = ""))

write.csv2(d_morphology_cells, file = paste("blobs_analysis/",
                                            "data/data_morphology_cells", "_", 
                                            temp_classif_short, 
                                            sep = ""))

tibble(blobs = link$d_cells) %>% 
  write.table(., file = paste(link$dir_path, "data/data_morphology_cells", "_", 
                              temp_classif_short, "_info_source.txt", sep = ""))
