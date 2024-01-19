
source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

# read data ---------------------------------------------------------------

d_pairs <- util$read_csv(temp$d_pairs) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# combine -----------------------------------------------------------------

d_morphology <- d_pairs %>%
  #filter(new_class %!in% "smooth") %>%
  rowwise() %>%
  mutate(div_maxcaliper = new_nucleus_max_caliper - ref_max_diameter_um) %>%
  mutate(div_mincaliper = new_nucleus_min_caliper - ref_min_diameter_um) %>%
  mutate(div_area = new_nucleus_area - `ref_area_um^2`) %>%
  mutate(div_circularity = new_nucleus_circularity - ref_circularity) %>%
  ungroup()

# save --------------------------------------------------------------------

write.csv2(d_morphology, file = paste("blobs_analysis/",
                                 "result/data_morphology", "_", 
                                 temp_classif_short,"_", 
                                 util$today(), 
                                 sep = ""))
