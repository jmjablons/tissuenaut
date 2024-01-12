
source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

# read data ---------------------------------------------------------------

d_pairs <- util$read_csv(temp$d_pairs) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# mine --------------------------------------------------------------------

temp <- list() #anew
temp$df <- d_pairs %>%
  select(new_class, ref_class, 
         new_measure = name$area_cells, 
         ref_measure = name$area_blolbs, 
         new_x = sprintf("new_%s", name$cells_x_coord), 
         new_y = sprintf("new_%s", name$cells_y_coord), 
         ref_x = sprintf("ref_%s", name$blobs_x_coord), 
         ref_y = sprintf("ref_%s", name$blobs_y_coord),
         pair_index,
         id_new, id_ref, 
         name$sample_identif)

d_points <- map_df(name$group_to_compare, function(p){
  return(temp$df %>%
           select(name$sample_identif, pair_index, all_of(contains(p))) %>%
           filter(if_all(contains("id"), ~!is.na(.x))) %>%
           #filter(!is.na(across(contains("id")))) %>%
           rename_with(
             ~stringr::str_replace_all(.,
                                       pattern = paste("\\_{0,}",n,"\\_{0,}", sep = ""), 
                                       replacement = "")) %>%
           mutate(group = paste(p)))})

d_points = d_points %>%
  mutate(r = radius(measure)) %>% #'util.R': radius
  tidyr::unite(name$sample_identif, sep = "_", col = "sample") %>%
  mutate(pair_index = ifelse(is.na(pair_index), 0, pair_index))
