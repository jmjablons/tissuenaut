
# continue after 'blob_prepare.R'

source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

# read data ---------------------------------------------------------------

d_blobs <- util$read_csv(temp$d_blobs) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo
d_cells <- util$read_csv(temp$d_cells) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# dummy table -------------------------------------------------------------

if(is.null(temp$ref_key_df)){
  temp$ref_key_df = d_blobs %>% summarise(.by = name$sample_identif)}

# run pair ----------------------------------------------------------------

combinations <- lapply(X = 1:nrow(temp$ref_key_df), FUN = function(x){
  setTxtProgressBar((txtProgressBar(min = 0,
                                    max = nrow(temp$ref_key_df),
                                    style = 3,
                                    width = nrow(temp$ref_key_df),
                                    char = "=")), x)
  temp_sample_identif <- temp$ref_key_df[x,]
  
  d_cells_small = d_cells %>%
    inner_join(temp_sample_identif, by = name$sample_identif) %>%
    select(name$sample_identif,
           id_new = name$data_id, 
           the_important_measure = name$the_important_measure, 
           x_new = name$cells_x_coord,
           y_new = name$cells_y_coord)
  
  d_blobs_small = d_blobs %>%
    inner_join(temp_sample_identif, by = name$sample_identif) %>%
    select(id_ref = name$data_id, 
           x_ref = name$blobs_x_coord, 
           y_ref = name$blobs_y_coord)
  
  return(map_df(1:nrow(d_cells_small), function(i) {
    d_cells_small[i,] %>% 
      bind_cols(d_blobs_small) %>%
      rowwise() %>%
      mutate(dist = minkowski_distance(
        x = .data[[name$x_new]], x1 = .data[[name$x_ref]],
        y = .data[[name$y_new]], y1 = .data[[name$y_ref]], 
        val$p),
        dist_x = abs(.data[[name$x_new]] - .data[[name$x_ref]]),
        dist_y = abs(.data[[name$y_new]] - .data[[name$y_ref]])) %>%
      filter(dist < minkowski_distance(
        0, val$threshold_value, 
        0, val$threshold_value, 
        val$p),
        dist_x < (the_important_measure/2),
        dist_y < (the_important_measure/2)) %>%
      ungroup()}))})

combinations_filtered <- lapply(X = combinations, FUN = function(x){
  return(x %>%
           group_by(id_ref) %>%
           mutate(softmax_dist = softmax(-dist)) %>%
           ungroup() %>%
           arrange(id_ref) %>%
           ungroup() %>%
           arrange(dist) %>%
           filter(softmax_dist == max(softmax_dist), .by = "id_new") %>%
           filter(softmax_dist == max(softmax_dist), .by = "id_ref") %>%
           arrange(id_new))})

# pairs analysis ----------------------------------------------------------

# pairs_listed <- combinations_filtered %>% bind_rows() %>% #previous: pairs$first
#   select(name$sample_identif, id_new, id_ref) %>%
#   arrange(id_new)
# 
# #check duplicates
# sum(duplicated(pairs_listed$id_ref))

# add unpaired cells & blobs ----------------------------------------------

pairs <- lapply( #previous pairs$total
  X = combinations_filtered, 
  FUN = function(x){
    temp_sample_identif <- x %>% 
      summarise(.by = name$sample_identif)
    temp_ids <- list(
      ids_blobs = with((d_blobs %>% 
                          inner_join(temp_sample_identif, 
                                     by = name$sample_identif)), 
                       unique(object_id)),
      ids_cells = with((d_cells %>% 
                          inner_join(temp_sample_identif, 
                                     by = name$sample_identif)), 
                       unique(object_id)))
    temp_unpaired <- list(
      ref = setdiff(temp_ids$ids_blobs, x$id_ref),
      new = setdiff(temp_ids$ids_cells, x$id_new))
    return(x %>%
             select(name$sample_identif, id_new, id_ref) %>%
             ungroup() %>%
             bind_rows(tibble(id_ref = temp_unpaired$ref) %>%
                         cross_join(temp_sample_identif)) %>%
             bind_rows(tibble(id_new = temp_unpaired$new) %>%
                         cross_join(temp_sample_identif)) %>%
             rowwise() %>%
             mutate(paired = all(!is.na(c(id_new, id_ref)))) %>%
             ungroup() %>%
             arrange(paired))})

### check
# pairs %>% bind_rows() %>% filter(!is.na(id_ref) & duplicated(id_ref))
# pairs %>% bind_rows() %>% filter(!is.na(id_new) & duplicated(id_new))

# further -----------------------------------------------------------------
# add pair key and measures

d_pairs <- pairs %>%
  bind_rows() %>%
  mutate(pair_index = ifelse(!is.na(id_new) & !is.na(id_ref), 1, 0)) %>%
  group_by(across(name$sample_identif)) %>%
  mutate(pair_index = ifelse(!is.na(id_new) & 
                               !is.na(id_ref), 
                             cumsum(pair_index), NA)) %>%
  ungroup() %>%
  left_join(d_cells %>%
              rename_with(~stringr::str_c("new_", .), 
                          -any_of(name$sample_identif)) %>%
              rename(id_new = new_object_id)) %>%
  left_join(d_blobs %>% 
              rename_with(~stringr::str_c("ref_", .), 
                          -any_of(name$sample_identif)) %>%
              rename(id_ref = ref_object_id) %>%
              mutate(ref_class = ifelse(!is.na(id_ref), 
                                        name$ref_class, ref_class)))

### check
# d_pairs$pair_index %>% na.omit() %>% range() #number of pairs

# export ------------------------------------------------------------------

write.csv2(d_pairs, 
           file = paste("blobs_analysis/data/data_pairs_preprocessed", 
                        "_", util$today(), sep = ""))

# validation --------------------------------------------------------------

r_summary_raw <- d_pairs %>%
  summarise(n_unpaired_new = length(which(is.na(id_ref))),
            n_unpaired_ref = length(which(is.na(id_new))),
            n_doubled_ref = length(which(duplicated(id_ref) & 
                                           !is.na(id_ref))),
            n_doubled_new = length(which(duplicated(id_new) & 
                                           !is.na(id_new))),
            n_paired_without_doubled = length(which(!duplicated(id_new) & 
                                                      !is.na(id_new) & 
                                                      paired == T)),
            n_paired_with_doubled = length(which(paired == T)),
            n_ref = length(which(!is.na(id_ref))),
            n_new = length(unique(id_new)), 
            .by = name$sample_identif)

# r_summary_prec <- r_summary_raw %>% mutate_all(~(.x/n_ref)*100)
