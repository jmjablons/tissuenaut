
#t_n_ref <- r_summary_raw$n_ref
  
r_class_matrix_raw <- d_pairs %>%
  group_by(class_ref, class_new) %>%
  summarise(n = length(pair_index)) %>%
  ungroup() %>%
  #mutate(n = (n/t_n_ref)*100) %>%
  tidyr::pivot_wider(values_from = n, names_from = class_new) %>%
  mutate(total_ref = rowSums(across(where(is.numeric)), na.rm = T))

#TODO contains duplicates
    
# summarise(n_unpaired_ref = length(which(is.na(key_ref))),
#           n_unpaired_new = length(which(is.na(key_new))),
#           #n_unpaired = length(which(paired == F)),
#           n_doubled_ref = length(which(duplicated(key_ref) & !is.na(key_ref))),
#           n_doubled_new = length(which(duplicated(key_new) & !is.na(key_new))),
#           n_paired_without_doubled = length(which(!duplicated(key_new) & !is.na(key_new) & paired == T)),
#           n_paired_with_doubled = length(which(paired == T)),
#           n_ref = length(which(!is.na(key_ref))),
#           n_new = length(unique(key_new)))
