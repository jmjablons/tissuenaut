
temp_singles <- d_new_fixed %>%
  filter(key_new %in% girls_single) %>%
  mutate(class_ref = NA) %>%
  bind_rows(
    d_ref_fixed %>%
      filter(key_ref %in% boys_single) %>%
      mutate(class_new = NA))

# temp_uncertain <- d_new_fixed %>%
#   filter(key_new %in% girls_uncertain) %>%
#   select(key_new, class_new) %>%
#   mutate(class_ref = "uncertain") %>%
#   bind_rows(
#     d_ref_fixed %>%
#       filter(key_ref %in% boys_uncertain) %>%
#       select(key_ref, class_ref) %>%
#       mutate(class_new = "uncertain")) %>%
#   mutate(pair_index = -1)

r_class_matrix_raw <- faithful_couples %>%
  bind_rows(temp_singles) %>%
  select(key_new, key_ref, class_ref, class_new) %>%
  mutate(pair_index = ifelse(!is.na(key_new) & 
                               !is.na(key_ref), 1, 0)) %>%
  mutate(pair_index = ifelse(!is.na(key_new) & 
                               !is.na(key_ref), cumsum(pair_index), NA)) %>%
  # bind_rows(temp_uncertain) %>%
  group_by(class_ref, class_new) %>%
  summarise(n = length(pair_index)) %>%
  ungroup() %>%
  #mutate(n = (n/t_n_ref)*100) %>%
  tidyr::pivot_wider(values_from = n, names_from = class_new) #%>%
  # mutate(total_ref = rowSums(across(where(is.numeric)), na.rm = T))
