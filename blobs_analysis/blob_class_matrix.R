
source("main_init.R")
source("blobs_analysis/name.R")

# read data ---------------------------------------------------------------

d_pairs <- util$read_csv(temp$d_pairs) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# get matrix --------------------------------------------------------------

d_pairs %>%
  tidyr::unite(name$sample_identif, sep = "_", col = "sample") %>%
  mutate(pair_index = ifelse(is.na(pair_index), 0, pair_index)) %>%
  summarise(n = length(pair_index), .by = c("sample", ref_class, new_class)) %>%
  tidyr::pivot_wider(values_from = n, names_from = new_class) %>%
  mutate(total_ref = rowSums(across(where(is.numeric)), na.rm = T))

d_pairs %>%
  filter(new_class %!in% name$without_this_class) %>%
  tidyr::unite(name$sample_identif, sep = "_", col = "sample") %>%
  mutate(pair_index = ifelse(is.na(pair_index), 0, pair_index)) %>%
  summarise(n = length(pair_index), .by = c("sample", ref_class, new_class)) %>%
  tidyr::pivot_wider(values_from = n, names_from = new_class) %>%
  mutate(total_ref = rowSums(across(where(is.numeric)), na.rm = T))
