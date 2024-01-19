
source("points_analysis/name.R")

# out ---------------------------------------------------------------------

combinations <- list()
#w_check <- list()
pairs <- list()

combinations <- map_df(1:nrow(d_new_fixed), function(i) {
  d_new_fixed[i,] %>% 
    bind_cols(d_ref_fixed) %>%
    rowwise() %>%
    mutate(dist = minkowski_distance(
      x = .data[[name$new_x]], x1 = .data[[name$ref_x]],
      y = .data[[name$new_y]], y1 = .data[[name$ref_y]], val$p),
      dist_x = abs(.data[[name$new_x]] - .data[[name$ref_x]]),
      dist_y = abs(.data[[name$new_y]] - .data[[name$ref_y]])) %>%
    filter(dist < minkowski_distance(0, val$threshold_value, 
                                     0, val$threshold_value, val$p),
           dist_x < (max_caliper_new/2),
           dist_y < (max_caliper_new/2)) %>%
    ungroup()})

# distance ----------------------------------------------------------------

combinations_softmaxed <- combinations %>%
  group_by(key_ref) %>%
  mutate(softmax_dist = softmax(-dist)) %>%
  ungroup() %>%
  arrange(key_ref)

# # check
# w_check$match_unsure_all <- combinations_softmaxed %>% filter(softmax_dist < 1)
# w_check$match_unsure_middle <- combinations_softmaxed %>%
#   filter(softmax_dist < 0.9, softmax_dist > 0.2)
# w_check$match_suspicious <- combinations_softmaxed %>%
#   filter(dist > 5, softmax_dist > 0.10)

# cutout ------------------------------------------------------------------

# pairs <- combinations_softmaxed %>%
#   ungroup() %>%
#   #arrange(dist) %>%
#   filter(softmax_dist == max(softmax_dist), .by = "key_new") %>%
#   filter(softmax_dist == max(softmax_dist), .by = "key_ref") %>%
#   arrange(key_new)

pairs2 <- combinations_softmaxed %>%
  group_by(key_new) %>%
  slice_max(softmax_dist, n = 1, with_ties = TRUE) %>%
  # group_by(key_ref) %>%
  # slice_max(softmax_dist, n = 1, with_ties = TRUE) %>%
  ungroup() %>%
  arrange(key_new)

# pairs analysis ----------------------------------------------------------

pairs_index <- pairs %>%
  select(key_new, key_ref)

# check duplicates
stopifnot(!any(duplicated(pairs_index$key_ref)))

#w_check$duplicates_key_new <- pairs_index$key_new[duplicated(pairs_index$key_new)]

##TODO Decide what to do with duplicates

pairs_unpaired <- list(
  ref = setdiff(unique(d_ref_fixed$key_ref), pairs_index$key_ref),
  new = setdiff(unique(d_new_fixed$key_new), pairs_index$key_new))

pairs_indexed <-
  pairs_index %>%
  bind_rows(tibble(key_ref = pairs_unpaired$ref, key_new = NA)) %>%
  bind_rows(tibble(key_new = pairs_unpaired$new, key_ref = NA)) %>%
  arrange(key_ref) %>%
  rowwise() %>%
  mutate(paired = all(!is.na(c(key_new, key_ref)))) %>%
  ungroup()

d_pairs <- pairs_indexed %>%
  left_join(d_new_fixed) %>%
  left_join(d_ref_fixed) %>%
  mutate(pair_index = ifelse(!is.na(key_new) & 
                               !is.na(key_ref), 1, 0)) %>%
  mutate(pair_index = ifelse(!is.na(key_new) & 
                               !is.na(key_ref), cumsum(pair_index), NA))

# cross validation --------------------------------------------------------

r_summary_raw <- pairs_indexed %>%
  summarise(n_unpaired_new = length(which(is.na(key_ref))),
            n_unpaired_ref = length(which(is.na(key_new))),
            #n_unpaired = length(which(paired == F)),
            n_doubled_ref = length(which(duplicated(key_ref) & !is.na(key_ref))),
            n_doubled_new = length(which(duplicated(key_new) & !is.na(key_new))),
            n_paired_without_doubled = length(
              which(!duplicated(key_new) & !is.na(key_new) & paired == T)),
            n_paired_with_doubled = length(which(paired == T)),
            n_ref = length(which(!is.na(key_ref))),
            n_new = length(unique(key_new)))

r_summary_prec <- r_summary_raw %>%
  mutate_all(~(.x/n_ref)*100)
