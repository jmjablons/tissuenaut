
source("points_analysis/name.R")

# total -------------------------------------------------------------------

all_girls <- d_new_fixed$key_new %>% unique()
all_boys <- d_ref_fixed$key_ref %>% unique()

# out ---------------------------------------------------------------------

#combinations <- list()

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
    ungroup()}) %>% unique()

# combinations$key_ref %>% sort() %>% duplicated() %>% which() %>% length()
# combinations$key_new %>% sort() %>% duplicated() %>% which() %>% length()

cat("/n TROUBLESHOOT /n")

# combinations %>%
#   group_by(val_1 = pmin(key_new, key_ref), val_2 = pmax(key_new, key_ref)) %>%
#   summarise(n = n(), .groups = "drop") %>% 
#   filter(n > 1)

potential_playgirls <- combinations %>%
  group_by(key_new) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1) %>%
  select(key_new) %>% unlist()

potential_playboys <- combinations %>%
  group_by(key_ref) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1) %>%
  select(key_ref) %>% unlist()

faithful_couples <- combinations %>%
  filter(key_ref %!in% potential_playboys) %>%
  filter(key_new %!in% potential_playgirls)

# faithful_couples$key_new %>% duplicated() %>% which()
# faithful_couples$key_ref %>% duplicated() %>% which()

girls_faithful <- faithful_couples$key_new %>% unique()
boys_faithful <- faithful_couples$key_ref %>% unique()

girls_involved <- combinations$key_new %>% unique()
boys_involved <- combinations$key_ref %>% unique()

# definitely single -------------------------------------------------------

girls_single <- setdiff(all_girls, girls_involved)
boys_single <- setdiff(all_boys, boys_involved)

# uncertain ---------------------------------------------------------------

girls_uncertain <- setdiff(girls_involved, girls_faithful)
boys_uncertain <- setdiff(boys_involved, boys_faithful)

# analysis ----------------------------------------------------------------

dedust <- function(v){length(unique(v))}

r_summary <- list(
  pairs = faithful_couples %>% unique() %>% nrow(),
  all_new = dedust(all_girls),
  all_ref = dedust(all_boys),
  new_uncertain = dedust(girls_uncertain),
  ref_uncertain = dedust(boys_uncertain),
  new_single = dedust(girls_single),
  ref_single = dedust(boys_single)) %>% as_tibble()
