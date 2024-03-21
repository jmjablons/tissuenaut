
temp_faithful <- faithful_couples %>%
  select(key_new, key_ref, class_ref, class_new) %>%
  mutate(pair_index = ifelse(!is.na(key_new) & 
                               !is.na(key_ref), 1, 0)) %>%
  mutate(pair_index = ifelse(!is.na(key_new) & 
                               !is.na(key_ref), cumsum(pair_index), NA))

girls_married_tagged <- temp_faithful %>%
  select(key = key_new, pair_index) %>%
  mutate(gender = "girl")

boys_married_tagged <- temp_faithful %>%
  select(key = key_ref, pair_index) %>%
  mutate(gender = "boy")

# single ------------------------------------------------------------------

girls_single_tagged <- 
  data.frame(key = girls_single,
            pair_index = 0) %>%
  mutate(gender = "girl") %>%
  as_tibble()

boys_single_tagged <- 
  data.frame(key = boys_single,
             pair_index = 0) %>%
  mutate(gender = "boy") %>%
  as_tibble()

# uncertain ---------------------------------------------------------------

girls_uncertain_tagged <- 
  data.frame(key = girls_uncertain,
             pair_index = -1) %>%
  mutate(gender = "girl") %>%
  as_tibble()

boys_uncertain_tagged <- 
  data.frame(key = boys_uncertain,
             pair_index = -1) %>%
  mutate(gender = "boy") %>%
  as_tibble()

# fill data ---------------------------------------------------------------

temp_girls <- bind_rows(
girls_married_tagged,
girls_single_tagged,
girls_uncertain_tagged) %>%
  left_join(d_new_fixed %>% 
              rename_with(~stringr::str_remove(., '_new')), 
            by = "key")

temp_boys <- bind_rows(
  boys_married_tagged,
  boys_single_tagged,
  boys_uncertain_tagged) %>%
  left_join(d_ref_fixed %>% 
              rename_with(~stringr::str_remove(., '_ref')), 
            by = "key")

# one last check ----------------------------------------------------------

temp_girls$key %>% sort() %>% duplicated() %>% which()
temp_boys$key %>% sort() %>% duplicated() %>% which()

stopifnot(!duplicated(temp_girls$key))
stopifnot(!duplicated(temp_boys$key))

# form data plot ----------------------------------------------------------

temp_all <- bind_rows(temp_girls, temp_boys)

temp_pair_indexes <- unique(temp_all$pair_index) %>% subset(. > 0)

temp_numbers = c(-1, 0 ,sample(x = c(temp_pair_indexes), 
                               size = length(temp_pair_indexes), 
                               replace = F)[1:length(temp_pair_indexes)])

temp_numbers_rainbowed <- temp_numbers %>% subset(. > 0)

temp_girls_colored <- temp_girls %>%
  mutate(pair_index = factor(pair_index, 
                             levels= temp_numbers, ordered = T))

temp_boys_colored <- temp_boys %>%
  mutate(pair_index = factor(pair_index, 
                             levels= temp_numbers, ordered = T))

# plot --------------------------------------------------------------------

p_pairs_colored <- temp_girls_colored %>%
  ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
  geom_circle(aes(fill = pair_index), colour = NA)+ 
  geom_point(aes(x = x, y = y, colour = pair_index),
             data = temp_boys_colored,
             pch = 15, alpha = .5, size = 1)+
  coord_equal()+
  theme_bw()+
  scale_y_reverse()+
  scale_fill_manual(values = c("gray", "gray", rainbow(length(temp_numbers_rainbowed)))) +
  scale_colour_manual(values = c("gray", "gray", rainbow(length(temp_numbers_rainbowed)))) +
  theme(legend.position = "none")
