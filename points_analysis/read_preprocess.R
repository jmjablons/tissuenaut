
d_ref_fixed <- d_ref %>%
  as_tibble() %>%
  mutate(r_ref = sqrt((100/pi))) %>%
  mutate(x = x * val$pixel_width,
         y = y * val$pixel_height) %>%
  filter(class %!in% name$omit_those) %>% 
  mutate(source_ref = name$manual,
         key_ref = row_number()) %>%
  rename(x_ref = x, 
         y_ref = y,
         class_ref = class)

d_new_fixed <- d_new %>%
  select(x_new = `Centroid X µm`,
         y_new = `Centroid Y µm`,
         max_caliper_new = `Nucleus: Max caliper`,
         class_new = Class,
         area_new = `Nucleus: Area`) %>%
  mutate(r_new = sqrt((area_new/pi))) %>%
  filter(class_new %!in% name$omit_those) %>%
  mutate(source_new = name$classif_short,
         key_new = row_number())
