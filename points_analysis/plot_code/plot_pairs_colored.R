
temp_plot <- list(
  t_temp = d_pairs %>%
    select(key_new, key_ref, pair_index) %>%
    filter(!is.na(key_ref)) %>%
    filter(!duplicated(key_new)) %>%
    filter(!is.na(key_new)))

temp_plot$t_new = d_new_fixed %>% 
    select(x = x_new, y = y_new, r = r_new, 
           class = class_new, source = source_new, key = key_new) %>%
    full_join(temp_plot$t_temp %>% select(key = key_new, pair_index))

temp_plot$t_ref = d_ref_fixed %>% 
    select(x = x_ref, y = y_ref, r = r_ref, 
           class = class_ref, source = source_ref, key = key_ref) %>%
    full_join(temp_plot$t_temp %>% select(key = key_ref, pair_index))

temp_plot$t_numbers = sample(x = c(unique(temp_plot$t_new$pair_index),NA), 
                      size = length(unique(temp_plot$t_new$pair_index)), 
                      replace = F)[1:length(unique(temp_plot$t_new$pair_index))]

p_pairs_colored <- temp_plot$t_new %>%
  mutate(pair_index = factor(pair_index, 
                             levels= temp_plot$t_numbers, ordered = T))%>%
  ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
  geom_circle(aes(fill = pair_index), colour = NA)+ 
  geom_point(aes(x = x, y = y, colour = pair_index),
             data = (temp_plot$t_ref %>% 
                       mutate(pair_index = 
                                factor(pair_index, 
                                       levels= temp_plot$t_numbers, ordered = T))),
             pch = 15, alpha = .5, size = 1)+
  coord_equal()+
  theme_bw()+
  scale_y_reverse()+
  scale_fill_manual(values = rainbow(length(unique(temp_plot$t_new$pair_index)))) +
  scale_colour_manual(values = rainbow(length(unique(temp_plot$t_new$pair_index)))) +
  theme(legend.position = "none")
