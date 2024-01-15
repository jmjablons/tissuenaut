
temp_plot <- list(
  t_new = d_new_fixed %>% 
    select(x = x_new, y = y_new, r = r_new, 
           class = class_new, source = source_new),
  t_ref = d_ref_fixed %>% 
    select(x = x_ref, y = y_ref, r = r_ref, 
           class = class_ref, source = source_ref))

temp_plot$temp_fill_colors <- c(sharp = "#F07A5C",
                                smooth = "#FFC289", 
                                strong_stain2 = "#553C66", 
                                weak_stain2 = "#B76EFA")

temp_plot$temp_outline_colors <- c(almost_visible = "gray",
                                   blurred = "#83916D",
                                   sharp_in_view = "black")

p_colocalization_class <- temp_plot$t_new %>%
  ggplot(aes(x0 = x, y0 = y, r = r, 
             group = source))+
  geom_point(aes(x = x, y = y, 
                 colour = class), 
             data = temp_plot$t_ref, 
             pch = 15, alpha = 0.5,
             size = 1)+
  geom_circle(aes(fill = class), 
              colour = NA)+ 
  coord_equal()+
  theme_bw()+
  scale_y_reverse()+
  scale_fill_manual(values = temp_plot$temp_fill_colors)+
  scale_colour_manual(values = temp_plot$temp_outline_colors)
