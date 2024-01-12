
source("main_init.R")
source("blobs_analysis/name.R")

# dependency --------------------------------------------------------------

require(ggplot2)
require(ggforce)

# read data ---------------------------------------------------------------

# requires d_points from 'blob_get_points_paired.R'

# helper ------------------------------------------------------------------

temp$numbers <- sample(x = c(unique(d_points$pair_index)), 
                 size = length(unique(d_points$pair_index)), 
                 replace = F)[1:length(unique(d_points$pair_index))]

# playground --------------------------------------------------------------

d_points %>%
  mutate(pair_index = factor(pair_index, levels= temp$numbers, ordered = T)) %>%
  ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
  geom_circle(aes(fill = pair_index), colour = NA, alpha = 0.5)+ 
  #coord_equal()+
  scale_y_reverse()+
  facet_wrap(~sample, scales = "free") +
  scale_fill_manual(values = rainbow(length(unique(d_points$pair_index)))) +
  theme(legend.position = "none")

# 2colors -----------------------------------------------------------------

d_points %>%
  mutate(pair_index = factor(pair_index, levels= temp$numbers, ordered = T)) %>%
  ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
  geom_circle(aes(fill = group), colour = NA, alpha = 0.5)+ 
  #coord_equal()+
  scale_y_reverse()+
  facet_wrap(~sample, scales = "free") +
  theme(legend.position = "bottom")

# only sharp --------------------------------------------------------------
# bw

d_points %>%
  filter(class %!in% name$without_this_class) %>%
   ggplot(aes(x0 = x, y0 = y, r = r))+
   geom_point(aes(x = x, y = y, shape = group, size = r))+
   #coord_equal()+
   theme_bw()+
   scale_y_reverse()+
   scale_shape_manual(values = c(21,4) )+
   facet_wrap(~sample, scales = "free") +
   theme(legend.position = "bottom") 

# only sharp --------------------------------------------------------------

d_points %>%
  filter(class %!in% name$without_this_class) %>%
  mutate(pair_index = factor(pair_index, 
                             levels= temp$numbers, ordered = T)) %>%
  ggplot(aes(x0 = x, y0 = y, r = r))+
  geom_point(aes(x = x, y = y, 
                 shape = group, color = pair_index, size = r))+
  coord_equal()+
  scale_y_reverse()+
  scale_shape_manual(values = c(21,4) )+
  facet_wrap(~sample, scales = "free") +
  scale_colour_manual(values = rainbow(length(temp$numbers))) #+
  theme(legend.position = "none")

# radius ------------------------------------------------------------------
# best:15x15in
 
d_points %>%
    filter(class %!in% name$without_this_class) %>%
   ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
   geom_circle(aes(fill = group), colour = NA, alpha = 0.5)+ 
   theme_bw()+
   scale_y_reverse()+
   facet_wrap(~sample, scales = "free") +
   scale_fill_manual(values = c("red","green"))+
   theme(legend.position = "bottom") 
 