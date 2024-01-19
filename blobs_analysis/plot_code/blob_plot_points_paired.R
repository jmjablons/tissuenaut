
source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

# dependency --------------------------------------------------------------

require(ggplot2)
require(ggforce)

# read data ---------------------------------------------------------------

d_points <- util$read_csv(temp$d_points) %>%
  select(-any_of(c('...1')))

# helper ------------------------------------------------------------------

temp$numbers <- sample(x = c(unique(d_points$pair_index)), 
                 size = length(unique(d_points$pair_index)), 
                 replace = F)[1:length(unique(d_points$pair_index))]

# playground --------------------------------------------------------------

plot_pairs_colored <- 
  d_points %>%
  mutate(pair_index = factor(pair_index, levels= temp$numbers, ordered = T)) %>%
  ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
  geom_circle(aes(fill = pair_index), colour = NA, alpha = 0.4)+ 
  #coord_equal()+
  scale_y_reverse()+
  facet_wrap(~sample, scales = "free") +
  scale_fill_manual(values = rainbow(length(unique(d_points$pair_index)))) +
  #theme(legend.position = "none")+
  labs(caption = paste0(name$classif_short, collapse = "\n"))

ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_pairs_colored_", temp_classif_short,
                         ".pdf", collapse = ""), 
       plot = plot_pairs_colored,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 14,
       units = "in",
       dpi = 100)  

# 2colors -----------------------------------------------------------------

plot_match_2groups <- d_points %>%
  mutate(pair_index = factor(pair_index, levels= temp$numbers, ordered = T)) %>%
  ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
  geom_circle(aes(fill = group), colour = NA, alpha = 0.5)+ 
  #coord_equal()+
  scale_y_reverse()+
  facet_wrap(~sample, scales = "free") +
  theme(legend.position = "bottom")

ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_match_2groups_", temp_classif_short,
                         ".pdf", collapse = ""), 
       plot = plot_match_2groups,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 14,
       units = "in",
       dpi = 100)  

# only sharp --------------------------------------------------------------
# bw

plot_match_bw <- d_points %>%
  filter(class %!in% name$without_this_class) %>%
   ggplot(aes(x0 = x, y0 = y, r = r))+
   geom_point(aes(x = x, y = y, shape = group, size = r))+
   #coord_equal()+
   theme_bw()+
   scale_y_reverse()+
   scale_shape_manual(values = c(21,4) )+
   facet_wrap(~sample, scales = "free") +
   theme(legend.position = "bottom") 

ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_match_bw_", temp_classif_short,
                         ".pdf", collapse = ""), 
       plot = plot_match_bw,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 14,
       units = "in",
       dpi = 100)  

# only sharp --------------------------------------------------------------

plot_pairs_filtered <- 
  d_points %>%
  filter(class %!in% name$without_this_class) %>%
  mutate(pair_index = factor(pair_index, 
                             levels= temp$numbers, ordered = T)) %>%
  ggplot(aes(x0 = x, y0 = y, r = r))+
  geom_point(aes(x = x, y = y, 
                 shape = group, color = pair_index, size = r))+
  #coord_equal()+
  scale_y_reverse()+
  scale_shape_manual(values = c(21,4) )+
  facet_wrap(~sample, scales = "free") +
  scale_colour_manual(values = rainbow(length(temp$numbers))) #+
theme(legend.position = "none")

ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_pairs_filtered_", temp_classif_short,
                         ".pdf", collapse = ""), 
       plot = plot_pairs_filtered,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 14,
       units = "in",
       dpi = 100)  

# radius ------------------------------------------------------------------
# best:15x15in
 
plot_match_2groups_filtered <- d_points %>%
    filter(class %!in% name$without_this_class) %>%
   ggplot(aes(x0 = x, y0 = y, r = r, group = pair_index))+
   geom_circle(aes(fill = group), colour = NA, alpha = 0.5)+ 
   theme_bw()+
   scale_y_reverse()+
   facet_wrap(~sample, scales = "free") +
   scale_fill_manual(values = c("red","green"))+
   theme(legend.position = "bottom") 
 
ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_match_2groups_filtered_", temp_classif_short,
                         ".pdf", collapse = ""), 
       plot = plot_match_2groups_filtered,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 14,
       units = "in",
       dpi = 100)  