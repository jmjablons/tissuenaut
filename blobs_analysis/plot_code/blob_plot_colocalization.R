
source("main_init.R")
source("blobs_analysis/value.R")
source("blobs_analysis/blob_links.R")

# dependency --------------------------------------------------------------

require(ggplot2)
require(ggforce)

# read data ---------------------------------------------------------------

d_morphology <- util$read_csv(link$d_morphology) %>%
  select(-any_of(c('...1'))) %>%
  tidyr::unite(name$sample_identif, sep = "_", col = "sample")

# plot --------------------------------------------------------------------

plot <- 
  d_morphology %>%
  filter(class %!in% name$without_this_class) %>%
  ggplot(aes(x0 = centroid_x_um , y0 = centroid_y_um))+
  geom_point(aes(x = centroid_x_um, y = centroid_y_um, 
                 shape = class, colour = class, size = area))+
  #coord_equal()+
  scale_y_reverse()+
  scale_shape_manual(values = c(4,0,1))+
  facet_wrap(~sample, scales = "free") +
  theme_no_axes()+
  scale_colour_manual(values = c("black","#E7A339","#4AAFD5")) #"#91B187"


# plot all classes --------------------------------------------------------

plot <- 
  d_morphology %>%
  #filter(class %!in% name$without_this_class) %>%
  ggplot(aes(x0 = centroid_x_um , y0 = centroid_y_um))+
  geom_point(aes(x = centroid_x_um, y = centroid_y_um, 
                 shape = class, colour = class, size = area))+
  scale_y_reverse()+
  scale_shape_manual(values = c(4,0,0,1,1))+
  facet_wrap(~sample, scales = "free") +
  theme_no_axes()+
  scale_colour_manual(values = c("black","#E14B0A","#F49E79","#00E0C6","#95F0E2")) #"#91B187"

# export ------------------------------------------------------------------

ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_colocalization_class",
                         ".pdf", collapse = ""), 
       plot = plot,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 14,
       units = "in",
       dpi = 100)  
