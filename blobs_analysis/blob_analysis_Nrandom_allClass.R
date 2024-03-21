
set.seed(123)

source("main_init.R")
source("blobs_analysis/value.R")
source("blobs_analysis/blob_links.R")

require(writexl)
require(ggsignif)

# read data ---------------------------------------------------------------

d_morphology <- util$read_csv(link$d_morphology) %>%
  select(-any_of(c('...1'))) %>%
  tidyr::unite(name$sample_identif, sep = "_", col = "sample")

# random cells ------------------------------------------------------------

temp_max_union <- (d_morphology %>%
                     #tidyr::unite(name$sample_identif, sep = "_", col = "sample") %>%
                     #tidyr::pivot_wider(names_from = name, values_from = value) %>%
                     #filter(class %!in% name$without_this_class) %>%
                     group_by(sample, classifier, class) %>%
                     summarise(how_many = n(), .groups = "drop") %>%
                     arrange(how_many))$how_many[1]

d_morphology_nrandom <- d_morphology %>%
  #tidyr::pivot_wider(names_from = name, values_from = value) %>%
  #filter(class %!in% name$without_this_class) %>%
  group_by(sample, classifier) %>%
  sample_n(temp_max_union) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = name$morphology_measures)

# plot --------------------------------------------------------------------

# d_morphology_nrandom %>%
#   ggplot(aes(x = classifier, y=value, fill = classifier)) +
#   geom_boxplot(outlier.color = NA) + 
#   geom_quasirandom(pch = 21) +
#   facet_grid(name~sample, switch = "y", scales = "free_y")+
#   theme(legend.position = "none", 
#         axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
#         strip.text = element_text(angle = 90))

d_morphology_nrandom %>%
  ggplot(aes(x = classifier, y=value)) +
  geom_boxplot(aes(colour = classifier), outlier.colour = NA)+
  geom_beeswarm(aes(fill = class),pch = 21, cex = 2)+
  facet_wrap(~name, scales = "free_y", nrow = 1)+
  theme(#legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    strip.text = element_text(angle = 0)) +
  scale_colour_manual(values = c("gray","#E7A339","#4AAFD5"))+
  scale_fill_manual(values = c("gray","#E7A339","black","#4AAFD5","black")) +
  labs(caption = paste0(
    c(temp_max_union, unique(d_morphology_nrandom$classifier)), 
    collapse = " | "))

# temp_plot$final <- 
#   ((temp_plot[[1]] + temp_plot[[2]] * theme(axis.title = element_blank(), 
#                                      legend.position = "none")) + 
#   plot_layout(widths = c(14,4)))

ggsave(filename = paste0("blobs_analysis/plot/",
                         "plot_morphology_Nrandom_", temp_classif_short,
                         ".pdf", collapse = ""), 
       plot = temp_plot$final,
       device = "pdf",
       scale = 1,
       width = 14,
       height = 8,
       units = "in",
       dpi = 100)  

# stat --------------------------------------------------------------------

# stat_morphology_Nrandom <- list(
#   t_test_samples = d_morphology_nrandom %>%
#     group_by(name, sample) %>%
#     rstatix::t_test(value~group),
#   wilcox_samples = d_morphology_nrandom %>%
#     group_by(name, sample) %>%
#     rstatix::wilcox_test(value~group),
#   wilcox_total = d_morphology_nrandom %>%
#     group_by(name) %>%
#     rstatix::wilcox_test(value~group),
#   ttest_total = d_morphology_nrandom %>%
#     group_by(name) %>%
#     rstatix::t_test(value~group))

stat <- list(
  ttest = d_morphology_nrandom %>%
    #filter(class %in% c("strong_stain2","painter")) %>%
    group_by(name) %>%
    rstatix::t_test(value~classifier),
  wilcox = d_morphology_nrandom %>%
    #filter(class %in% c("strong_stain2","painter")) %>%
    group_by(name) %>%
    rstatix::wilcox_test(value~classifier))

writexl::write_xlsx(
  stat, 
  paste0("blobs_analysis/result/stat_morphology_6n_random_allClasses.xlsx"))
