
set.seed(123)

source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

#require(writexl)

# read data ---------------------------------------------------------------

d_morphology_4measures <- util$read_csv(temp$d_morphology_4measures) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# random cells ------------------------------------------------------------

temp_max_union <- (d_morphology_4measures %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  filter(class %!in% name$without_this_class) %>%
  group_by(sample, group) %>%
  summarise(how_many = n(), .groups = "drop") %>%
  arrange(how_many))$how_many[1]

d_morphology_nrandom <- d_morphology_4measures %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  filter(class %!in% name$without_this_class) %>%
  group_by(sample, group) %>%
  sample_n(temp_max_union) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = 6:9)

# plot --------------------------------------------------------------------

temp_plot <- list()
temp_plot[[1]] <- d_morphology_nrandom %>%
  ggplot(aes(x = group, y=value, group = group, fill = group)) +
  geom_boxplot(outlier.color = NA) + 
  geom_quasirandom(pch = 21) +
  facet_grid(name~sample, switch = "y", scales = "free_y")+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        strip.text = element_text(angle = 90))

temp_plot[[2]] <- d_morphology_nrandom %>%
  ggplot(aes(x = group, y=value, group = group, fill = group)) +
  geom_quasirandom(
    pch = 21) +
  facet_grid(name~"total", scales = "free_y")+
  theme(strip.text = element_text(angle = 90))+
  labs(caption = paste0(
    c(temp_classif_short, temp_max_union, unique(d_morphology_nrandom$class)), 
    collapse = " | "))

temp_plot$final <- 
  ((temp_plot[[1]] + temp_plot[[2]] * theme(axis.title = element_blank(), 
                                     legend.position = "none")) + 
  plot_layout(widths = c(14,4)))

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

stat_morphology_Nrandom <- list(
  t_test_samples = d_morphology_nrandom %>%
    group_by(name, sample) %>%
    rstatix::t_test(value~group),
  wilcox_samples = d_morphology_nrandom %>%
    group_by(name, sample) %>%
    rstatix::wilcox_test(value~group),
  wilcox_total = d_morphology_nrandom %>%
    group_by(name) %>%
    rstatix::wilcox_test(value~group),
  ttest_total = d_morphology_nrandom %>%
    group_by(name) %>%
    rstatix::t_test(value~group))

writexl::write_xlsx(
  stat_morphology_Nrandom, 
  paste0("blobs_analysis/result/stat_morphology_Nrandom_", 
         temp_classif_short,".xlsx"))
