
require(ggbeeswarm)

# filtered ----------------------------------------------------------------

temp <- d_morphology %>%
  filter(class %!in% name$without_this_class) %>%
  tidyr::pivot_longer(cols = name$morphology_measures)

temp %>%
  ggplot(aes(x = classifier, y=value , fill = class)) +
  geom_boxplot(outlier.colour = NA)+
  geom_beeswarm(pch = 21, cex = 3)+
  facet_grid(name~sample, 
             switch = "y", 
             scales = "free_y")+
  theme(#legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    #strip.text = element_text(angle = 90)
  ) +
  scale_colour_manual(values = c("gray","#E7A339","#4AAFD5"))+
  scale_fill_manual(values = c("gray","#E7A339","#4AAFD5"))

temp %>%
  group_by(name, sample) %>%
  rstatix::kruskal_test(value~class) %>%
  filter(p < 0.05)

temp %>%
  group_by(name, sample) %>%
  rstatix::dunn_test(value~class) %>%
  filter(p < 0.05)

# all classes -------------------------------------------------------------

d_morphology %>%
  tidyr::pivot_longer(cols = name$morphology_measures) %>%
  filter(sample %in% "1001443_4_NA",
         classifier %in% c("manual", "strong","sharp")) %>%
  ggplot(aes(x = classifier, y=value)) +
  geom_boxplot(aes(colour = classifier), outlier.colour = NA)+
  geom_beeswarm(aes(fill = class),pch = 21, cex = 3)+
  facet_wrap(name~sample, 
             #switch = "y", 
             nrow = 1,
             scales = "free")+
  theme(#legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    #strip.text = element_text(angle = 90)
  ) +
  scale_colour_manual(values = c("gray","#E7A339","#4AAFD5"))+
  scale_fill_manual(values = c("gray","#E7A339","black","#4AAFD5","black"))

# d_morphology %>%
#   tidyr::pivot_longer(cols = name$morphology_measures) %>%
#   ggplot(aes(x = classifier, y=value , fill = class)) +
#   geom_hline(yintercept = 0, linetype = "dotted")+
#   geom_quasirandom(width = 0.3,
#                    pch = 21) +
#   facet_grid(name~sample, switch = "y", scales = "free_y")+
#   theme(#legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     strip.text = element_text(angle = 90))
# 
# d_morphology %>%
#   tidyr::pivot_longer(cols = name$morphology_measures) %>%
#   ggplot(aes(x = classifier, y=value , fill = class)) +
#   geom_hline(yintercept = 0, linetype = "dotted")+
#   geom_boxplot()+
#   #geom_quasirandom(width = 0.3, pch = 21) +
#   facet_grid(name~sample, switch = "y", scales = "free_y")+
#   theme(#legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     strip.text = element_text(angle = 90))

# sample ------------------------------------------------------------------

temp %>%
  filter(sample %in% "1001443_4_NA",
         classifier %in% c("manual", "sharp")) %>%
  group_by(name, sample) %>%
  rstatix::t_test(value~class)

temp %>%
  filter(sample %in% "1001443_4_NA",
         classifier %in% c("manual", "strong")) %>%
  group_by(name, sample) %>%
  rstatix::t_test(value~class)

dat_text <- temp %>%
  group_by(name, classifier, class) %>%
  summarise(y = max(value), .groups = "drop") %>%
  mutate(y = y + 0.1*y) %>%
  rename(x = classifier) %>%
  mutate(label = "ns") %>%
  filter(x %!in% "manual")

dat_text$label[dat_text$name == "circularity" & dat_text$x == "sharp"] = "*"
dat_text$label[dat_text$name == "mincaliper" & dat_text$x == "sharp"] = "*"
dat_text$label[dat_text$name == "circularity" & dat_text$x == "strong"] = "*"

temp %>%
  filter(sample %in% "1001443_4_NA") %>%
  ggplot(aes(x = classifier, y=value , fill = class)) +
  geom_boxplot(outlier.colour = NA)+
  geom_beeswarm(pch = 21, cex = 3)+
  #geom_quasirandom(width = 0.45, pch = 21) +
  facet_wrap(name~sample, 
             #switch = "y", 
             nrow = 1,
             scales = "free")+
  theme(#legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    #strip.text = element_text(angle = 90)
  ) +
  scale_colour_manual(values = c("gray","#E7A339","#4AAFD5"))+
  scale_fill_manual(values = c("gray","#E7A339","#4AAFD5"))+
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label))
