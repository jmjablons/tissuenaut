require(readxl)
require(ggplot2)
require(ggbeeswarm)

temp <- read_excel("analysis_pairs_result3/cells_summary.xlsx")

temp %>%
  tidyr::pivot_longer(cols = -one_of(c("image","classif"))) %>%
  filter(!stringr::str_detect(name, "uncertain")) %>%
  rename(percent = value) %>%
  ggplot(aes(x = classif, y = percent)) + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  geom_boxplot(alpha = 0.7, fill = "darkgrey", outlier.colour = NA)+
  geom_beeswarm(pch = 21, size = 2, fill = 'darkgrey') +
  facet_wrap(~name, nrow = 1) +
  ggdark::dark_theme_gray()

ggdark::invert_geom_defaults()
