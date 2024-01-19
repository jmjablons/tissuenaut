
source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

# dependency --------------------------------------------------------------

require(ggbeeswarm)
require(patchwork)
require(rstatix)

# read data ---------------------------------------------------------------

d_morphology_4measures <- util$read_csv(temp$d_morphology_4measures) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# plot line ---------------------------------------------------------------

d_morphology_4measures %>%
  mutate(pair_index = ifelse(pair_index == 0, NA, pair_index),
         pair_index = factor(pair_index)) %>%
  filter(class %!in% "smooth") %>% 
  arrange(sample, name, pair_index) %>%
  ggplot(aes(x = group, y=value, group = pair_index, fill = group)) +
  geom_line(data = . %>% filter(!is.na(pair_index))) +
  geom_point(pch = 21, size = 0.5)+
  facet_grid(name~sample, switch = "y", scales = "free_y")+
  theme(legend.position = "none")

# plot differences --------------------------------------------------------

temp <- d_morphology_4measures %>%
  filter(pair_index != 0) %>%
  group_by(sample, name, pair_index) %>%
  arrange(class, .by_group = TRUE) %>%
  summarise(div = (function(x){x[1]-x[2]})(value)) %>%
  ungroup()

temp %>%
  ggplot(aes(x = "", y=div, fill = "gray")) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_quasirandom(width = 0.3,
                   pch = 21) +
  facet_grid(name~sample, switch = "y", scales = "free_y")+
  theme(legend.position = "none", strip.text = element_text(angle = 90))

# stat paired -------------------------------------------------------------

d_morphology_4measures %>%
  filter(pair_index != 0) %>%
  mutate(pair_index = factor(pair_index)) %>%
  #filter(class %!in% "smooth") %>%
  group_by(name, sample) %>%
  rstatix::t_test(value~group, paired = T) %>%
  filter(p < 0.05)

# check differences -------------------------------------------------------

temp %>%
  #filter(class %!in% "smooth") %>%
  group_by(sample, name, pair_index) %>%
  summarise(n = n()) %>%
  group_by(sample, name, n) %>%
  summarise(n_l = n()) %>%
  ungroup()
