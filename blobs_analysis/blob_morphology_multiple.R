
set.seed(123)

source("main_init.R")
source("blobs_analysis/value.R")
source("blobs_analysis/blob_links.R")

# read data ---------------------------------------------------------------

d_morphology <- util$read_csv(link$d_morphology) %>%
  select(-any_of(c('...1'))) %>%
  tidyr::unite(name$sample_identif, sep = "_", col = "sample")

# ttest -------------------------------------------------------------------

temp_d <- d_morphology %>%
  #tidyr::pivot_wider(names_from = name, values_from = value) %>%
  filter(class %!in% name$without_this_class) %>%
  tidyr::pivot_longer(cols = name$morphology_measures) %>%
  filter(name %in% "area") %>%
  filter(classifier %in% c("sharp", "manual"))

fun_stat <- function(a){
    (temp_d %>%
      group_by(sample, classifier, class) %>%
      sample_n(temp_max_union) %>%
      ungroup() %>%
      rstatix::t_test(value~class))$p}

vrep <- Vectorize(fun_stat)
multi_stat <- vrep(1:10000)

multi_stat <- multi_stat_strong_10t
multi_stat[multi_stat < 0.05] %>% length()

multi_stat_sharp <- multi_stat
multi_stat_strong <- multi_stat

multi_stat_strong_10t <- multi_stat
multi_stat_sharp_10t <- multi_stat


# summary -----------------------------------------------------------------

temp_s <- d_morphology %>%
  #tidyr::pivot_wider(names_from = name, values_from = value) %>%
  filter(class %!in% name$without_this_class) %>%
  tidyr::pivot_longer(cols = name$morphology_measures) %>%
  filter(name %in% "area") %>%
  filter(classifier %in% c("strong"))

fun_summary <- function(a){
  temp_s %>%
    group_by(sample, classifier, class) %>%
    sample_n(temp_max_union) %>%
    ungroup() %>%
    group_by(class) %>%
    summarise(median = median(value), iqr = IQR(value), min = min(value), max = max(value))%>%
    mutate(lp = a)}

vsum <- Vectorize(fun_summary)
multi_sum <- vsum(1:1000) %>% t() %>% as_tibble() %>% tidyr::unnest()

multi_sum_sharp
multi_sum_painter 
multi_sum_strong

