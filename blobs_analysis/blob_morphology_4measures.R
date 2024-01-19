
source("main_init.R")
source("blobs_analysis/name.R")
source("blobs_analysis/blob_source_data.R")

# values ------------------------------------------------------------------

temp$select_them <- list(
  new = c("new_class","id_new",
          "new_nucleus_max_caliper", 
          "new_nucleus_min_caliper",
          "new_nucleus_area",
          "new_nucleus_circularity"),
  ref = c("ref_class","id_ref",
          "ref_max_diameter_um", 
          "ref_min_diameter_um",
          "ref_area_um^2",
          "ref_circularity"))

temp$measures <- list(
  new = c("new_nucleus_max_caliper", 
          "new_nucleus_min_caliper",
          "new_nucleus_area",
          "new_nucleus_circularity"),
  ref = c("ref_max_diameter_um", 
          "ref_min_diameter_um",
          "ref_area_um^2",
          "ref_circularity"))

# read data ---------------------------------------------------------------

d_morphology <- util$read_csv(temp$d_morphology) %>%
  select(-any_of(c('...1'))) #may silence the name repair #todo

# prepare -----------------------------------------------------------------

d_morphology_4measures <- map_df(c("new", "ref"), function(p) {
  return(
    d_morphology %>%
      select(name$sample_identif, pair_index, 
             all_of(temp$select_them[[p]])) %>%
      filter(!is.na(across(contains("id")))) %>%
      tidyr::pivot_longer(cols = temp$measures[[p]]) %>%
      rename_with(
        ~ stringr::str_replace_all(
          ., pattern = paste("\\_{0,}", p, "\\_{0,}", sep = ""),
          replacement = "")) %>%
      mutate(group = paste(p)) %>%
      mutate(name = stringr::str_replace_all(
        name,
        pattern = paste("\\_{0,}", p, "\\_{0,}", sep = ""),
        replacement = "")) %>%
      mutate(name = stringr::str_replace_all(
        name,
        pattern = paste("\\_{0,}", "nucleus", "\\_{0,}", sep = ""),
        replacement = "")))})

d_morphology_4measures = d_morphology_4measures %>%
  tidyr::unite(name$sample_identif, sep = "_", col = "sample") %>%
  mutate(pair_index = ifelse(is.na(pair_index), 0, pair_index)) %>%
  mutate(name = ifelse(name %in% c("max_caliper", "max_diameter_um"), 
                       "max_diameter", name)) %>%
  mutate(name = ifelse(name %in% c("min_caliper", "min_diameter_um"), 
                       "min_diameter", name)) %>%
  mutate(name = ifelse(name %in% c("area", "area_um^2"), "area", name))

# save --------------------------------------------------------------------

write.csv2(d_morphology_4measures, 
           file = paste("blobs_analysis/",
                        "result/data_morphology_4measures", "_", 
                        temp_classif_short,"_", 
                        util$today(), 
                        sep = ""))
