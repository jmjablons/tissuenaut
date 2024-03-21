
### PREPROCESS REF BLOBS FILES

# init --------------------------------------------------------------------

source("main_init.R") #dependencies #util #variables
source("blobs_analysis/blob_links.R") #link$files_manual etc 
source("blobs_analysis/name.R")

# dir tree ----------------------------------------------------------------

dir.create("blobs_analysis/data/")
dir.create("blobs_analysis/result/")
dir.create("blobs_analysis/plot/")

# read --------------------------------------------------------------------

d_blobs_raw <- link$files_manual %>% #set in 'blob_source_data.R'
  as.data.frame() %>%
  rename(filepath = '.') %>%
  rowwise() %>%
  do(., readr::read_delim(.$filepath, 
                          delim = "\t", 
                          escape_double = FALSE, 
                          show_col_types = FALSE, 
                          trim_ws = TRUE)) %>%
  ungroup()

# preprocess --------------------------------------------------------------

# names(d_blobs_raw) %<>%
#   stringr::str_replace_all("\\s", "_") %>% 
#   stringr::str_replace_all("\\:", "") %>%
#   stringr::str_replace_all("\\µ", "u")

d_blobs_raw = d_blobs_raw %>%
  rename_with(tolower) %>%
  mutate(image = gsub(x = image, 
                       pattern = "\\.ome.tif", 
                       replacement = "")) %>%
  rename(structure = name) %>%
  dplyr::rename_with( ~ stringr::str_replace_all(., "\\s+", "_")) %>%
  dplyr::rename_with( ~ stringr::str_replace_all(., "\\:", "")) %>%
  dplyr::rename_with( ~ stringr::str_replace_all(., "\\µ", "u")) %>%
  tidyr::separate_wider_delim(image,"_",names = c("image","slice")) %>%
  tidyr::separate_wider_regex(cols = slice,
                              c(position_rostral = "\\d", 
                                hemi = "[a-z]{1,}"),
                              too_few = "align_start")

# save --------------------------------------------------------------------

write.csv2(d_blobs_raw, file = paste(link$dir_path, "data/data_blobs_raw", sep = ""))

link$files_manual %>%
  write.table(paste(link$dir_path, "data/data_blobs_raw_info_source.txt", sep = ""))
