
set.seed(123)

# init --------------------------------------------------------------------

source("main_init.R")
source("points_analysis/name.R")
source("points_analysis/settings_batch_analysis_pairs.R") #temp_files_new
# temp_files_ref #name

# switch ------------------------------------------------------------------

temp_classif <- temp_files_new[[1]] %>% 
  stringr::str_extract("results_.{1,}\\/") %>%
  stringr::str_remove(c("results_manual_.{8}_")) %>%
  stringr::str_remove(c("\\/"))

name$classif_short = util$name_shorter_classif(temp_classif)

# output ------------------------------------------------------------------

path_out = paste0(getwd(),"/analysis_pairs_result/")
up_dirs = name$classif_short
down_dirs = c("result", "plots")

create_dir = function (up_dirs, down_dirs, pathway = path_out) {
  if(!dir.exists(path_out)){dir.create(path_out)}
  for (i in up_dirs){
    if(!dir.exists(paste0(pathway, i))){dir.create(paste0(pathway, i))}
    for (j in down_dirs){
      if(!dir.exists(paste0(pathway, i, "/", j))){
        dir.create(paste0(pathway, i, "/", j))}
    }}}

create_dir(up_dirs,down_dirs)

# loop --------------------------------------------------------------------
for(x in 1:4){
  d_new <- read_delim(temp_files_new[[x]],
                      delim = "\t", escape_double = FALSE,
                      locale = locale(), trim_ws = TRUE)
  d_ref <- read_delim(temp_files_ref[[x]],
                      delim = "\t", escape_double = FALSE,
                      col_types = cols(...4 = col_skip()),
                      trim_ws = TRUE)
  temp_version <- temp_files_ref[[x]] %>% 
    stringr::str_extract(".{29}$") %>%
    stringr::str_remove(c(".ome.tif-points.tsv")) %>%
    stringr::str_remove(c("\\/"))
  # read preprocess ---------------------------------------------------------
  source("points_analysis/read_preprocess.R")
  # run analysis ------------------------------------------------------------
  source("points_analysis/protagonist_real.R")
  r_summary_raw_fixed <- r_summary_raw %>%
    mutate(image = temp_version, classif = name$classif_short)
  writexl::write_xlsx(r_summary_raw_fixed, 
                      path = paste0(path_out, up_dirs,"/",down_dirs[1],"/",
                                    "summary_raw_", 
                                    temp_version, "_",
                                    name$classif_short,
                                    ".xlsx"))
  # run class matrix --------------------------------------------------------
  source("points_analysis/get_r_class_matrix.R")
  r_class_matrix_raw_fixed <- r_class_matrix_raw %>%
    mutate(image = temp_version, classif = name$classif_short)
  writexl::write_xlsx(r_class_matrix_raw_fixed, 
                      path = paste0(path_out, up_dirs,"/",down_dirs[1],"/",
                                    "r_class_matrix_", 
                                    temp_version, "_",
                                    name$classif_short,
                                    ".xlsx",
                                    collapse = ""))
  # plot pairs colored ------------------------------------------------------
  source("points_analysis/plot_code/plot_pairs_colored.R") #d_pairs
  p_pairs_colored$labels$caption <- 
    paste0(c(temp_version, name$classif_short), collapse = "\n")
  ggsave(filename = paste0(path_out, up_dirs,"/",down_dirs[2],"/",
                           "plot_pairs_colored_", 
                           temp_version, "_", name$classif_short,
                           ".pdf",
                           collapse = ""), 
         plot = p_pairs_colored,
         device = "pdf",
         scale = 1,
         width = 8,
         height = 8,
         units = "in",
         dpi = 100)  
  # plot colocalization class -----------------------------------------------
  source("points_analysis/plot_code/plot_colocalization_class.R")
  p_colocalization_class$labels$caption <- paste0(c(temp_version, name$classif_short), collapse = "\n")
  ggsave(filename = paste0(path_out, up_dirs,"/",down_dirs[2],"/",
                           "plot_colocalization_class_", 
                           temp_version, "_",
                           name$classif_short,
                           ".pdf",
                           collapse = ""), 
         plot = p_colocalization_class,
         device = "pdf",
         scale = 1,
         width = 8,
         height = 8,
         units = "in",
         dpi = 100) }
