
name <- list(
  sample_identif = c("image","position_rostral","hemi"),
  roi_class = "property",
  marking_class = "marking",
  x_new = "x_new",
  x_ref = "x_ref",
  y_new = "y_new",
  y_ref = "y_ref",
  cells_x_coord = "centroid_x_µm",
  cells_y_coord = "centroid_y_µm",
  the_important_measure = "nucleus_max_caliper",
  data_id = "object_id",
  blobs_x_coord = "centroid_x_um",
  blobs_y_coord = "centroid_y_um",
  ref_class = "manual",
  area_cells = "new_nucleus_area",
  area_blolbs = "ref_area_um^2",
  group_to_compare = c("new","ref"),
  without_this_class = "smooth")

name_roi <- list()
name_roi$roi_position_columns <- c(x0 = "startx_px", 
                                   y0 = "starty_px", 
                                   dx = "dx_px", 
                                   dy = "dy_px")
name_roi$roi_position_um_columns <- sprintf("%s_um",name_roi$roi_position_columns)
names(name_roi$roi_position_um_columns) = names(name_roi$roi_position_columns)
# name_roi$roi_position_um_columns <- stringr::str_replace_all(
#   name_roi$roi_position_columns, pattern = "$","_um")
