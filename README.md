# tissuenaut

Image analysis toolkit developed for the analysis of automatic neuron detections generated by QuPath (https://github.com/qupath/qupath) within images of DAB-immunohistochemical stained brain sections. The project leverages reference data of manually marked points on the same images to evaluate the performance of automated methods.

-   **Point Analysis (Co-localization):** Analyzes co-localization between automatic and manual cells within a subset of population images, providing cross-validation of accurately and wrongly paired cells.

-   **Blob Analysis (Morphological Comparison):** Compares automatic and manual outlines of cells based on morphological measures such as area, eccentricity, and circularity within regions of interest (ROI) in sample images. Plots or results are generated to assess differences in cell characteristics.

-   **Image Downsizing (Resizing):** Provides a feature to resize large image files into thumbnails or smaller dimensions for improved processing efficiency and visualization.

## Contents

-   Installation
-   Features
    -   [Pairwise Neuronal Co-localization Analysis](#pairwise-neuronal-colocalization-analysis)
    -   [Class Matrix Generation for Cross-Validation](#class-matrix-generation-for-cross-validation)
    -   [Blob Analysis for Morphological Comparison](#blob-analysis-for-morphological-comparison)
    -   [Image Downsizing and Saving](#image-downsizing-and-saving)

## **Installation**

Ready to go

``` r
# Clone the repository
git clone https://github.com/jmjablons/tissuenaut
cd yourproject

# Ensure R and necessary packages (e.g., dplyr, purrr, imager) are installed
# Install packages if needed
install.packages("dplyr")
install.packages("purrr")
install.packages("imager")
```

## Features

-   **Pairwise Neuronal Co-localization Analysis**: This function facilitates the analysis of neuronal co-localization within tissue images by comparing points based on their coordinates in a 2-dimensional space. It processes rows of two data frames (`d_new_fixed` and `d_ref_fixed`), calculating Minkowski distances and filtering based on specified thresholds to identify co-localized neuron pairs.

    ``` r
    combinations <- map_df(1:nrow(d_new_fixed), function(i) {
      d_new_fixed[i,] %>% 
        bind_cols(d_ref_fixed) %>%
        rowwise() %>%
        mutate(dist = minkowski_distance(
          x = .data[[new_x]], x1 = .data[[ref_x]],
          y = .data[[new_y]], y1 = .data[[ref_y]], p),
          dist_x = abs(.data[[new_x]] - .data[[ref_x]]),
          dist_y = abs(.data[[new_y]] - .data[[ref_y]])) %>%
        filter(dist < threshold_value,
               dist_x < (max_caliper_new/2),
               dist_y < (max_caliper_new/2)) %>%
        ungroup()
    }) %>% unique()
    ```

-   **Class Matrix Generation for Cross-Validation**: After identifying co-localized neuron pairs, the analysis extends to generating a class matrix (`r_class_matrix_raw`). This matrix summarizes pair classifications (`class_new`) within reference classes (`class_ref`), providing insights for cross-validation and automatic classifier evaluation.

    ``` r
    r_class_matrix_raw <- faithful_couples %>%
      bind_rows(temp_singles) %>%
      select(key_new, key_ref, class_ref, class_new) %>%
      mutate(pair_index = ifelse(!is.na(key_new) & !is.na(key_ref), 1, 0)) %>%
      mutate(pair_index = ifelse(!is.na(key_new) & !is.na(key_ref), cumsum(pair_index), NA)) %>%
      group_by(class_ref, class_new) %>%
      summarise(n = length(pair_index)) %>%
      ungroup() %>%
      tidyr::pivot_wider(values_from = n, names_from = class_new)
    ```

-   **Blob Analysis for Morphological Comparison**: The blob analysis compares morphological measures of automatically annotated cells with manually outlined ones within specified regions of interest (ROI) across images. It assesses differences in cell characteristics (e.g., size, shape) between automated and manual analyses, providing insights into the accuracy and effectiveness of automated cell annotation methods.

-   **Image Downsizing and Saving**: Resize and save a batch of images based on specified dimensions (`m_dream_height`, `max_caliper_new`) to facilitate downstream analysis and visualization. Based on `r imager` package (<https://github.com/asgr/imager>). Here: github.com/jmjablons/tissuenaut/blob/scaledown_images/batch_scaledown_images.R#L1
