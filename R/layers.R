
#' @title Layers in QP
#'
#' @description
#' This function classifies each worker into one of 4 layers.
#' Furthermore, it counts how many distinct layers exist by firm in each year.
#'
#' The function first further aggregates the qualif variable and then
#' produces the layers from it.
#'
#' This function follows the classification used in the Oppromola paper
#' However, the only difference is regarding qualif==53 | qualif==55.
#' Oppromola includes them as layer=0
#' I follow what Martim, Joana and Jaime did and include it as layer = 1.
#' This is done due to aggregation limitations, consistent with INE.
#'
#' In the folder consult_do you can find their stata file.
#'
#' @param data A tibble
#' @param var_qualif A variable with the qualif 1 digit variable. Not a string.
#' @param var_firm A variable with the firm unique ID. Not a string.
#' @param var_year A variable with the year. Not a string.
#'
#' @return The same dataset with 2 extra columns: layer (the hierarchical position of the worker) and n_layers (the amount of layers in a firm), both from 0 to 3. The first is at the worker level and the second at the firm level.
#'
#' @export
#'
#' @examples
#' # df %>%
#' #   layers(qualif,
#' #          firm,
#' #          year)
#'
#'
#'

layers <- function(data,
                   var_qualif,
                   var_firm,
                   var_year) {

  # classify the worker according to hierarchical layer:
  data %<>%
    dplyr::mutate(qualif_agreg = dplyr::case_when(
      {{ var_qualif }} == 1 ~ 3,
      {{ var_qualif }} == 2 | {{ var_qualif }} == 3 ~ 2,
      {{ var_qualif }} == 4 | {{ var_qualif }} == 5 ~ 1,
      {{ var_qualif }} == 6 | {{ var_qualif }} == 7 | {{ var_qualif }} == 8 ~ 0,
      TRUE ~ NA
    ))


  # Classify the layers given the existing qualif_agreg
  # So that the bottom layer of the firm is layer 0
  # Layer 1 is directly above, and so one
  # Thus, the only way a worker gets to be layer 3 is to be top layer
  # in a firm with 4 layers

  # create a list with a vector of unique
  vec_unique_qualif_agreg <- data %>%
    dplyr::group_by({{var_firm}},
                    {{var_year}}) %>%
    dplyr::mutate(unique_qualif_agreg = unique(qualif_agreg) %>% sort()) %>%
    dplyr::pull(unique_qualif_agreg)
  # sort automatically eliminates NAs

  # vector with the qualif agreg
  vec_qualif_agreg <- data %>% dplyr::pull(qualif_agreg)

  # match them
  vec_layer <- vec_qualif_agreg %>%
    purrr::map2(vec_unique_qualif_agreg,
                match) %>%
    unlist()

  # add id to the data
  data$layer <- vec_layer

  # how many layers in each firm in a given year
  data %<>%
    dplyr::group_by({{var_firm}},
                    {{var_year}}) %>%
    dplyr::mutate(n_layers = max(layer,
                                 na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-qualif_agreg)

  return(data)

}











