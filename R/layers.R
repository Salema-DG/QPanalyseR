
#' @title Layers in QP
#'
#' @description
#' This function classifies each worker into one of 4 layers,
#' using the variable "qualif" (professional category),
#' according to the following table:
#'
#' | Layer | Qualif | Classification |
#' |--|--|--|
#' | 4 | 1 | Top executives (top management) |
#' | 3 | 2, 3 | Middle management, supervisors, and team leaders |
#' | 2 | 4, 5 | Higher-skilled professionals |
#' | 1 | 6, 7, 8, 9 | Semi- and non-skilled professionals |
#'
#' However, the translation from qualif to layer is not immediate.
#' For the top layer to exist, all other layers must also exist in a firm.
#'
#' Furthermore, the function also
#' counts how many distinct layers exist by firm in each year,
#' from 1 to 4.
#'
#' I follow the classification of Caliendo et al. (2020),
#' but I do it from 1 to 4, instead of 0 to 3.
#' Similarly, I refer to layers and not to layers of management.
#' However, the only difference is regarding qualif 53 and qualif 55.
#' Caliendo et al. (2020) includes them as layer=0.
#' I follow what Martim, Joana and Jaime did and include it as layer = 1.
#' This is done due to aggregation limitations, consistent with INE.
#'
#'
#' @param data A tibble
#' @param var_qualif A variable with the qualif 1 digit variable. Not a string.
#' @param var_firm A variable with the firm unique ID. Not a string.
#' @param var_year A variable with the year. Not a string.
#'
#' @return The same dataset with 2 extra columns: layer (the hierarchical position of the worker) and n_layers (the amount of layers in a firm), both from 1 to 4. The first is at the worker level and the second at the firm level.
#'
#' @references Caliendo, L., Mion, G., Opromolla, L. D., & Rossi-Hansberg, E. (2020). Productivity and organization in Portuguese firms. Journal of Political Economy, 128(11), 4211-4257.
#'
#' @export
#'
#' @examples
#'
#' data("qp_0.1perc_sample")
#'
#' # df <- qp_0.1perc_sample %>%
#' #   layers(qualif_1d,
#' #          firm,
#' #          year)
#'
#'
#'

layers <- function(data,
                   var_qualif,
                   var_firm,
                   var_year) {

  # Agregate the qualif variable
  data %<>%
    dplyr::mutate(qualif_agreg = dplyr::case_when(
      {{ var_qualif }} == 1 ~ 4,
      {{ var_qualif }} == 2 | {{ var_qualif }} == 3 ~ 3,
      {{ var_qualif }} == 4 | {{ var_qualif }} == 5 ~ 2,
      {{ var_qualif }} == 6 | {{ var_qualif }} == 7 | {{ var_qualif }} == 8 ~ 1,
      {{ var_qualif }} == 9 ~ 1, # aprendices also bottom layer
      TRUE ~ NA
    ))

  # set the group
  data %<>%
    dplyr::group_by({{var_firm}},
                    {{var_year}}) %>%
    dplyr::mutate(id = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  # arrange the data
  data %<>%
    dplyr::arrange(id)

  # count how many times each group appers
  nid <- peerest::fast_vec_count(data$id)

  # Classify the layers given the existing qualif_agreg
  # So that the bottom layer of the firm is layer 1
  # Layer 1 is directly above, and so one
  # Thus, the only way a worker gets to be layer 4 is to be top layer
  # in a firm with 4 layers

  # create a list with a vector of unique qualif_agreg, streched at worker level
  list_unique_qualif_agreg <- data$qualif_agreg %>%
    base::split(data$id) %>% #split by the group, in a list
    purrr::map(unique) %>% # the unique value for each vector of that list
    purrr::map(~{ # sort the vector, unless it's only made of NAs
      if (.x %>% is.na() %>% all()) {
        NA
      } else {
        sort(.x)
      }
    }) %>%
    rep(nid)

  # match them (add it to the data)
  data$layer <- data$qualif_agreg %>%
    purrr::map2(list_unique_qualif_agreg,
                match) %>%
    unlist()


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











