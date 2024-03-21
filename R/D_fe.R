
#' @title Fitted Fixed effects from fixest
#'
#' @description
#' This function extracts the FE from a fixest object and then adds it to the data.
#' Thus, every observations gets their respective estimated FE value for each FE.
#' The function deals with interactions of FE.
#'
#' @param data A tibble.
#' @param m A fixest object (model).
#'
#' @return A tibble with the estiamted FEs.
#'
#' @export
#'
#' @examples
#' # df %<>% D_fe(m1)
#'
#'

D_fe <- function(data, m) {

  # retrieve the FE of the model
  list_fe <- fixest::fixef(m)

  # these are the names of the variables that lead to the FE
  # names(list_fe)
  for (i in names(list_fe)){

    # for the case of interations in the FE
    if(i %>% stringr::str_detect("\\^") ){

    # name to give
    nametobe <- paste("D_", i, sep = "")

    name1 <- i %>% stringr::str_extract(".*(?=\\^)")
    name2 <- i %>% stringr::str_extract("(?<=\\^).*")

    # for each naming vector
    # create a dataframe with the fe
    df_temp <- list_fe[[i]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{ name1 }} := names(list_fe[[i]]) %>% stringr::str_extract(".*(?=\\_)") %>% as.integer()) %>%
      dplyr::mutate({{ name2 }} := names(list_fe[[i]]) %>% stringr::str_extract("(?<=\\_).*") %>% as.integer()) %>%
      dplyr::rename({{ nametobe }} := value)

    data %<>%
      dplyr::left_join(
        df_temp,
        by = c(name1,
               name2) #by the fe
      )
    #normal FE
    } else{

      # name to give
      nametobe <- paste("D_", i, sep = "")

      # for each naming vector
      # create a dataframe with the fe
      df_temp <- list_fe[[i]] %>%
        tibble::as_tibble() %>%
        dplyr::mutate({{ i }} := names(list_fe[[i]]) %>% as.integer()) %>%
        dplyr::rename({{ nametobe }} := value)

      data %<>%
        dplyr::left_join(
          df_temp,
          by = i #by the fe
        )
      }

  }

  return(data)
}



