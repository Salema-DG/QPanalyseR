#' @title Largest connected set by two variables
#'
#' @description
#' This function computes the largest connected set for two variables.
#' In a connected set, all units are connected by atleast one factor in a
#' transitive manner. That is, I can find a path from one observation to any
#' other using only the 2 factors provided.
#'
#' @param data A tibble
#' @param fe1 1st variable
#' @param fe2 2nd variable
#'
#' @return A dataset with the largest connect set
#'
#' @export
#'
#' @examples
#' # Example: AKM
#' # df %>%
#' #   lcs(firm,
#' #       firm_lag)
#'
#'
#'
lcs <- function(data, fe1, fe2) {

  #setup
  temp <- data %>%
    dplyr::select({{ fe1 }},
                  {{ fe2 }}) %>%
    tidyr::drop_na({{ fe1 }},
                   {{ fe2 }})

  #this is the function that computes it from 2 factors
  cf = lfe::compfactor(
    list(f1 = temp %>% dplyr::pull({{ fe1 }}) %>% as.factor(),
         f2 = temp %>% dplyr::pull({{ fe2 }}) %>% as.factor()))
  # The outout sets are ordered. cf = 1 is the LCS, cf = 2 is the second largest, and so on

  # Keep only the largest
  df_cf <- tibble::tibble(cf,
                  {{ fe1 }} := temp %>% dplyr::pull({{ fe1 }})) %>%
    dplyr::filter(cf == 1)


  #change in the dataset
  by <- dplyr::join_by({{ fe1 }})

  data %<>%
    dplyr::semi_join(df_cf,
                     by)

  return(data)
}





