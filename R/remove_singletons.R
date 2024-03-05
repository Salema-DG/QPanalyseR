
#' @title Remove Singletons in chosen columns
#'
#' @description
#' This function removes singletins in chosen columns.
#' If more than one column is chosen, it's normal that,
#' by deleting the singletons in one column, we create singletons in
#' another columns.
#' I have a document that indicates that this is not problematic.
#' The only reason to delete singletons is for computational efficiency.
#' The estimated coeffficients are not biased and only
#' in very specific cases is inference harmed.
#' Thus, the function, by dafault, iterates only one time.
#' However, the user may choose to do it more times.
#'
#' @param data A tibble
#' @param ... Columns to delete singletons from
#' @param iter_max How many iterations to delete singletons. If convergence is achioeved, this is ingnored.
#'
#' @return A dataset with no singletons in the last column chosen.
#'
#' @export
#'

remove_singletons <- function(data, ..., iter_max = 1) {

  # keep the names of selected columns in a vector
  names <- data %>% dplyr::select(...) %>% colnames()


  #put the data ready for the data.table backbend
  data %<>% dtplyr::lazy_dt()

  # save how many rows there are in the data
  #n_new <- data$parent %>% nrow() - because it's in lazy_dt, I cannot do this.
  n_new <- data %>%
    dplyr::summarise(n_new = n()) %>%
    tibble::as_tibble() %>%
    dplyr::pull(n_new)

  n_old <- 0 # for the while

  i_n <- 0

  while (n_new != n_old & iter_max != i_n) {

    n_old <- n_new
    i_n <- i_n + 1
    for (i in names) {
      data %<>%
        dplyr::count(.data[[i]]) %>% #If you have a character vector of variable names, and want to operate on them with a for loop, index into the special .data pronoun. See programming with https://github.com/tidyverse/dplyr/blob/master/vignettes/programming.Rmd
        dplyr::filter(n == 1) %>%
        dplyr::select(-n) %>%
        dplyr::anti_join(data, ., by = i)
    }

    n_new <- data %>%
      dplyr::summarise(n_new = n()) %>%
      dplyr::as_tibble() %>%
      dplyr::pull(n_new)
  }

  data %<>%
    tibble::as_tibble()

  return(data)
}
