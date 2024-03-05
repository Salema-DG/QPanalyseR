
#' @title Keep track of deleted observations in data cleaning
#'
#' @description
#' This function allows the user to keep track of the deleted rows
#' while cleaning the data.
#'
#' @param data This is the dataset df_track, that keeps track of the number of obervations in the main dataset.
#' @param name This is a short description of the data restriction
#' @param n How many rows are in the main dataset. By default is nrow(df)
#'
#' @return A dataset with an extra line and how many rows df has.
#'
#' @export
#'
#' @examples
#'
#' #df_track <- tibble(
#' #              Stage = "Original Data",
#' #              Observations = nrow(df)
#' #              )
#' # df_track %>% add_track("Delete workers older than 65 years".)
#'
#'
add_track <- function(data,
                      name,
                      n = nrow(df)) {

  data %<>%
    dplyr::bind_rows(
      tibble::tibble(
        Stage = name,
        Observations = n
      )
    )

  return(data)

}


