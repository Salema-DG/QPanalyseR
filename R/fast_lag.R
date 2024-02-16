
#' @title Fast lagged/lead values
#' 
#' @description
#' This function creates lagged values faster than dplyr::lag, using data.table.
#' The function handles multiple lags/leads at once, of the same column. 
#' vec_n and new_names must have the same size.
#' 
#' @param data A tibble.
#' @param col Columns to find the lag/lead value. Not a string.
#' @param group Group to find lags/leads within. Only one group. Not a string.
#' @param lag_or_lead String. Either "lag" or "lead".
#' @param vec_n A numeric vector indicating by how many observations should the lag/lead jump.
#' @param new_names A character vector with the names of the new variables with the lag/leads.
#' @param order Variable to order the dataset by. Not a string. Default is year. 
#' 
#' @return A tibble with the lagged/leadded columns.
#' 
#' @importFrom data.table :=
#' 
#' @export
#' 
#' @examples
#' # the new names
#' #names <- paste("lead_wage", 1:5, sep="_")
#' 
#' # run the fast lag/lead function
#' #df %<>% fast_lag(col = wage_lh_real, 
#' #                 group = worker, 
#' #                 lag_or_lead = "lead",
#' #                 vec_n = 1:5,
#' #                 new_names = names)
#' 
#' 
#' 
fast_lag <- function(data, 
                     col, 
                     group, 
                     lag_or_lead,
                     vec_n,
                     new_names, 
                     order = year) {
  
  #turn into data.table object
  dt <- data %>% 
    dplyr::arrange({{order}}, {{group}}) %>% 
    dplyr::select({{group}}, {{col}}) %>%  
    data.table::as.data.table()
  
  # the lags
  dt[, (new_names) :=  data.table::shift(.SD, 
                                         n = vec_n, 
                                         type=lag_or_lead), 
     by = c(names(dt)[1]), 
     .SDcols = c(names(dt)[2]) ]

  
  # back to tibble and return
  return(  data %>% 
             dplyr::arrange({{order}}, {{group}}) %>% 
             dplyr::bind_cols(dt %>% tibble::as_tibble() %>% dplyr::select(-{{group}}, -{{col}})))
}








