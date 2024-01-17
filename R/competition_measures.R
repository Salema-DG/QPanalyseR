
#' @title Worker level competition in QP
#'
#' @description
#' This function produces measures of competition using worker level information.
#' The first measure is "Out of Sample Hires", from Manchin "Monopsony", 1st chapter.
#' This measures the proportion of workers hired from outside of the sample.
#' A low proportion of poaches is an indicator of low competition.
#' The second measure is the Herfindahl Hirschman index (HHI) of employment.
#' Here, the market share is relative to share of employment and wages,
#' instead of the typical sales variable.
#'
#' @param data A tibble
#' @param hiring_var Name of the variable indicating if the worker was hired. Not a string.
#' @param poach_var Name of the variable indicating id the worker was poached. Not a string.
#' @param firm_var The name of the firm identifier, used for HHI. Not a string.
#' @param wage_var The name of the variable with total wages. Not a strinf.
#' @param ... The grouping to compute competitiveness for. Should contain "year".
#'
#' @export
#'
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#'
#' @examples
#' # load the simple dataset
#' data(qp_0.1perc_sample)
#'
#' qp_0.1perc_sample |>
#'   competition_measures(hiring,
#'                        poach,
#'                        firm,
#'                        total_wage,
#'                        year,
#'                        sector)
#'
competition_measures <- function(data,
                                 hiring_var,
                                 poach_var,
                                 firm_var,
                                 wage_var,
                                 ...) {

  # bind names for global variables
  size <- wage_costs <- share_size <- share_wages <- NULL

  # to use hiring_var in filter do:
  # set the condition as a string using englue
  string <- rlang::englue("{{hiring_var}} == 1")
  # parse that
  parsed <- rlang::parse_expr(string)
  # inside the filter, evaluate the expression to get true/false

  # 1st: out of sample hires (OSH)
  df_prop_out_of_sample <- data %>%
    dplyr::filter(eval(parsed)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(prop_out_of_sample = 1 - sum({{poach_var}}, na.rm = T)/dplyr::n())

  # 2nd: Herfindahl–Hirschman index:
  # HHI = sum(share^2)
  df_hhi <- data %>%
    dplyr::group_by(..., {{firm_var}}) %>%
    dplyr::summarize(size = dplyr::n(),
              wage_costs = sum({{wage_var}})) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(share_size = size/sum(size),
           share_wages = wage_costs/sum(wage_costs)) %>%
    dplyr::summarise(hhi_size = sum(share_size^2),
              hhi_wages = sum(share_wages^2),
              n_firms = dplyr::n()) %>%
    dplyr::ungroup()

  # 3rd: normalized HHI (from 0 to 1)
  df_hhi <- df_hhi %>%
    dplyr::mutate(normalized_hhi_size = dplyr::case_when(
      n_firms == 1 ~ 1,
      n_firms != 1 ~ ((hhi_size - (1/n_firms))/(1 - (1/n_firms)))
    )) %>%
    dplyr::mutate(normalized_hhi_wages = dplyr::case_when(
      n_firms == 1 ~ 1,
      n_firms != 1 ~ ((hhi_wages - (1/n_firms))/(1 - (1/n_firms)))
    ))


  # join everything
  by <- dplyr::join_by(...)

  df <- df_prop_out_of_sample %>%
    dplyr::left_join(df_hhi,
                     by = by)

  return(df)

}





