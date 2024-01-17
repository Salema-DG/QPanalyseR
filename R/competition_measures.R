
#' @title Worker level competition in QP
#'
#' @description
#' This function produces measures of competition using worker level information.
#' The first measure is "Out of Sample Hires", from Manchin "Monopsony", 1st chapter.
#' This measures the proportion of workers hired from outside of the sample.
#' A low proportion of poaches is an indicator of low competition.
#' The second measure is the Herfindahl Hirschman index (HHI) of employment.
#' Here, the market share is relative to share of employment,
#' instead of the typical sales variable.
#'
#' @param data A tibble
#' @param hiring_var Name of the variable indicating if the worker was hired. Not a string.
#' @param poach_var Name of the variable indicating id the worker was poached. Not a string.
#' @param firm_var The name of the firm identifier, used for HHI. Not a string.
#'
#' @param ... The grouping to compute competitiveness for. Should contain "year".
#'
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#'
#' # to load this data, you must have the peerest package loaded
#' data(simul_data)
#'
#' qp_sample |>
#'   competition_measures(hiring_var = hiring,
#'                        poach_var = poach,
#'                        firm_var = firm,
#'                        wage_var = total_wage,
#'                        year,
#'                        sector
#'
competition_measures <- function(data,
                                 hiring_var,
                                 poach_var,
                                 firm_var,
                                 wage_var,
                                 ...) {

  # 1st: out of sample hires (OSH)
  df_prop_out_of_sample <- data %>%
    filter(glue::glue("{{hiring_var}} == 1")) %>%
    group_by(...) %>%
    summarise(prop_out_of_sample = 1 - sum({{poach_var}}, na.rm = T)/n())

  # 2nd: Herfindahl–Hirschman index:
  # HHI = sum(share^2)
  df_hhi <- data %>%
    group_by({{firm_var}}) %>%
    summarize(size = n(),
              wage_costs = sum({{wage_var}})) %>%
    ungroup() %>%
    group_by(...) %>%
    mutate(share_size = size/sum(size),
           share_wages = wage_costs/sum(wage_costs)) %>%
    summarise(hhi_size = sum(share_size^2),
              hhi_wages = sum(share_wages^2),
              n_firms = n()) %>%
    ungroup()

  # 3rd: normalized HHI (from 0 to 1)
  df_hhi <- df_hhi %>%
    mutate(normalized_hhi_size = case_when(
      n_firms == 1 ~ 1,
      n_firms != 1 ~ ((hhi_size - (1/n_firms))/(1 - (1/n_firms)))
    )) %>%
    mutate(normalized_hhi_wages = case_when(
      n_firms == 1 ~ 1,
      n_firms != 1 ~ ((hhi_wages - (1/n_firms))/(1 - (1/n_firms)))
    ))

  # join everything
  by <- dplyr::join_by(...)

  df <- df_prop_out_of_sample %>%
    left_join(df_hhi)

  return(df)

}






