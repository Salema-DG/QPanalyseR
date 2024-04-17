
#' @title Ready event data to Plot
#'
#' @description
#' The function turns data that comes from an event study or DiD
#' into a ready to plot dataset of the analysis.
#' Runs a linear model with FE.
#' The plot shows confidence intervals.
#' The data can have been matched.
#'
#' @param data A tibble
#' @param time_var Variable with the period indication. Treatment occurs from -1 to 0. Not a string. Will be used as a variable in the model.
#' @param unit Unit of analysis. eg. individual, firm, country. Not a string. Will be used as a FE
#' @param outcome_vars Character vector with outcome variables.
#' @param treatment It's the treatment variable.
#'
#' @export
#'
#' @return A tibble with ready to plot data. The columns are:
#'
#'
#' @examples
#' # example code
#' # df_plot_event <- df_event %>%
#' #   plot_event_study(outcome_vars = c("res_wage", "av_peer"),
#' #                    time_var = t,
#' #                    unit = worker,
#' #                    treatment = quality)
#'


# a DiD within a worker (worker FE)

plot_event_study <- function(data,
                             outcome_vars,
                             time_var,
                             unit,
                             treatment){

  # Set the base level as -1
  data %<>%
    dplyr::mutate("{{time_var}}" := {{time_var}} %>%
                    as.factor() %>%
                    forcats::fct_relevel("-1"))

  formula <- paste0(" ~ ", deparse(substitute(time_var)), " | ", deparse(substitute(unit)))


  # run the regressions
  # run the regressions
  m_event <- data %>%
    dplyr::arrange({{treatment}}) %>%
    split(data %>% dplyr::pull({{treatment}})) %>% # split according to quality
    purrr::map(~{
      c(outcome_vars) %>%
        paste(formula) %>%
        purrr::map(as.formula) %>%
        purrr::map(fixest::feols, data = .x)
    })

  # to plot the CI and the coeffs, use broom:tidy to transform to a dataframe
  df_plot <- m_event %>%
    purrr::map(purrr::map,
        broom::tidy,
        conf.int = TRUE)

  df_plot %<>%
    purrr::map(dplyr::bind_rows)

  df_plot %<>%
    purrr::reduce(dplyr::bind_rows)

  # -1 is always the reference level
  # Thus, on the coefficients it's always
  time_frame <- data %>%
    dplyr::pull({{time_var}}) %>%
    unique() %>%
    sort()

  time_frame <- time_frame[time_frame != "-1"]

  treats <- data %>%
    dplyr::pull({{treatment}}) %>%
    unique() %>%
    forcats::fct_inorder()

  df_plot %<>%
    dplyr::mutate({{time_var}} := rep(time_frame, length(outcome_vars) * length(treats)),
                  {{treatment}} := rep(treats, each = length(outcome_vars) * length(time_frame)),
                  dp_var = rep(outcome_vars, each = length(time_frame)) %>% rep(times = length(treats)) )


  # add the intercept
  df_plot <- df_plot %>%
    dplyr::filter(t != 1000) %>%
    dplyr::select(-term) %>%
    dplyr::bind_rows(
      tibble::tibble({{treatment}} := rep(treats, each = length(outcome_vars)),
                     dp_var = rep(outcome_vars, times = length(treats)),
                     t = rep("-1", length(time_frame) + 1 ))
    )

  # fill the rest in the -1
  df_plot[is.na(df_plot)] <- 0

  return(df_plot)

}


globalVariables(c("term"))


