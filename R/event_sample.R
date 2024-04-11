
#' @title Create a sample with observations that went through an event
#'
#' @description
#' A function to build a sample for the event study.
#' Consider the event is from t = -1 to t=0
#' Inspired my Cornelison et al. (2017, AER) event study.
#' Note: for now stability_cond = "both" is the only possible condition
#' The necessity of having the outcome varaible in the function comes from the
#' fact that we may have the same unit treted twice. In this way, we are sure that
#' they are counted the same way.
#'
#'
#' @param data A tibble
#' @param outcome_var Variable where we want to see the impact in
#' @param time_var Variable with the period indication.
#' @param event_var Variable representing the event shock
#' @param unit Unit of analysis. eg. individual, firm, country
#' @param change_for_event Minimum change in event_var necessary to classify as event.
#' @param event_cond To be classified as an event, does the change need to be "positive", "negative" or "both"?.
#' @param stable_n_before Number of periods before where stability of the event variable must apply
#' @param stable_n_after Number of periods after where stability of the event variable must apply
#' @param stability_cond Type of stability condition. Can be "event_var", "constant_setting" or "both".
#' @param stability_change The maximum change that the outcome varaible can have and be called "stable"
#' @param ... Stability variables. If "constant_setting" or "both" is selected stability_cond, choose which variables warrant such stability.
#'
#' @return If "both" is selected, a column indicating "negative" or positive is also added.
#'
#' @export
#'
#' @importFrom magrittr %<>%
#'
#' @author Miguel Salema
#'

event_sample <- function(data,
                         outcome_var,
                         time_var,
                         event_var,
                         unit,
                         change_for_event,
                         event_cond = "both",
                         stable_n_before,
                         stable_n_after,
                         stability_cond = "both",
                         ...
                         ) {

  # Only possible for now
  stopifnot(stability_cond == "both")


  # Determine the group of stability
  data %<>%
    dplyr::group_by(...) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  #########################################################################################
  # Step 1: Get a sample of workers that stayed in the same group_id for x years in a row #
  #########################################################################################

  # x years in the same group_id
  names_lag_group_id <- paste("group_id_m", 1:stable_n_before, sep="_")
  names_lead_group_id <- paste("group_id_p", 1:stable_n_after, sep="_")

  data %<>%
    fast_lag(col = group_id,
             group = {{unit}},
             lag_or_lead = "lag",
             vec_n = 1:stable_n_before,
             new_names = names_lag_group_id,
             order = {{time_var}})

  data %<>%
    fast_lag(col = group_id,
             group = {{unit}},
             lag_or_lead = "lead",
             vec_n = 1:stable_n_after,
             new_names = names_lead_group_id,
             order = {{time_var}})


  data %<>%
    dplyr::mutate(cont = case_when(
      if_all(tidyselect::all_of(c(names_lag_group_id, names_lead_group_id)), ~{.x == group_id}) ~ 1,
      TRUE ~ 0
    ))

  #-----------------------------------------#
  # keep only the ones that have continuity #
  #-----------------------------------------#

  # What is the year of the number y_forward lead
  data %<>%
    fast_lag(col = {{time_var}},
             group = {{unit}},
             lag_or_lead = "lead",
             vec_n = stable_n_after,
             new_names = "max_lead_year",
             order = {{time_var}})

  #the same for lag
  data %<>%
    fast_lag(col = {{time_var}},
             group = {{unit}},
             lag_or_lead = "lag",
             vec_n = stable_n_before,
             new_names = "min_lag_year",
             order = {{time_var}})

  #Remove if there is any discontinuity, either forward or backward
  #example, if the year is 2013, y_forward = 3, y_backward = 2, no discontinuity means:
  # that max_lead_year = 2016 and min_lag_year = 2011. Thus, 2016 - 2011 = 3 + 2
  data %<>%
    mutate(cont2 = case_when(
      (max_lead_year - min_lag_year) == (stable_n_after + stable_n_before) ~ 1,
      TRUE ~ 0
    ))

  ####################################
  # Step 2: Lag average peer quality #
  ####################################

  # lag and lead the av_peer column
  names_lag_event_var <- paste0("event_var_m", 1:stable_n_before)
  names_lead_event_var <- paste0("event_var_p", 1:stable_n_after)

  data %<>%
    fast_lag(col = {{event_var}},
             group = {{unit}},
             lag_or_lead = "lag",
             vec_n = 1:stable_n_before,
             new_names = names_lag_event_var,
             order = {{time_var}})

  data %<>%
    fast_lag(col = {{event_var}},
             group = {{unit}},
             lag_or_lead = "lead",
             vec_n = 1:stable_n_after,
             new_names = names_lead_event_var,
             order = {{time_var}})


  #########################################
  # Step 3: lag/lead the outcome variable #
  #########################################

  # lag and lead the av_peer column
  names_lag_outcome_var <- paste0("outcome_var_m", 1:stable_n_before)
  names_lead_outcome_var <- paste0("outcome_var_p", 1:stable_n_after)

  data %<>%
    fast_lag(col = {{outcome_var}},
             group = {{unit}},
             lag_or_lead = "lag",
             vec_n = 1:stable_n_before,
             new_names = names_lag_outcome_var,
             order = {{time_var}})

  data %<>%
    fast_lag(col = {{outcome_var}},
             group = {{unit}},
             lag_or_lead = "lead",
             vec_n = 1:stable_n_after,
             new_names = names_lead_outcome_var,
             order = {{time_var}})

  ##################
  # Step 4: FILTER #
  ##################

  # keep only the workers with continuity and indide same group
  data %<>%
    dplyr::filter(cont == 1 & cont2 == 1)

  ##############################
  # Step 5: Filter º2 #
  ##############################

  # The difference in the event_var must be always below stability_change
  # except from -1 to 0, which should be bigger than change_for_event

  # expression for the case_when
  filter_expression <-
    paste("abs(",
          c(names_lag_event_var[-1], names_lead_event_var[-1]),
          "-",
          c(names_lag_event_var[1], names_lead_event_var[1]),
          ") < stability_change",
          collapse = " & ") %>%
    glue::glue(" & abs(event_var_m1 - {event_var}) > change_for_event & abs(event_var_p1 - {event_var}) < stability_change")


  # Classify an event given the previous conditions
  data  %<>%
    dplyr::mutate(event = dplyr::case_when(
      eval(rlang::parse_expr( filter_expression ))
      ~ 1,
      TRUE ~ 0
    ))

  # Drop observations that don't fulfill the condition
  data %<>%
    filter(event == 1)

  ##############################
  # Step 6: Classify the Event #
  ##############################

  # Classify if it's a positive or a negative shock
  data  %<>%
    dplyr::mutate(quality = dplyr::case_when(
      eval(rlang::parse_expr(glue::glue("-(event_var_m1 - {event_var}) > change_for_event")))
      ~ "positive",
      TRUE ~ "negative"
    ))

  ##############################
  # Step 6: Turn the Data Tidy #
  ##############################


  # Keep only the columns necessary for the event study
  keep_col <-
    c(glue::glue("{unit}"),
      glue::glue("{time_var}"),
      glue::glue("{event_var}"),
      glue::glue("{outcome_var}"),
      "group_id",
      "quality",
      names_lag_av_peer,
      names_lead_av_peer,
      names_lag_res_wage,
      names_lead_res_wage
    )

  data %<>%
    dplyr::select(all_of(keep_col))


  # transform the lags into one obs per column
  # goal is to have a column t that is -yearsback to year forward

  #standardize av_peer
  data %<>%
    rename("{event_var}_m0" = {{event_var}},
           "{outcome_var}_m0" = {{outcome_var}})

  # create a dataframe with the info that does not vary in time, and time
  df_aux <- data %>%
    select(
      {{unit}},
      {{time_var}},
      group_id,
      quality)

  # add a unique event id, in case we have several treated units repeated
  df_aux$event_id <- 1:nrow(df_aux)


    # FIQUEI AQUI!!!!


  # I must also use the time_var, not only the worker to identify the evet
  # because there might be workers that have more than 1 event
  df_aux1 <- data %>%
    select(all_of(c("worker",
                    "year",
                    "av_peer_m0",
                    names_lag_av_peer)
    )) %>%
    pivot_longer(!c(worker, year), #coluns not to pivot
                 names_prefix = "av_peer_m",
                 names_to = "t", # these are the columns
                 values_to = "av_peer",
                 names_transform = list(t = as.numeric)
    ) %>%
    mutate(t = -t)

  df_aux2 <- data %>%
    select(all_of(c("worker",
                    "year",
                    names_lead_av_peer)
    )) %>%
    pivot_longer(!c(worker, year), #coluns not to pivot
                 names_prefix = "av_peer_p",
                 names_to = "t", # these are the columns
                 values_to = "av_peer",
                 names_transform = list(t = as.numeric)
    )


  df_aux3 <- data %>%
    select(all_of(c("worker",
                    "year",
                    "res_wage_m0",
                    names_lag_res_wage
    )))  %>%
    pivot_longer(!c(worker, year), #coluns not to pivot
                 names_prefix = "res_wage_m",
                 names_to = "t", # these are the columns
                 values_to = "res_wage",
                 names_transform = list(t = as.numeric)
    ) %>%
    mutate(t = -t)

  df_aux4 <- data %>%
    select(all_of(c("worker",
                    "year",
                    names_lead_res_wage
    )))  %>%
    pivot_longer(!c(worker, year), #coluns not to pivot
                 names_prefix = "res_wage_p",
                 names_to = "t", # these are the columns
                 values_to = "res_wage",
                 names_transform = list(t = as.numeric)
    )

  # join the data

  # bind 1 with 2 and 3 with 4 and then join them
  data <-
    bind_rows(df_aux1, df_aux2) %>%
    left_join(
      bind_rows(df_aux3, df_aux4),
      by = c("worker", "year", "t")
    ) %>%
    left_join(
      df_aux,
      by = c("worker", "year")
    )


  # correct the year
  # year is correct in t == 0
  data %<>%
    mutate(year = year + t)


  # arrange
  data %<>%
    arrange(worker,
            year)



  # return
  return(data)
}



