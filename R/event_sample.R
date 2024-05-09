
#' @title Create a sample with observations that went through an event
#'
#' @description
#' A function to build a sample for the event study.
#' Consider the event is from t = -1 to t=0
#' Inspired my Cornelison et al. (2017, AER) event study.
#' Note: for now, stability_cond = "both" is the only possible condition
#' The user can join the data with more outcome variables after the sample is created.
#'
#'
#' @param data A tibble
#' @param outcome_var Not a string. Name of the outcome variable.
#' @param time_var Variable with the period indication. Not a string.
#' @param event_var Variable representing the event shock. Not a string.
#' @param unit Unit of analysis. eg. individual, firm, country. Not a string.
#' @param change_for_event Minimum change in event_var necessary to classify as event.
#' @param event_cond To be classified as an event, does the change need to be "positive", "negative" or "both"?.
#' @param stable_n_before Number of periods before where stability of the event variable must apply
#' @param stable_n_after Number of periods after where stability of the event variable must apply
#' @param stability_cond Type of stability condition. Can be "event_var", "constant_setting" or "both".
#' @param stability_change The maximum change that the outcome variable can have and be called "stable"
#' @param did Logical value. Default is FALSE. If true, a control group is returned.
#' @param ... Stability variables. If "constant_setting" or "both" is selected stability_cond, choose which variables warrant such stability.
#'
#' @return Returns a tidy dataset with observations that classify as an event. If "both" is selected, a column indicating "negative" or positive is also added.
#'
#' @export
#'
#' @importFrom magrittr %<>%
#' @importFrom magrittr %<>%
#'
#' @author Miguel Salema
#'
#' @examples
#' # example code
#'
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
                         stability_change,
                         did = FALSE,
                         ...
                         ) {

  # Only possible for now
  stopifnot(stability_cond == "both")
  stopifnot(event_cond == "both")


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
    dplyr::mutate(cont = dplyr::case_when(
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
    dplyr::mutate(cont2 = dplyr::case_when(
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


  #####################
  # Step 5: Filter º2 #
  #####################

  # 5.1: Stability: The first differences (FD) of the event_var must
    # be below stability_change,
    # except from -1 to 0


  # Build filter 5.1

  event_var_string <- deparse(substitute(event_var))


  # expression for the case_when
  filter_stability <-
    paste("abs(",
          c(names_lag_event_var[length(names_lag_event_var)], names_lead_event_var[length(names_lead_event_var)]),
          "-",
          c(names_lag_event_var[1], names_lead_event_var[1]),
          ") < stability_change",
          collapse = " & ") %>%
    stringr::str_glue(" & abs(event_var_p1 - {event_var_string}) < stability_change")

  # Apply 5.1: both control and treatment need the stability condition
  data  %<>%
    dplyr::mutate(stability = dplyr::case_when(
      eval(rlang::parse_expr( filter_stability )) ~ 1,
      TRUE ~ 0
    ))

  data %<>%
    dplyr::filter(stability == 1)


  # 5.2: Between t = -1 and t = 0 the FD of event_var must be bigger
  # then change_for_event for treated ind. and
  # smaller then stability_change for the control group.

  # Build filter 5.2
  filter_treat <- stringr::str_glue("abs(event_var_m1 - {event_var_string}) > change_for_event ")

  if (did == T) {
    filter_control <- stringr::str_glue("abs(event_var_m1 - {event_var_string}) < stability_change ")
  }

  # Classify an event (or control) given the previous conditions
  if (did == F) {

    data  %<>%
      dplyr::mutate(type = dplyr::case_when(
        eval(rlang::parse_expr( filter_treat )) ~ "treat",
        TRUE ~ "none"
      ))

    data %<>%
      dplyr::filter(type == "treat")

  } else{

    data  %<>%
      dplyr::mutate(type = dplyr::case_when(
        eval(rlang::parse_expr( filter_treat )) ~ "treat",
        eval(rlang::parse_expr( filter_control )) ~ "control",
        TRUE ~ "none"
      ))

    data %<>%
      dplyr::filter(type %in% c("treat", "control"))
  }


  ##############################
  # Step 6: Classify the Event #
  ##############################

  if (did == F) {

    # Classify if it's a positive or a negative shock
    data  %<>%
      dplyr::mutate(type = dplyr::case_when(
        eval(rlang::parse_expr(glue::glue("-(event_var_m1 - {event_var_string}) > change_for_event"))) ~ "positive",
        TRUE ~ "negative"
      ))

  } else{

    # Classify if it's a positive or a negative shock
    data  %<>%
      dplyr::mutate(type = dplyr::case_when(
        type == "control" ~ "control",
        eval(rlang::parse_expr(glue::glue("-(event_var_m1 - {event_var_string}) > change_for_event"))) ~ "positive",
        TRUE ~ "negative"
      ))

  }


  ##############################
  # Step 6: Turn the Data Tidy #
  ##############################

  # Keep only the columns necessary for the event study
  keep_col <-
    c(deparse(substitute(unit)),
      deparse(substitute(time_var)),
      deparse(substitute(event_var)),
      deparse(substitute(outcome_var)),
      "group_id",
      "type",
      names_lag_event_var,
      names_lead_event_var,
      names_lag_outcome_var,
      names_lead_outcome_var
    )

  data %<>%
    dplyr::select(tidyselect::all_of(keep_col))

  # add a unique event id, in case we have several treated units repeated
  data$event_id <- 1:nrow(data)

  # transform the lags into one obs per column
  # goal is to have a column t that is -yearsback to year forward

  #standardize av_peer
  data %<>%
    dplyr::rename("event_var_m0" = {{event_var}},
                  "outcome_var_m0" = {{outcome_var}})

  # create a dataset with the info that does not vary in time plus time itself
  df_aux <- data %>%
    dplyr::select(
      {{unit}},
      {{time_var}},
      group_id,
      type,
      event_id)


  # I must also use the time_var, not only the worker to identify the event
  # because there might be workers that have more than 1 event

  # Now, I must do 4 pivots:
  #   - outcome variable forward
  #   - outcome variable backward
  #   - event variable forward
  #   - event variable backward

  # event variable backward
  df_aux_event_variable_backward  <- data %>%
    dplyr::select(tidyselect::all_of(
      c("event_id",
        "event_var_m0",
        names_lag_event_var)
    )) %>%
    tidyr::pivot_longer(!c(event_id), #columns not to pivot
                        names_prefix = glue::glue("event_var_m"),
                        names_to = "t", # these are the columns
                        values_to = "event_var",
                        names_transform = list(t = as.numeric)
    ) %>%
    dplyr::mutate(t = -t)

  # event variable forward
  df_aux_event_variable_forward  <- data %>%
    dplyr::select(tidyselect::all_of(
      c("event_id",
        names_lead_event_var
    )
    )) %>%
    tidyr::pivot_longer(!c(event_id), #coluns not to pivot
                 names_prefix = glue::glue("event_var_p"),
                 names_to = "t", # these are the columns
                 values_to = "event_var",
                 names_transform = list(t = as.numeric)
    )

  # outcome variable backward
  df_aux_outcome_variable_backward  <- data %>%
    dplyr::select(tidyselect::all_of(c("event_id",
                                       "outcome_var_m0",
                    names_lag_outcome_var
    )
    )) %>%
    tidyr::pivot_longer(!c(event_id), #columns not to pivot
                 names_prefix = glue::glue("outcome_var_m"),
                 names_to = "t", # these are the columns
                 values_to = "outcome_var",
                 names_transform = list(t = as.numeric)
    ) %>%
    dplyr::mutate(t = -t)

  # outcome variable forward
  df_aux_outcome_variable_forward  <- data %>%
    dplyr::select(tidyselect::all_of(c("event_id",
                    names_lead_outcome_var
    )
    )) %>%
    tidyr::pivot_longer(!c(event_id), #coluns not to pivot
                 names_prefix = glue::glue("outcome_var_p"),
                 names_to = "t", # these are the columns
                 values_to = "outcome_var",
                 names_transform = list(t = as.numeric)
    )


  # Bind the datasets
  df_aux_event_variable <-
    dplyr::bind_rows(
      df_aux_event_variable_backward,
      df_aux_event_variable_forward)

  df_aux_outcome_variable <-
    dplyr::bind_rows(
      df_aux_outcome_variable_backward,
      df_aux_outcome_variable_forward)


  # Join event data with outcome data and with df_aux
  data_final <- df_aux_event_variable %>%
    dplyr::left_join(df_aux_outcome_variable,
              by = c("event_id", "t")) %>%
    dplyr::left_join(
      df_aux,
      by = c("event_id")
    )

  # the year refers to t == 0
  # thus, we must correct the year variable
  data_final %<>%
    dplyr::mutate({{time_var}} := {{time_var}} + t)

  # arrange
  data_final %<>%
    dplyr::arrange({{unit}},
            event_id,
            {{time_var}})

  # rename to the original names
  data_final %<>%
    dplyr::mutate("{{event_var}}" := event_var,
                  "{{outcome_var}}" := outcome_var) %>%
    dplyr::select(-event_var,
                  -outcome_var)

  # return
  return(data_final)
}

globalVariables(c("cont",
                  "cont2",
                  "event",
                  "event_id",
                  "group_id",
                  "type",
                  "stability"))

