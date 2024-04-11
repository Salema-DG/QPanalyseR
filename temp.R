# example temp

library(devtools)

load_all()

data(qp_0.1perc_sample)

qp_0.1perc_sample %>%
  event_sample(outcome_var = base_wage, # will be residualized wages
               time_var = year,
               event_var = wage_lh_real, #will be av_peer
               unit = worker,
               change_for_event = 0.5,
               event_cond = "both",
               stable_n_before = 2,
               stable_n_after = 1,
               stability_cond = "both",
               stability_change = 0.1,
               occup,
               sector) %>% view()
