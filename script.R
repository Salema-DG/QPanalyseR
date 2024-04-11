


# load the data with model estimates:
# simple_data_year_response_within.rds

# load the coeffficents of eta:
# df_year_hetero_coeffs

# Calculate the wage residualized
# I could add every variable but the peer effect, or I can do the residuals + peer part:

# calculate the residualized wages

data %<>% mutate(res_wage = residuals + quality_peer_fitted)

