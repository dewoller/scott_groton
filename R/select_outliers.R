n_sd=3
select_outliers =  function( df_in_long_filtered, n_sd=3 ) {

  df_in_long_filtered %>%
    group_by( cohort_age, type ) %>%
    summarise(
              cohort_period_sd = sd( value),
              cohort_period_mean = mean( value),
              outlier_upper_limit = cohort_period_mean + (cohort_period_sd * n_sd),
              outlier_lower_limit = cohort_period_mean - (cohort_period_sd * n_sd),
              .groups='drop') %>%
    inner_join( df_in_long_filtered, by = c("cohort_age","type" )) %>%
    filter( value >outlier_upper_limit | value <outlier_lower_limit  ) %>%
    select( id, type, cohort_age)

}

