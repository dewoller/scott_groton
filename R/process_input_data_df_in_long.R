
process_input_data_df_in_long<- function(df_in)
{

  df_in %>%
    select(cohort, id, starts_with( prefix )) %>%
    pivot_longer( cols=starts_with( prefix ),
                 names_to=c('type','period') ,
                 names_pattern=paste0(prefix, "(...)_t(.)"),
                 values_to='value',
                 values_drop_na=TRUE ) %>%
    mutate( id = str_trim(id)) %>%
    mutate( period = as.numeric(period)) %>%
    mutate( cohort_period = cohort + period - 1 ) %>%
    mutate( cohort_age = cohort_period / 2 + 4.5 ) %>%
    filter( cohort != 1 ) %>%
    { . } -> df_in_long

  df_in_long

}


