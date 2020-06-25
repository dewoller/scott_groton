
process_input_data_df_in<- function(prefix, spss_orig, min_mandndi)
{

  spss_orig %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    filter( !is.na(participant_no)) %>%
    filter( mandndi > min_mandndi) %>%
    { . } -> df_in

  df_in %>%
    select(ends_with('t2')) %>%
    rename_all( str_replace, '_t2','') %>%
    names() %>%
    { . } -> t1_names

  t1_names %>%
    str_replace('$', '_t1') %>%
    { . } -> t1_names_new


  df_in %>%
    rename_all( str_replace, '_t1','') %>%
    rename_at( vars( all_of( t1_names)), ~t1_names_new) %>%
    { . } -> df_in

  df_in %>%
    mutate( cohort = age_months_to_cohort( age_corrected ))

}


