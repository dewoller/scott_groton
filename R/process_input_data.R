
process_input_data<- function(prefix, spss_orig, min_mandndi)
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

  #t1_names %>% clipr::write_clip()


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
    { . } -> df_in_long


df_in_long


}


