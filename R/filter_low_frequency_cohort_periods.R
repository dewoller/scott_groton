filter_low_frequency =  function( df_in_long ) {

  df_in_long %>%
    filter( type == 'mps') %>%   # just pick any type, cause if there is less than 3 of this type, there is less than 3 records
    group_by( cohort,period ) %>%
    filter( n() <3 )  %>%
    distinct( cohort, period) %>%
    ungroup() %>%
    { . } -> df_tofilter

  df_in_long  %>%
    anti_join( df_tofilter) %>%
    { . } -> df_in_long_filtered

  df_in_long_filtered

}
