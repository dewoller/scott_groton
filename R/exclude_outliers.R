exclude_outliers =  function( df_in_long_filtered, df_outliers ) {

  df_in_long_filtered %>%
    anti_join( df_outliers, by=c('cohort_age','id','type') ) 

}
