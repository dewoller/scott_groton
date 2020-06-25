generate_all_models <- function( df_in_long ) {

  df_in_long %>%
    distinct( type ) %>%
    rename( target_type = type ) %>%
    by_row( generate_model, df_in_long ) %>%
    unnest( .out ) %>%
    { . } -> df_BIC


  df_BIC %>%
    mutate( power = paste( 'Power', power)) %>%
    spread( target_type, BIC)

}
