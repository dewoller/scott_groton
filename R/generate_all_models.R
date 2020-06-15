generate_all_models <- function( df_in ) {

  df_in %>%
    distinct( type ) %>%
    rename( target_type = type ) %>%
    by_row( generate_model, df_in ) %>%
    unnest( .out ) %>%
    { . } -> df_BIC


  df_BIC %>%
    mutate( power = paste( 'Power', power)) %>%
    spread( power, BIC)
}
