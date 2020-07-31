generate_all_mlm <- function(df_in_long) {

  df_in_long %>%
    distinct( type ) %>%
    rename( target_type = type ) %>%
    mutate(rv = map( target_type, generate_mlm, df_in_long )) %>%
    unnest(rv) %>%
    group_by( target_type) %>%
    select(-formula) %>%
    nest(data=c(model_name, rv)) %>%
    ungroup() %>%
    { . } -> b

  b
}
