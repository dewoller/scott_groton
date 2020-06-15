make_one_graph_set = function( df1 )
{
  graph_type = df1$type
  label=df1$label
  variable_name =df1$variable_name
  cat('\n')
  cat(paste0('# Variable ', variable_name, ' ', label ))
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    group_by( cohort, period) %>%
    summarise( value = mean( value), n = n()) %>%
    ungroup() %>%
    mutate( cohort = as.factor(cohort)) %>%
    ggplot( aes( cohort_period, value, color=cohort   )) +
    geom_line() +
    geom_point(aes(size=n)) +
    ggtitle( paste0('Average ', variable_name, ' by cohort ')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    ggplot( aes( cohort_period, value)) +
    geom_boxplot() +
    ggtitle( paste0('Boxplot for each cohort/period for variable ', variable_name , '')) %>%
    { . } -> p
  print(p)

  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    group_by( cohort_period) %>%
    summarise( value = mean( value), n = n()) %>%
    ungroup() %>%
    ggplot( aes( cohort_period, value)) +
    geom_line() +
    geom_point(aes(size=n)) +
    ggtitle( paste0('Average overall ', variable_name, ' for each cohort/period')) %>%
    { . } -> p
  print(p)
  cat('\n')

  cat("\n## Predicted Curves\n\n")

  df_in_long %>%
    filter( type == graph_type ) %>%
    ggplot( aes( cohort_period, value)) +
    geom_point() +
    geom_smooth(method = lm , se = FALSE) +
    ggtitle( paste0('Curve prediction x^1  for variable ', variable_name, ' for each cohort/period')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    ggplot( aes( cohort_period, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y~poly(x,2,raw=TRUE) , se = FALSE) +
    ggtitle( paste0('Curve prediction x^2  for variable ', variable_name, ' for each cohort/period')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    ggplot( aes( cohort_period, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y~poly(x,3,raw=TRUE) , se = FALSE) +
    ggtitle( paste0('Curve prediction x^3  for variable ', variable_name, ' for each cohort/period')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    ggplot( aes( cohort_period, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y~poly(x,4,raw=TRUE) , se = FALSE) +
    ggtitle( paste0('Curve prediction x^4  for variable ', variable_name, ' for each cohort/period')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    ggplot( aes( cohort_period, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 4), se = FALSE) +
    ggtitle( paste0('Curve prediction using bsplines of degree 4  for variable ', variable_name, ' for each cohort/period')) %>%
    { . } -> p
  print(p)
  cat('\n')
  TRUE

}
