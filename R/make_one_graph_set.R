variable_name='dur'
graph_type='dur'
label='Duration'

make_one_graph_set = function( df1, df_in_long, df_age_label )
{

  graph_type = df1$type
  label=df1$label
  variable_name =df1$variable
  cat('\n')
  cat(glue::glue('## Variable {variable_name} {label}'))
  cat('\n')

  df_in_long %>%
    filter( type == graph_type ) %>%
    { . } -> df_in_long_filtered

  df_in_long_filtered %>%
    group_by( cohort, period, cohort_period, cohort_age) %>%
    summarise( value = mean( value), n = n(),
              .groups='drop') %>%
    mutate( cohort = as.factor(cohort)) %>%
    { . } -> temp

  temp %>%
    ggplot( aes( cohort_age, value, color=cohort   )) +
    geom_line() +
    geom_point(aes(size=n)) +
    ylab(label) +
    scale_size( name   = "N Points", breaks =  1:5*10 ) +
    scale_color_discrete(name= age_legend_title,
                        breaks=df_age_label$cohort,
                        labels=df_age_label$age_label
    ) +
    theme(legend.position="bottom") +
    scale_x_continuous("Age", labels = as.character(temp$cohort_age), breaks = temp$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle( glue::glue('Average {variable_name} by cohort ')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long_filtered %>%
    group_by( cohort_age) %>%
    summarise( m = mean( value ),
              interval = sd( value),
              .groups='drop') %>%
    ungroup() %>%
    mutate( sd_1_sd_u = m + interval ) %>%
    mutate( sd_1_sd_d = m - interval ) %>%
    mutate( sd_2_sd_u = m + interval * 2 ) %>%
    mutate( sd_2_sd_d = m - interval  * 2) %>%
    mutate( sd_3_sd_u = m + interval * 3 ) %>%
    mutate( sd_3_sd_d = m - interval  * 3) %>%
    select(cohort_age, starts_with('sd')) %>%
    pivot_longer( -cohort_age,
                 names_to=c('interval', 'direction'),
                 names_pattern = 'sd_(....)_(.)',
                 values_to='value') %>%
                 { . } ->df_in_long_filtered_sd

  df_in_long_filtered %>%
    ggplot( ) +
    geom_boxplot(aes( cohort_age, value, group=cohort_age)) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_point( aes( cohort_age, value, color=interval ),
               data= df_in_long_filtered_sd,
               shape=3) +
    ylab(label) +
    ggtitle( glue::glue('Boxplot for each age group for variable {variable_name}, {label}')) %>%
    { . } -> p
  print(p)

  cat('\n')

  df_in_long_filtered %>%
    group_by( cohort_age) %>%
    summarise( value = mean( value), n = n(),
              .groups='drop') %>%
    ggplot( aes( cohort_age, value)) +
    geom_line() +
    geom_point(aes(size=n)) +
    ylab(label) +
    scale_size( name   = "N Points", breaks =  1:5*10 ) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle( glue::glue('Average overall {variable_name} for each age group')) %>%
    { . } -> p
  print(p)
  cat('\n')

  cat("\n### Predicted Curves\n\n")

  df_in_long_filtered %>%
    ggplot( aes( cohort_age, value)) +
    geom_point() +
    geom_smooth(method = lm , se = TRUE) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab(label) +
    ggtitle( glue::glue('Curve prediction x^1  for variable {variable_name} for each age group')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long_filtered %>%
    ggplot( aes( cohort_age, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y~poly(x,2,raw=TRUE) , se = TRUE) +
    ylab(label) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle( glue::glue('Curve prediction x^2  for variable {variable_name} for each age group')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long_filtered %>%
    ggplot( aes( cohort_age, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y~poly(x,3,raw=TRUE) , se = TRUE) +
    ylab(label) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle( glue::glue('Curve prediction x^3  for variable {variable_name} for each age group')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long_filtered %>%
    ggplot( aes( cohort_age, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y~poly(x,4,raw=TRUE) , se = TRUE) +
    ylab(label) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle( glue::glue('Curve prediction x^4  for variable {variable_name} for each age group')) %>%
    { . } -> p
  print(p)
  cat('\n')

  df_in_long_filtered %>%
    ggplot( aes( cohort_age, value)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 4), se = TRUE) +
    ylab(label) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle( glue::glue('Curve prediction using bsplines of degree 4  for variable {variable_name} for each age group')) %>%
    { . } -> p
  print(p)
  cat('\n')
  TRUE

}
