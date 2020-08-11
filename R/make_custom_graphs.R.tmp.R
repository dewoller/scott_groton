
make_custom_graphs = function( df_in_long, df_age_label ) {

  make_graph_set_x2(df_in_long %>% mutate( value = value / 1000 ),
                        df_age_label,
                        graph_type = 'dur',
                        max_y=400,
                        ylabel = "Duration Total (Seconds)" )


  make_graph_set_x2(df_in_long ,
                    df_age_label,
                    graph_type = 'ter',
                    max_y=80,
                    ylabel = "Total Errors" )

  make_graph_set_x2(df_in_long ,
                    df_age_label,
                    graph_type = 'ler',
                    max_y=60,
                    ylabel = "Legal Errors" )

  make_graph_set_x2(df_in_long ,
                        df_age_label,
                        graph_type = 'rer',
                        max_y=25,
                        ylabel = "Rule Break Errors" )

}

make_graph_set_x2 = function( df_in_long, df_age_label, graph_type, max_y, ylabel  ) {
  make_graph_set_cohort( df_in_long, df_age_label, graph_type, max_y, ylabel  )
  make_graph_set_overall( df_in_long, df_age_label, graph_type, max_y, ylabel  )

}

make_graph_set_cohort = function( df_in_long, df_age_label, graph_type, max_y, ylabel  )
{

  df_in_long %>%
    filter( type == graph_type ) %>%
    { . } -> df_in_long_filtered

  df_in_long_filtered %>%
    group_by( cohort, period, cohort_period, cohort_age) %>%
    summarise( value = mean( value), n = n(),
              .groups='drop') %>%
    mutate( cohort = as.factor(cohort)) %>%
    { . } -> temp

  # the graph
  temp %>%
    ggplot( aes( cohort_age, value    )) +
    geom_point(aes(size=n)) +
    ylab(label) +
    scale_size( name   = "N Points", breaks =  1:5*3 ) +
    scale_color_discrete(name= age_legend_title,
                         breaks=df_age_label$cohort,
                         labels=df_age_label$age_label
                         ) +
theme(legend.position="right") +
scale_x_continuous("Age", labels = as.character(temp$cohort_age), breaks = temp$cohort_age) +
scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))  %>%
{ . } -> p_leg1

    leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p_leg1)), "guide-box")
lg1 = get_legend( p_leg1)

# the graph
temp %>%
  ggplot( aes( cohort_age, value, color=cohort   )) +
  geom_line() +
  theme(legend.position="bottom") +
  scale_size( name   = "N Points", breaks =  1:5*10 ) +
  scale_color_discrete(name= age_legend_title,
                       breaks=df_age_label$cohort,
                       labels=df_age_label$age_label
                       ) +
scale_x_continuous("Age", labels = as.character(temp$cohort_age), breaks = temp$cohort_age) +
scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")) %>%
{ . } -> p_leg2

    leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p_leg2)), "guide-box")
lg2 = get_legend( p_leg2)

# the graph
temp %>%
  ggplot( aes( cohort_age, value, color=cohort   )) +
  geom_point(aes(size=n)) +
  geom_line() +
  theme(legend.position="none") +
  ylab( ylabel ) +
  scale_size( name   = "N Points", breaks =  1:5*10 ) +
  scale_color_discrete(name= age_legend_title,
                       breaks=df_age_label$cohort,
                       labels=df_age_label$age_label
                       ) +
scale_x_continuous("Age", labels = as.character(temp$cohort_age), breaks = temp$cohort_age) +
scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")) %>%
{ . } -> p

# 4.1 setup legends grid
plot_grid(
          plot_grid(p, leg1, rel_widths=c(10,1)),
          lg2, ncol=1, rel_heights=c(10,1)) -> p3
print(p3)
cat('\n')

}


make_graph_set_overall = function( df_in_long, df_age_label, graph_type, max_y, ylabel  )

{
  df_in_long %>%
    filter( type == graph_type ) %>%
    { . } -> df_in_long_filtered


  df_in_long_filtered %>%
    group_by( cohort_age) %>%
    summarise( value = mean( value), n = n(),
              .groups='drop') %>%
    ggplot( aes( cohort_age, value)) +
    geom_line() +
    geom_point(aes(size=n)) +
    ylab(ylabel) +
    scale_size( name   = "N Points", breaks =  1:5*10 ) +
    scale_x_continuous("Age", labels = as.character(df_in_long_filtered$cohort_age), breaks = df_in_long_filtered$cohort_age) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) %>%
    { . } -> p
  print(p)
  cat('\n')
}

