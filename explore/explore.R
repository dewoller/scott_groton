source('_drake.R')


min_mandndi = 80
read.spss('data/Groton Maze_v2.sav') %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter( !is.na(participant_no)) %>%
  filter( mandndi > min_mandndi) %>%
  { . } -> df_in

df_in %>%
  count(mandndi) %>%
  filter( is.na( mandndi))



df_in %>%
  select(starts_with('age_t')) %>%
  count(age_corrected_t1) %>%
  count(age_t1) %>%
  count(agegroup_t1) %>%
  arrange(cohort)

df_in %>%
  count(cohort) %>%
  arrange(cohort)

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
  rename_at( vars( t1_names), ~t1_names_new) %>%
  { . } -> df_in

#t1_names %>% clipr::write_clip()


prefix='cog_gml'
df_in %>%
  select(-starts_with(prefix)) %>%
  arrange(cohort)

Total errors = CogGMLTER
Rule break errors = CogGMLRER
Duration? = CogGMLDUR

df_in %>%
  select(cohort, id, starts_with( prefix )) %>%
  pivot_longer( cols=starts_with( 'cog_gml' ),
                names_to=c('type','period') ,
                names_pattern="cog_gml(...)_t(.)",
                values_to='value',
                values_drop_na=TRUE ) %>%
  mutate( id = str_trim(id)) %>%
  mutate( period = as.numeric(period)) %>%
  { . } -> df_in_long

df_in_long %>%
  distinct( type ) %>%
  map( type, graph1 )


graph_type='dur'

graph1 = function( graph_type ) {

  df_in_long %>%
    filter( type == graph_type ) %>%
    group_by( cohort, period) %>%
    summarise( value = mean( value), n = n()) %>%
    ungroup() %>%
    mutate( cohort_period = cohort + period - 1 ) %>%
    mutate( cohort = as.factor(cohort)) %>%
    ggplot( aes( cohort_period, value, color=cohort   )) +
    geom_line() +
    geom_point(aes(size=n)) +
    ggtitle( paste('Average', type, 'by cohort ')) %>%
    { . } -> p
  print(p)

  df_in_long %>%
    filter( type == graph_type ) %>%
    mutate( cohort_period = cohort + period - 1 ) %>%
    group_by( cohort_period) %>%
    summarise( value = mean( value), n = n()) %>%
    ungroup() %>%
    ggplot( aes( cohort_period, value)) +
    geom_line() +
    geom_point(aes(size=n)) +
    ggtitle( paste('Average', prefix, '')) %>%
    { . } -> p
  print(p)

  df_in_long %>%
    filter( type == graph_type ) %>%
    mutate( cohort_period = as.factor(cohort + period - 1 )) %>%
    ggplot( aes( cohort_period, value)) +
    geom_boxplot() +
    ggtitle( paste('Average', type, '')) %>%
    { . } -> p
  print(p)

}







