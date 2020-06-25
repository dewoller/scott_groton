df_in %>%
  distinct(age_corrected) %>%
  arrange( desc( age_corrected))

60/12
147/12

df_in %>%
  select(age_t1, age_corrected, cohort, id, starts_with( prefix )) %>%
  pivot_longer( cols=starts_with( prefix ),
               names_to=c('type','period') ,
               names_pattern=paste0(prefix, "(...)_t(.)"),
               values_to='value',
               values_drop_na=TRUE ) %>%
  mutate( id = str_trim(id)) %>%
  mutate( period = as.numeric(period)) %>%
  mutate( cohort_period = cohort + period - 1 ) %>%
  filter( period == 1) %>%
  distinct( age_t1, age_corrected, id, period, cohort_period, cohort) %>%
  mutate( age_group = floor(age_corrected / 6) /2) %>%
  mutate( cohort_age = cohort_period / 2 + 4.5 ) %>% 
  { . } -> a

a %>%
  filter( cohort_age != age_group) %>%
  select( -cohort_period ) 

a %>%
  filter( cohort_age == age_group) %>%
  count( cohort, cohort_age, age_group, cohort_period, cohort_to_age( cohort))



  { . } -> df_in_long

filter



