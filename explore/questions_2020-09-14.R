source('_drake.R')


loadd( df_in_long_filtered )
loadd( df_in)



df_in %>%
  mutate( id = str_trim(id)) %>%
  inner_join( df_in_long_filtered  %>% distinct( id) ) %>%
  select(age_corrected) %>%
  mutate( age = age_corrected / 12, age_group=round(age)) %>%
  group_by( age_group) %>%
  summarise( mean_age = mean(age), sd_age= sd(age), .groups='drop') %>%
  gt()


df_in %>%
  mutate( id = str_trim(id)) %>%
  select(age_corrected) %>%
  mutate( age = age_corrected / 12, age_group=round(age)) %>%
  group_by( age_group) %>%
  summarise( mean_age = mean(age), sd_age= sd(age), .groups='drop', n=n()) %>%
  gt()
