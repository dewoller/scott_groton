df_in %>%
  distinct(age_corrected_t1) %>%
  arrange( desc( age_corrected_t1))


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
  filter( cohort_age != age_group) %>%
  select( -cohort_period ) %>%
  count(d)

  { . } -> df_in_long


  #generate_model( 'ler', df_in)
  df_in %>%
    #dplyr::filter( type == pluck(target_type,1,1) ) %>%
    dplyr::filter( type == pluck('ler',1,1) ) %>%
    mutate(age = cohort + period - 1) %>%
    select( value, age) %>%
    { . } -> df_clean

  lm_model <-
    linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

    fit( lm_model, formula=age ~ poly( value, 2), data=df_clean) %>% 
    { . } -> a

  findiplist(a)

      pluck('fit' ) %>% 
      { . } -> a


      BIC()





