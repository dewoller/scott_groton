get_age_label <- function( df_in_long ) {


  df_in_long %>%
    filter( type=='dur' ) %>%
    distinct( cohort, cohort_age) %>%
    group_by(cohort) %>%
    filter( cohort_age == min(cohort_age) ) %>%
    arrange( cohort_age) %>%
    mutate(
           p1 = nmonth_label(cohort_age),
           p2 = nmonth_label(cohort_age+.5),
           ) %>%
    mutate( age_label = glue::glue('{p1} - < {p2}'))  %>%
    select( cohort, age_label) %>%
    { . } -> age_label
  age_label

}

nmonth_label = function( age ) {
  paste0( floor(age), 'yr', ifelse((age) - floor( age) > .4,'6mth','' ))
}

