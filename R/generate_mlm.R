target_type='dur'
generate_mlm <- function(target_type, df_in_long) {


  df_in_long %>%
    dplyr::filter( type == pluck(target_type,1,1) ) %>%
    { . } -> df_clean

df_numeric = df_clean[,c('value','cohort_age')]

  mahal = mahalanobis( df_numeric,
                      colMeans(df_numeric, na.rm=TRUE),
                      cov( df_numeric, use='pairwise.complete.obs')
  )

  cutoff = qchisq( 1-.001, ncol( df_numeric ))

  df_no_out = df_clean[mahal <cutoff, ]
  df_no_out = df_clean

  models = tribble( ~model_name, ~formula,
                   "M1", as.formula(value ~1 + cohort_age + (1|id) ),
                   "Mpaper.1", as.formula(value ~1 + cohort*cohort_age + (1 + cohort_age|id)  ),
                   "Mpaper.2", as.formula(value ~1 + cohort*cohort_age + I(cohort_age^2) + (1 + cohort_age|id)) ,
                   "Mpaper.3", as.formula(value ~1 + cohort*cohort_age + I(cohort_age^2) + I(cohort_age^3) + (1 + cohort_age|id)) ,
                   "Mpaper.p1", as.formula(value ~1 + cohort*cohort_age + (1 + cohort_age|id)  ),
                   "Mpaper.p2", as.formula(value ~1 + cohort*cohort_age + I(cohort_age^2) + (1 + cohort_age + I(cohort_age^2)|id)) ,
                   "Mpaper.p3", as.formula(value ~1 + cohort*cohort_age + I(cohort_age^2) + I(cohort_age^3) + (1 + cohort_age + I(cohort_age^2) + I(cohort_age^3)|id))
)

  models %>%
    mutate( rv = map2(models$model_name, models$formula, function(model_name,formula) {
                        lmer( formula , data = df_no_out, REML=FALSE ) }))


}
