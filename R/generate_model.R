# load the relevant tidymodels libraries

# diabetes_recipe <-
#   # which consists of the formula (outcome ~ predictors)
#   recipe(diabetes ~ pregnant + glucose + pressure + triceps +
#          insulin + mass + pedigree + age,
#        data = diabetes_clean) %>%
# # and some pre-processing steps
# step_normalize(all_numeric()) %>%
# step_knnimpute(all_predictors())

#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#
# end diabetes
#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#
#generate_model( data.frame(target_type='ler'), df_in_long)
#target_type = 'ler'

generate_model <- function( target_type, df_in_long )
{

  df_in_long %>%
    dplyr::filter( type == pluck(target_type,1,1) ) %>%
    mutate(age = cohort + period - 1) %>%
    select( value, age) %>%
    { . } -> df_clean

  # df_clean %>%
  #   initial_split(  prop=3/4) %>%
  #   { . } -> df_split
  #
  # df_train <- training(df_split)
  # df_test <- testing(df_split)
  # df_cv <- vfold_cv(df_train)
  #
  # df_recipe <-
  #   # which consists of the formula (outcome ~ predictors)
  #   recipe(value ~ age,
  #          data = df_clean ) %>%
  # # and some pre-processing steps
  # step_normalize(all_numeric()) %>%
  # step_knnimpute(all_predictors())

  lm_model <-
    linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

  fitOne =  function( i) {
    fit( lm_model, formula=value ~ poly( age, i), data=df_clean) %>%
      pluck('fit' ) %>%
      BIC()
  }

  tibble( power=1:4) %>%
    mutate( BIC= map_dbl( power, fitOne))

}

