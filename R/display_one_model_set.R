variable_name='dur'
graph_target_type='dur'
label=''

#source("_drake.R")

display_one_model_set = function( df1, df_mlm_model )
{


  graph_target_type = df1$target_type
  label=df1$label
  variable_name =df1$variable
  cat('\n')
  cat('\n')
  cat(glue::glue('## Model set for Variable {variable_name} {label}'))
  cat('\n')
  cat('\n')

  df_mlm_model %>%
    filter( target_type == graph_target_type ) %>%
    pluck(2,1, 'rv') %>%
    do.call(Curry(anova, .[[1]]), .) %>%
    print()

  df_mlm_model %>%
    filter( target_type == graph_target_type ) %>%
    pluck(2,1, 'rv') %>%
    msummary()


}
