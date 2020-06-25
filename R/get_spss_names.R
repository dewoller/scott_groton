

get_spss_names <- function( spss_orig ) {


  read.spss('data/Groton Maze_v2.sav') %>%
  { . } -> spss_orig

    attr(spss_orig, 'variable.labels') %>%
      data.frame( stringsAsFactors=FALSE) %>%
      setNames('label') %>%
      tibble::rownames_to_column(var = "variable")  %>%
      as_tibble() %>%
      mutate(variable=str_to_lower(variable)) %>%
      { . } -> df_names

    df_names %>%
      filter( str_detect( variable, '_t5$', )) %>%
      mutate( variable = str_replace( variable, '_t5','')) %>%
      mutate( variable = str_replace( variable, '^cog','cog_')) %>%
      mutate( label = str_replace( label, ' - TIME 5','')) %>%
      { . } -> df_one_names

      df_one_names

}

