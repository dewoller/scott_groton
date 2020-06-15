min_mandndi = 80
prefix='cog_gml'  # all relevant variables start with this

the_plan <-
  drake_plan(
   ## Plan targets in here.
             spss_orig  = read.spss('data/Groton Maze_v2.sav') ,
             df_in = process_input_data(prefix, spss_orig, min_mandndi )  ,
             df_one_names = get_spss_names( spss_orig )  ,
             df_model = generate_all_models( df_in ),
             makeModel = target(
                                  command = {
                                    rmarkdown::render(knitr_in("analysis/models.Rmd"))
                                    file_out("doc/models.html")
                                  }
             )
)

