# age on x axis(
#
# 3.  can remove outliers >2.5 sd from mean  try removeing outliers that are 2.5 and 3, rerun graphs
# display confidence intervals for overall graph
# get rid of all background, and white grid


min_mandndi = 80
prefix='cog_gml'  # all relevant variables start with this

the_plan <-
  drake_plan(
   ## Plan targets in here.
             spss_orig  = read.spss('data/Groton Maze_v2.sav') ,
             df_in_long = process_input_data(prefix, spss_orig, min_mandndi )  ,
             df_one_names = get_spss_names( spss_orig )  ,
             df_model = generate_all_models( df_in_long ),
             graphs = target(
                                  command = {
                                    knitr_in("R/make_one_graph_set.R")
                                    workflowr::wflow_build( knitr_in("analysis/initial.Rmd"))
                                    file_out("docs/initial.html")
                                  }
             )
)

