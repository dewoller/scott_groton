# age on x axis(
#
# 3.  can remove outliers >2.5 sd from mean  try removeing outliers that are 2.5 and 3, rerun graphs
# display confidence intervals for overall graph
# get rid of all background, and white grid


min_mandndi = 80
prefix='cog_gml'  # all relevant variables start with this

age_legend_title = "Cohort at baseline (in 6 monthly intervals)"

the_plan <-
  drake_plan(
             # get data
             spss_orig  = read.spss('data/Groton Maze_v2.sav') ,
             #
             df_in = process_input_data_df_in(prefix, spss_orig, min_mandndi )  ,
             #
             df_in_long = process_input_data_df_in_long( df_in )  ,
             #
             df_in_long_filtered = filter_low_frequency( df_in_long ),
             #
             df_one_names = get_spss_names( spss_orig )  ,
             #
             df_age_label = get_age_label( df_in_long ),
             #
             df_outliers = select_outliers( df_in_long_filtered, n_sd=3 ),
             #
             df_in_long_filtered_twice = exclude_outliers( df_in_long_filtered, df_outliers ),

             # model data
             df_model = generate_all_models( df_in_long ),
             df_model_filtered = generate_all_models( df_in_long_filtered ),
             df_model_filtered_twice = generate_all_models( df_in_long_filtered_twice),
             # multilevel model data
             df_mlm_model = generate_all_mlm(df_in_long),
             df_mlm_model_filtered = generate_all_mlm(df_in_long_filtered),
             df_mlm_model_filtered_twice = generate_all_mlm(df_in_long_filtered_twice),

             # generate reports
             graphs = target(
                                  command = {
                                    knitr_in("R/make_one_graph_set.R")
                                    workflowr::wflow_build( knitr_in("analysis/initial.Rmd"))
                                    file_out("docs/initial.html")
                                  }
             )
)


