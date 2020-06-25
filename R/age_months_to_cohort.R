
age_months_to_cohort = function( months ) {

  months  %>%
    divide_by( 6 ) %>%
    floor() %>%
    divide_by(2) %>%
    subtract( 4.5 ) %>%
    multiply_by( 2 )


}



# test_that("age_months_to_cohort_test", {
#             readd(df_in) %>%
#               mutate( ch = age_months_to_cohort( age_corrected )) %>%
#               select( age_corrected, cohort, ch) %>%
#               filter( ch != cohort) %>%
#               count() %>%
#               expect_equal(3)
# })
