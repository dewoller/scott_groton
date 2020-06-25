test_that("multiplication works", {
            expect_equal(2 * 2, 4)
})
################################################################################
test_that("Make one graph set", {

            loadd(df_one_names)
            loadd( df_in_long)

            df_in_long %>%
              distinct( type ) %>%
              mutate( variable = paste0(prefix, type) ) %>%
              inner_join( df_one_names ) %>%
              head(1) %>%
              make_one_graph_set(df_in_long)

            expect_equal(TRUE, TRUE)
})


################################################################################

test_that("Make one graph set", {

loadd(df_one_names)
loadd( df_in_long)

df_in_long %>%
  filter( type == 'mps') %>%
  group_by( cohort,period ) %>%
  filter( n() <3 )  %>%
  distinct( cohort, period) %>%
  ungroup() %>%
  { . } -> df_tofilter

df_in_long %>%
  distinct( type ) %>%
  mutate( variable = paste0(prefix, type) ) %>%
  inner_join( df_one_names, by='variable' ) %>%
  head(1) %>%
  make_one_graph_set()

            expect_equal(TRUE, TRUE)
})


################################################################################
