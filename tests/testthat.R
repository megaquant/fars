library(testthat)
library(fars)

test_that("sumyrs",expect_that(fars_summarize_years(2013:2015),is_a("tbl_df")))
# test_check("fars")
