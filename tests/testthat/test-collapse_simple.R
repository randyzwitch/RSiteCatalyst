context("collapse_simple")


# ref data ----------------------------------------------------------------

test_df <- tryCatch(
  {
    dget("../testdata/test_set_unnested.txt")
  }, warning = function(w) {
    suppressMessages(w)
    dget("./tests/testdata/test_set_unnested.txt")
  }, finally = NULL
)

test_df_2 <- test_df
test_df_2$tags[3] <- list(NULL)


# tests -------------------------------------------------------------------

test_that("basic collapsing works for tags", {
  expect_identical(c("Search", NA, NA, NA, "SA_CNAME"), 
                   collapse_simple_target(test_df, "tags"))
})

test_that("basic collapsing works for compatibility", {
  expect_identical(
    c("reportsAndAnalytics, adHocAnalysis, dataWarehouse", "reportsAndAnalytics, adHocAnalysis, dataWarehouse", 
      "reportsAndAnalytics, adHocAnalysis, dataWarehouse", "reportsAndAnalytics, adHocAnalysis, dataWarehouse", 
      "reportsAndAnalytics, adHocAnalysis, dataWarehouse"), 
    collapse_simple_target(test_df, "compatibility")
  )
})

test_that("a field that is not found throws an error", {
  expect_error(collapse_simple_target(test_df, "idd"), 
               ".* not found in x")
})

test_that("a target field that is not a list throws an error", {
  expect_error(collapse_simple_target(test_df, "id"), 
               ".* must be a list")
})

test_that("a target field that is not a simple list throws an error", {
  expect_error(collapse_simple_target(test_df$definition$container, "rules"), 
               ".*character expected for parsing, but encountered data.frame at x\\[\\[1\\]\\] instead")
})

test_that("a target field with an unexpected structure throws an error", {
  expect_error(collapse_simple_target(test_df$definition, "container"), 
               ".*Mismatch in row count and target column length")
})

test_that("NULL is handled properly", {
  expect_identical(collapse_simple_target(test_df, "tags"), 
                   collapse_simple_target(test_df_2, "tags"))
})

