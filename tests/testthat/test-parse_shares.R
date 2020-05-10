context("parse_shares")

# ref data ----------------------------------------------------------------

test_df <- tryCatch(
  {
    dget("../testdata/test_set_unnested.txt")
  }, warning = function(w) {
    suppressMessages(w)
    dget("./tests/testdata/test_set_unnested.txt")
  }, finally = NULL
)

# structure if requesting shares, but no shares at all so 
# return is list() instead of data.frame()
test_noShares <- tryCatch(
  {
    dget("../testdata/test_set_noShares.txt")
  }, warning = function(w) {
    suppressMessages(w)
    dget("./tests/testdata/test_set_noShares.txt")
  }, finally = NULL
)

# list of mock dfs for shares only
good_shares <- list(structure(list(type = c("user", "user", "group"), 
                                   name = c("aa", "bb", "group1")), 
                              .Names = c("type", "name"), 
                              row.names = c(NA, 3L), 
                              class = "data.frame"), 
                    structure(list(type = "user", 
                                   name = "ab"), 
                              .Names = c("type", "name"), 
                              row.names = c(NA, -1L), 
                              class = "data.frame"), 
                    structure(list(type = "user", 
                                   name = "ac"), 
                              .Names = c("type", "name"), 
                              row.names = c(NA, -1L), 
                              class = "data.frame")
)
good_df <- data.frame(
  A = LETTERS[1:3], 
  shares = cbind(unname(good_shares)), 
  id = c("id_1", "id_2", "id_3"),
  name = c("mock_1", "mock_2", "mock_3"),
  stringsAsFactors = FALSE
)


# tests -------------------------------------------------------------------
test_that("'shares' must be a list of data.frames found in the context of a data.frame", {
  expect_error(parse_shares(good_df[, c("shares")]))
  expect_error(parse_shares(good_shares), 
               ".*x must be a data.frame")
  expect_error(parse_shares(data.frame(
    shares = LETTERS[1:3], 
    id = "id", 
    name = "name",
    stringsAsFactors = FALSE)
  ), ".*Expected a list for 'structures', but class is character"
  )
})

test_that("As long as 'shares', 'name', and 'id' are present, collapsing works", {
  expect_is(parse_shares(good_df), "data.frame")
  expect_equal(
    sum(vapply(good_df$shares, nrow, FUN.VALUE = integer(1))), 
    nrow(parse_shares(good_df))
  )
  expect_error(parse_shares(good_df[, c("shares", "id")]), 
               ".*One or more expected names of 'id' and/or 'name' missing in x")
  expect_error(parse_shares(good_df[, c("shares", "name")]), 
               ".*One or more expected names of 'id' and/or 'name' missing in x")
})

test_that("prefixing names works", {
  expect_true(names(parse_shares(good_df))[1] == "id")
  expect_true(
    all(
      grepl("shares\\.", setdiff(names(parse_shares(good_df)), "id"))
    )
  )
})

test_that("A return without 'shares' raises an error", {
  expect_error(parse_shares(test_df[, setdiff(names(test_df), "shares")]), 
               ".*'shares' not found in.*"
  )
})

test_that("A multi-row return with a single shared segment returns a single-row df with 3 fields", {
  out <- parse_shares(test_df)
  expect_is(out, "data.frame")
  expect_length(nrow(out), 1L)
  expect_length(names(out),3L)
})

test_that("A return with 'shares' that are all zero-length returns NA_character_", {
  expect_identical(parse_shares(test_df[1:4, ]), NA_character_)
  expect_identical(parse_shares(test_noShares), NA_character_)
})
