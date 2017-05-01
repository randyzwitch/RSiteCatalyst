context("call.Get_base")


# filters -----------------------------------------------------------------

# filters must be a list
test_that("filters must be a list", {
  expect_error(call.Get_base(filters = c("A", "a")),
               ".*Class of 'filters' must be a list, but is currently.*")
})
# filters must be a named list
test_that("filters must be a named list", {
  expect_error(call.Get_base(filters = list("A", "B")), 
               ".*One or more names missing in 'filters.*")
})
# filters must be a named list, and all names must be valid
test_that("filters names must be valid", {
  expect_error(call.Get_base(filters = list(name = "A", alt="B")), 
               ".*Invalid input name\\(s\\) detected in 'filters'.*")
})
# filters named elements of name, owner, reportSuiteID must be vectors of length 1
test_that("filters 'reportSuiteID must be a vector of length 1", {
  expect_error(call.Get_base(filters = list(reportSuiteID = c("rsid1", "rsid2"))), 
               ".*owner, name, reportSuiteID must all be vectors of length 1.*"
               )
})
test_that("filters 'owner must be a vector of length 1", {
  expect_error(call.Get_base(filters = list(owner = c("person1", "person2"))), 
               ".*owner, name, reportSuiteID must all be vectors of length 1.*"
  )
})
test_that("filters 'name must be a vector of length 1", {
  expect_error(call.Get_base(filters = list(name = c("name1", "name2"))), 
               ".*owner, name, reportSuiteID must all be vectors of length 1.*"
  )
})
# test tags using helper to avoid having to authenticate and allow self-contained test
test_that("filters 'tags' can be a vector of length >= 1", {
  expect_silent(SCAdmin:::.l_helper_process_filters(list(tags = c("tag1", "tag2")))
  )
})
test_that("filters 'tags' of length >1 is collapsed into length 1 comma-sep character", {
  expect_equal(
    SCAdmin:::.l_helper_process_filters(list(tags = c("tag1", "tag2"))), 
    list(tags = c("tag1,tag2"))
  )
})
# filters named elements of approved, favorite must be vectors of length 1 AND
#  logical, or coercible to logical
test_that("approved must logical or coercible", {
  expect_error(call.Get_base(filters = list(approved = "A")), 
               ".*Invalid inputs resulting in 'NA' upon coercion to 'logical'.*")
})
test_that("approved must length 1 logical or coercible", {
  expect_error(call.Get_base(filters = list(approved = c("0", TRUE))), 
               ".*approved, favorite must all be vectors of length 1.*")
})
test_that("favorite must logical or coercible", {
  expect_error(call.Get_base(filters = list(favorite = "A")), 
               ".*Invalid inputs resulting in 'NA' upon coercion to 'logical'.*")
})
test_that("favorite must length 1 logical or coercible", {
  expect_error(call.Get_base(filters = list(favorite = c("0", TRUE))), 
               ".*approved, favorite must all be vectors of length 1.*")
})



# accessLevel and sort ----------------------------------------------------

# accessLevel must be vector of length 1
test_that("accessLevel must be a vector of length 1", {
  expect_error(call.Get_base(accessLevel = c("all", "owned")), 
               ".*'accessLevel' must be a vector of length 1")
})
# sort must be vector of length 1
test_that("sort must be a vector of length 1", {
  expect_error(call.Get_base(sort = c("id", "name")), 
               ".*'sort' must be a vector of length 1")
})

# accessLevel cannot contain invalid values
test_that("invalid values to accessLevel raise an error", {
  expect_error(call.Get_base(accessLevel = c("al")), 
               ".*Invalid input values\\(s\\) detected in 'accessLevel'.*")
})
# sort cannot contain invalid values
test_that("invalid values to sort raise an error", {
  expect_error(call.Get_base(sort = c("ID")), 
               ".*Invalid input values\\(s\\) detected in 'sort'.*")
})


# fields ------------------------------------------------------------------

# fields must contain valid values
## error; always get `id`, but not allowed value:
test_that("an invalid value of length 1 for 'fields' throws an error", {
  expect_error(call.Get_base(fields = "id"),
  "Invalid input values\\(s\\) detected in 'fields'.*")
})
# fields must contain ALL valid values
## 'reportSuiteID' is valid, 'id' is not
test_that("a single invalid value in 'fields' throws an error", {
  expect_error(call.Get_base(fields = c("id", "reportSuiteID")), 
               "Invalid input values\\(s\\) detected in 'fields'.*")
})
