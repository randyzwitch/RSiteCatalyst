context("flatten_nested_defn")


# ref data ----------------------------------------------------------------

if(dir.exists("../testdata/")) {
  testdat_dir <- "../testdata/"
} else {
  testdat_dir <- "./tests/testdata/"
}


# actual full segments
test_dats <- lapply(
  list(nest_rule = "nested_rule.txt", 
       nest_cont = "nested_container.txt", 
       nest_stack = "nested_stacked.txt"), 
  function(f) dget(paste0(testdat_dir, f))
)
# actual calculated metric, as a negative control of sorts
simple_CM <- dget(paste0(testdat_dir, "simple_calcMetric.txt"))

# reference outputs (regenerate these if you make a useful change to exported fun)
ref_outs <- lapply(
  list(nest_rule = "refOut_nested_rule.txt", 
       nest_cont = "refOut_nested_container.txt", 
       nest_stack = "refOut_nested_stacked.txt"), 
  function(f) dget(paste0(testdat_dir, f))
)


# live test output
live_outs <- lapply(test_dats, function(f) suppressMessages(flatten_nested_defn(f)))

# still ToDo; two types of errors, one my problem, one the API
# test_errors <- lapply(
#   list(complex_nest = "TODO_segs_exceptions.txt", # combination of nesting patterns
#        defn_notpars = "segs_10_one_notOK.txt", # one not parsed by API combined with 10 OK
#        defn_refpars = "segs_10_allOK.txt"), # only the 10 OK, as a positive control
#   function(f) dget(paste0(testdat_dir, f))
# )



# Basic functionality test ------------------------------------------------
# if this test fails, something has changed
test_that("exported function returns reference output", {
  expect_true(
    all(
      unlist(
        Map(identical, ref_outs, live_outs))
    )
  )
})

# First test helpers ------------------------------------------------------

## .is_nested_valid
test_that("valid structures must have 'container' as a first-level name", {
  expect_false(all
               (vapply(test_dats, function(f) 
                 SCAdmin:::.is_nested_valid(f), logical(1)))
  )
  expect_true(all(
    vapply(test_dats, function(f) 
      SCAdmin:::.is_nested_valid(f[["definition"]]), logical(1)))
  )
  expect_false(SCAdmin:::.is_nested_valid(simple_CM$definition))
})

##.filt_rule_null
test_that("invalid structures are trapped by filt_rule_null", {
  expect_length(
    lapply(test_dats, function(f) SCAdmin:::.filt_rule_null(f[["definition"]])), 
    n = 3
  )
  expect_error(SCAdmin:::.filt_rule_null(simple_CM$definition), 
               ".*a valid structure was not detected"
  )
})

