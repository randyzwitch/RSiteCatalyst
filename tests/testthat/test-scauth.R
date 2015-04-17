test_that("SCAuth legacy credentials work as expected", {
  
  skip_on_cran()

  #Correct [masked] credentials
  expect_output(SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", "")),
                "Credentials Saved in RSiteCatalyst Namespace.")
  
  #Test for error when no arguments passed
  expect_error(SCAuth())
  
  #Test for error when only key passed
  expect_error(SCAuth("rzwitch:ZwitchCorp"))
  
  #Test for error when company name not passed
  #Throws warning first, then error
  #Not real secret :)
  expect_error(expect_warning(SCAuth("rzwitch", "bff49a587d1abd70c2d0f02fa3e9592e")))
  
  #Warning/Error for not enough characters in secret
  expect_error(expect_warning(SCAuth("rzwitch:ZwitchCorp", "bff49a587d1abd70c2d0f02fa3e9592")))
  
})

