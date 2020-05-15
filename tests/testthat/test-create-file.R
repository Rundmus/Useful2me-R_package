test_that("use.R_template creates file", {
  tF0 <- tempfile()
  tmp <- setwd(tempdir(dirname(tF0)))
  on.exit(setwd(tmp))
  
  tF <- basename(tF0)
  
  expect_message(
    use.R_template(tF, " "), 
    paste0('# The file \"', tF, '.R\" has been created.')
  )
  expect_true(file.exists(paste0(tF, ".R")))
  
  #  ref: https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package
  f <- file()
  options(mypkg.connection = f)
  ans <- paste(c("n", "y"), collapse = "\n") # set this to the number of tests you want to run
  write(ans, f)
  
  #  'n'
  expect_message(
    suppressMessages(use.R_template(tF, " ")), 
    '# No change.'
  )
  #  'y'
  expect_message(
    suppressMessages(use.R_template(tF, " ")), 
    paste0('# The file \"', tF, '.R\" has been created.')
  )
})
