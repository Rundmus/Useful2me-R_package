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
  
})
