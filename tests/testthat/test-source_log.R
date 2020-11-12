test_that("source_log function", {
  tF0 <- tempfile()
  tmp <- setwd(tempdir(dirname(tF0)))
  on.exit(setwd(tmp))
  
  cat(
    "library(dplyr)",
    "require(english)",
    "s <- stats::sd(stats::rnorm(10))",
    file = tF0,
    sep = "\n"
  )

  expect_null(.source(tF0, path = "."))
  
  #  created log file name
  log_fn <- list.files(dirname(tF0), pattern = paste0(basename(tF0), ".*.log"))

  #  expect a line with loaded packages within first 5 lines
  first5lines <- readLines(log_fn, n = 5L)
  expect_true(
    any(grepl("dplyr-[[:digit:]]", first5lines))
  )
  expect_true(
    any(grepl("english-[[:digit:]]", first5lines))
  )
  expect_true(
    any(grepl("stats-[[:digit:]]", first5lines))
  )
  
  file.remove(log_fn)
})

