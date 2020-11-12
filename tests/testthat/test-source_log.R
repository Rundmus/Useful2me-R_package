test_that("source_log function", {
  tF0 <- tempfile()

  cat(
    "print('slibrary(test)')",
    "library(dplyr)",
    "print('hello')  # library(tibble)",
    "  require(english)",
    "s <- stats::sd(stats::rnorm(10))",
    "m <- utils::citation()",
    file = tF0,
    sep = "\n"
  )

  # readLines(tF0)
  
  expect_null(.source(tF0, path = dirname(tF0)))
  
  #  created log file name
  log_fn <- list.files(
    dirname(tF0), 
    pattern = paste0(basename(tF0), ".*.log"),
    full.names = T
  )

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
  expect_true(
    any(grepl("utils-[[:digit:]]", first5lines))
  )
  expect_false(
    any(grepl("tibble-[[:digit:]]", first5lines))
  )
  
  file.remove(log_fn)
})

