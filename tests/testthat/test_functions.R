data <- data.frame("y"= rnorm(20), "x"= gl(2, 10))

context("plot functions")

test_that("unwhich", {
  expect_equal(unwhich(c(2, 5), 8), c(F, T, F, F, T, F, F, F))
})


test_that("boxplot_nEle", {
  expect_silent(
    boxplot_nEle(y ~ x, data, test= "t.test", test_txt_par= list("col"= "blue"))
  )
  tg <- function(f, data) {
    boxplot_nEle(f, data, test= "t.test")
  }
  expect_silent(tg(y ~ x, data))
  
})

