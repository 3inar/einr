testmatrix <- plyr::raply(10, rnorm(1000))
testlist <- plyr::rlply(10, rnorm(1000))

pdf(file=NULL)

test_that("intervals accepts matrix or list", {
  intervals(testmatrix)
  intervals(testlist)

  expect_error(intervals("character"))
  expect_error(intervals(5))

})

dev.off()