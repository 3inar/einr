testmatrix <- plyr::raply(10, rnorm(1000))
testlist <- plyr::rlply(10, rnorm(1000))

testnames<- c(
  "centroids",
  "lasso",
  "stability (lasso)",
  "elastic net",
  "stability (enet)",
  "centroids + time",
  "lasso + time",
  "stability (l) + time",
  "elastic net + time",
  "stability (e) + time"
)

pdf(file=NULL)

test_that("intervals accepts matrix or list", {
  intervals(testmatrix)
  intervals(testlist)

  expect_error(intervals("character"))
  expect_error(intervals(5))

})

dev.off()