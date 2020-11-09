context("Symmetric datasets")
library(stat3312)


testthat::test_that("create dataset", {
  counts <- c(266, 15, 61, 28, 10, 414, 50, 40, 8, 22, 578, 22, 7, 6, 27, 301)
  table <- matrix(counts, ncol=4, byrow=TRUE)

  names <- c("ne", "nw", "s", "w")
  colnames(table) <-  names
  rownames(table) <-  names

  name.matrix <- sapply(names, rep, times=4)
  lives <- as.vector(name.matrix)
  moves <- as.vector(t(name.matrix))

  col.names <- c("live", "move", "counts")
  dataset <- symmetric.contingency.table.to.dataset(table, col.names)
  expected.dataset <- data.frame(live=lives, move=moves, counts=counts)

  expect_equal(dataset, expected.dataset)
})
#> Test passed 🌈
