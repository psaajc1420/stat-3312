#' Replicates data set based off the count variable.
#'
#' @param dataset A data.frame.
#' @param ... column name to replicate
#' @import dplyr
#' @export
#'
replicate.by.count <- function(dataset, ...) {
  col.name <- names(list(...))
  counts <- unlist(dataset[col.name])
  unique.counts <- unique(counts)
  data <- data.frame()
  for (row in seq(unique.counts)) {
    indexes <- (2*row-1):(2*row)
    unique.count <- unique.counts[row]
    data <- data %>% bind_rows(dataset[rep(indexes, unique.count),])
  }
  data %>%
    mutate(case = rep(seq(1, sum(counts)/2), each=2)) %>%
    select(-col.name)
}
