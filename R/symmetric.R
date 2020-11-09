#' Attaches the symmetric column to the dataset
#'
#' @param dataset A data.frame.
#' @param table.mapping A data.frame.
#' @return new dataset with symmetric data
#' @examples
#'
#' @export
add.symmetric.column <- function(dataset, table.mapping) {
  symmetric.column <- as.vector(table.mapping)
  dataset <- cbind(dataset, symmetric.column)
  return(dataset)
}

#' Creates a data set from a symmetric contingency table
#'
#' @param table A contingency table
#' @param col.names The dataset columns
#' @param num.way.table optional:
#' @return dataset from contingency table
#' @examples
#'
#' @export
symmetric.contingency.table.to.dataset <- function(table, col.names, num.way.table=2) {

  table.col.names <- colnames(table)
  table.row.names <- rownames(table)

  # check whether or not there are row names or column names
  if (is.null(table.col.names) & is.null(table.row.names)) {
    error <- simpleError("table must have column names or rownames")
    stop(error)
  }

  # if either are not null then decided which one isn't and use
  # the vector that's not null for the table names
  if (is.null(table.col.names)) {
    table.names <- table.row.names
  } else {
    table.names <- table.col.names
  }


  # create counts column by unraveling the table into a
  # 1-dimensional vector
  counts <- as.vector(t(table))

  # TODO - refactor to get rid of the gtools dependency
  two.way.cols <- gtools::permutations(n=length(table.names),
                                       r=num.way.table,
                                       v=table.names,
                                       repeats.allowed=TRUE)

  dataset <- cbind(as.data.frame(two.way.cols), counts)
  colnames(dataset) <- col.names
  return(dataset)
}
