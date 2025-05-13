#' Tools for handling auto report data
#'
#' @param data List of auto report data
#' @param entity A character string defining the entity. Likely one of
#'   \code{c("package", "type", "owner", "organization")}.
#'
#' @return A character vector of unique values for the entity in question.
#' @export
#'
#' @examples
#' ar <- list(ar1 = list(type = "A"), ar2 = list(type = "B"))
#' unique_autoreport(ar, "type") # c("A", "B")

unique_autoreport <- function(data, entity) {
  unique(data[[entity]])
}
