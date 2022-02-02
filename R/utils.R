#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Operator .data
#' @importFrom rlang .data
NULL

#' Operator "."
#' @importFrom utils globalVariables
utils::globalVariables(c("."))

#' Package Stats functions
#' @importFrom stats na.omit quantile time ts
NULL