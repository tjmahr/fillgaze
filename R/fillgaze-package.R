#' fillgaze.
#'
#' @name fillgaze
#' @docType package
#' @import rlang
NULL



#' Set values in dataframe columns to NA
#'
#' @param data a dataframe
#' @param ... predicate functions that return true whenever a value should be
#'   replaced with NAs. The functions should be named, so that the argument
#'   `var1 = is.finite` would replace all the values in the column `var1` where
#'   `is.finite()` returns `TRUE` with `NA`` values. These predicate functions
#'   can be defined using the [formula syntax for anonymous
#'   functions][rlang::as_function]
#' @return a modified copy of the dataframe
#' @export
#' @examples
#' is_zero <- function(x) x == 0
#' set_values_to_na(mtcars, cyl = ~ .x == 6, vs = is_zero)
set_values_to_na <- function(data, ...) {
  dots <- quos(...)
  stopifnot(names(dots) %in% names(data), !anyDuplicated(names(data)))

  for (i in seq_along(dots)) {
    name <- sym(names(dots)[i])
    f <- as_function(eval_tidy(dots[[i]]))
    data <- dplyr::mutate(data, !! name := ifelse(f(!! name), NA, !! name))
  }

  data
}

