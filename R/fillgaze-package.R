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
  stopifnot(names(dots) %in% names(data), !anyDuplicated(names(dots)))

  for (i in seq_along(dots)) {
    name <- sym(names(dots)[i])
    f <- as_function(eval_tidy(dots[[i]]))
    data <- dplyr::mutate(data, !! name := ifelse(f(!! name), NA, !! name))
  }

  data
}


#' @export
find_gaps <- function(data, var) {
  var <- enquo(var)

  # If dataframe has dplyr groups, split based on those
  by_group <- split(dplyr::ungroup(data), f = dplyr::group_indices(data))
  gaps <- lapply(by_group, find_gaps_in_group, var)

  # If there are groups, we need to change start/end frames by row number in
  # overall table
  rows_per_group <- split(seq_len(nrow(data)), dplyr::group_indices(data))
  min_row_per_group <- lapply(rows_per_group, min)

  # Add grouping columns
  if (length(group_vars(data)) != 0) {
    # But don't add group columns when a group didn't have any gaps
    with_gaps <- names(Filter(function(x) nrow(x) != 0, gaps))

    group_vars <- lapply(by_group, distinct, !!! dplyr::groups(data))
    group_vars <- group_vars[with_gaps]

    for (group in with_gaps) {
      cols_to_add <- as.list(group_vars[[group]])
      gaps[[group]] <- tibble::add_column(
        gaps[[group]],
        .before = 1,
        UQS(cols_to_add))

      offset <- min_row_per_group[[group]] - 1
      gaps[[group]]["start"] <- gaps[[group]]["start"] + offset
      gaps[[group]]["end"] <- gaps[[group]]["end"] + offset
    }

  }

  bind_rows(gaps)
}

find_gaps_in_group <- function(data, var) {
  gazes <- eval_tidy(var, data)

  # Grab all the non-NA gaze frames.
  tracked <- which(!is.na(gazes))

  # The lag in frame numbers of non-NA gazes tells us how many NA frames were
  # skipped when we extracted all the non-NA gazes. Include the 0 at front
  # because diff(1:n) returns n-1 values
  differences <- diff(c(0, tracked))

  # Locations from `which` are not accurate because they don't take into account
  # earlier missing frames. Use the cumulative sum of missing frames to correct
  # these start locations.
  gap_start <- which(1 < differences)
  gap_size <- differences[gap_start] - 1
  total_gap_sizes <- cumsum(gap_size)

  # First gap doesn't need to be offset
  start_offsets <- c(0, total_gap_sizes[-length(total_gap_sizes)])
  gap_start <- gap_start + start_offsets - 1
  gap_end <- gap_start + gap_size + 1

  # Enforce valid windows! Margins need to be non-NA and next to an NA value
  stopifnot(
    is.na(gazes[c(gap_start + 1, gap_end - 1)]),
    !is.na(gazes[c(gap_start, gap_end)])
  )

  find_these_gaps <- function(...) gap(..., data = gazes)
  gaps <- Map(find_these_gaps, gap_start, gap_end, gap_size)

  is_not_first_frame <- function(gap) gap$start != 0
  gaps <- Filter(is_not_first_frame, gaps)

  gap_df <- purrr::map_df(gaps, tidy_gap)
  gap_df <- tibble::add_column(gap_df, .var = quo_name(var), .before = 1)
  tibble::as_tibble(gap_df)
}





#' @export
fill_gaze_gaps <- function(data, ..., func = median, max_gap, max_sd) {
  dots <- quos(...)

  prepare_gaps <- function(var) {
    df <- find_gaps(data, !! var)
    df$sd_change <- df$change / sd(df$change)

    too_long <- df$na_size > max_gap
    df <- df[!too_long, ]

    too_big <- abs(df$sd_change) > max_sd
    df <- df[!too_big, ]

    df
  }

  gaps <- purrr::map_df(dots, prepare_gaps)

  for (gap_i in seq_len(nrow(gaps))) {
    var_to_fill <- gaps[[gap_i, ".var"]]
    rows_to_fill <- seq(gaps[[gap_i, "start"]] + 1, gaps[[gap_i, "end"]] - 1)
    value_to_fill <- func(c(gaps[[gap_i, "start_value"]],
                            gaps[[gap_i, "end_value"]]))


    data[rows_to_fill, var_to_fill] <- value_to_fill
  }
  data
}






# Simple container for the information we care about when interpolating a gap
gap <- function(start, end, na_size, data) {
  list(
    start = start,
    end = end,
    na_size = na_size,
    start_value = data[start],
    end_value = data[end],
    change = data[end] - data[start]
  )
}

tidy_gap <- function(gap) {
  data.frame(
    start = gap$start, end = gap$end, na_size = gap$na_size,
    start_value = gap$start_value, end_value = gap$end_value,
    change = gap$change
    # , seq = list(gap$seq), na_seq = list(gap$na_seq)
  )
}







