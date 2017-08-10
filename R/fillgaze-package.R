#' fillgaze.
#'
#' @name fillgaze
#' @docType package
#' @import rlang dplyr
#' @importFrom tibble as_tibble data_frame add_column rowid_to_column
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
find_gaze_gaps <- function(data, var, time_var = NULL) {
  var <- enquo(var)
  time_var <- enquo(time_var)

  if (quo_is_null(time_var)) {
    # rowid_to_column doesn't work on grouped dfs
    groups <- groups(data)
    data <- ungroup(data)
    data <- rowid_to_column(data, ".rowid")
    data <- group_by(data, !!! groups)
    time_var <- quo(!! sym(".rowid"))
  }

  # If dataframe has dplyr groups, split based on those
  by_group <- split(dplyr::ungroup(data), f = dplyr::group_indices(data))
  gaps <- lapply(by_group, find_gaps_in_group, var, time_var)

  # If there are groups, we need to change start/end frames by row number in
  # overall table
  rows_per_group <- split(seq_len(nrow(data)), dplyr::group_indices(data))
  min_row_per_group <- lapply(rows_per_group, min)

  # Add grouping columns
  if (!length_zero(group_vars(data))) {
    # But don't add group columns when a group didn't have any gaps
    with_gaps <- names(Filter(function(x) nrow(x) != 0, gaps))

    group_vars <- lapply(by_group, distinct, !!! dplyr::groups(data))
    group_vars <- group_vars[with_gaps]

    for (group in with_gaps) {
      cols_to_add <- as.list(group_vars[[group]])
      gaps[[group]] <- add_column(
        gaps[[group]],
        .before = 1,
        UQS(cols_to_add))

      offset <- min_row_per_group[[group]] - 1
      gaps[[group]]["start_row"] <- gaps[[group]]["start_row"] + offset
      gaps[[group]]["end_row"] <- gaps[[group]]["end_row"] + offset
    }

  }

  df_gaps <- bind_rows(gaps)

  if (nrow(df_gaps) != 0) {
    # Find typical changes between frames
    diffs <- purrr::map(by_group, ~ diff(pull(.x, !! var), 1))
    all_diffs <- unlist(diffs, use.names = FALSE)
    clean_vals <- all_diffs[!is.na(all_diffs)]

    time_diffs <- purrr::map(by_group, ~ diff(pull(.x, !! time_var), 1))
    all_time_diffs <- unlist(time_diffs, use.names = FALSE)
    clean_times <- all_time_diffs[!is.na(all_diffs)]

    # Compute change per change in time
    clean_change_per_time <- clean_vals / clean_times
    change_per_time <- df_gaps$change_value / df_gaps$change_time

    typical_change <- stats::sd(c(clean_change_per_time, change_per_time))
    df_gaps$sd_change <- change_per_time / typical_change
    # 10, 20, NA, 30, 40, NA, 50, 60 has SD(change) of 0, so division by 0
    # yields Inf. Convert to 0-ish.
    df_gaps$sd_change <- ifelse(!is.finite(df_gaps$sd_change), 0.0001,
                                df_gaps$sd_change)
  }

  df_gaps
}

find_gaps_in_group <- function(data, var, time_var) {
  gazes <- eval_tidy(var, data)
  times <- eval_tidy(time_var, data)

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

  find_these_gaps <- function(...) gap(..., data = gazes, times = times)
  gaps <- Map(find_these_gaps, gap_start, gap_end, gap_size)

  is_not_first_frame <- function(gap) gap$start_row != 0
  gaps <- Filter(is_not_first_frame, gaps)

  gap_df <- purrr::map_df(gaps, tidy_gap)
  gap_df <- add_column(
    gap_df, .before = 1,
    .var = quo_name(var),
    .time_var = quo_name(time_var))
  as_tibble(gap_df)
}


#' @export
fill_gaze_gaps <- function(data, ..., time_var = NULL, max_na_rows = NULL, max_duration = NULL,  max_sd = NULL) {
  func <- stats::median
  dots <- quos(...)
  time_var <- enquo(time_var)

  columns_to_fill <- tidyselect::vars_select(names(data), !!! dots)
  vars <- quos(!!! syms(columns_to_fill))

  data_grouped <- split(data, group_indices(data))

  prepare_gaps <- function(var) {
    df <- find_gaze_gaps(data, !! var, !! time_var)
    if (nrow(df) != 0) {
      df <- filter(df, !treat_empty_as_false(na_rows > max_na_rows))
      df <- filter(df, !treat_empty_as_false(na_duration > max_duration))
      df <- filter(df, !treat_empty_as_false(abs(sd_change) > max_sd))
    }
    df
  }

  gaps <- purrr::map_df(vars, prepare_gaps)

  for (gap_i in seq_len(nrow(gaps))) {
    var_to_fill <- gaps[[gap_i, ".var"]]

    first_na_row <- gaps[[gap_i, "start_row"]] + 1
    last_na_row <- gaps[[gap_i, "end_row"]] - 1
    rows_to_fill <- seq(first_na_row, last_na_row)

    value_to_fill <- func(c(gaps[[gap_i, "start_value"]],
                            gaps[[gap_i, "end_value"]]))

    data[rows_to_fill, var_to_fill] <- value_to_fill
  }
  data
}






# Simple container for the information we care about when interpolating a gap
gap <- function(start, end, na_size, data, times) {
  list(
    start_row = start,
    end_row = end,
    na_rows = na_size,
    start_value = data[start],
    end_value = data[end],
    change = data[end] - data[start],
    time_start = times[start],
    time_first_na = times[start + 1],
    time_end = times[end],
    na_duration = times[end] - times[start + 1],
    change_time = times[end] - times[start]
  )
}

tidy_gap <- function(gap) {
  data.frame(
    start_row = gap$start_row,
    end_row = gap$end_row,
    na_rows = gap$na_rows,
    start_value = gap$start_value,
    end_value = gap$end_value,
    change_value = gap$change,
    time_start = gap$time_start,
    time_first_na = gap$time_first_na,
    time_end = gap$time_end,
    na_duration = gap$na_duration,
    change_time = gap$change_time
  )
}




treat_empty_as_false <- function(xs) {
  if (length_zero(xs)) FALSE else xs
}

length_zero <- function(x) length(x) == 0

