context("find_gaze_gaps()")

library(testthat)
df <- data.frame(
  day = c(1, 1, 1, 1, 1, 1),
  trial = c("a", "a", "a", "b", "b", "b"),
  time = c(3, 6, 9, 12, 15, 18),
  # single gap split across two trials
  x = c(1, 2, NA, NA, 5, 6),
  # no gaps
  y1 = c(NA, 2, 3, 4, NA, NA),
  y2 = c(1, 2, 3, 4, 5, 6),
  y3 = c(NA, NA, NA, NA, NA, NA),
  # two gaps within trials
  z = c(10, NA, 30, 40, NA, 60)
)

names_no_times <- c(".var", "start_row", "end_row", "na_rows",
                    "start_value", "end_value", "change_value",
                    "sd_change")


test_that("finding gaze gaps ignores gaps on ends of window", {
  expect_equal(nrow(find_gaze_gaps(df, y1)), 0)
})


test_that("finding gaze gaps doesn't find gaps if no data missing", {
  expect_equal(nrow(find_gaze_gaps(df, y2)), 0)
})


test_that("finding gaze gaps doesn't find gaps if all data missing", {
  expect_equal(nrow(find_gaze_gaps(df, y3)), 0)
})


test_that("finding gaze gaps works with no groups and no time columns", {
  # one gap
  gaps <- find_gaze_gaps(df, x)
  expect_equal(gaps$.var, "x")
  expect_equal(gaps$.time_var, ".rowid")
  expect_equal(gaps$start_row, 2)
  expect_equal(gaps$end_row, 5)
  expect_equal(gaps$na_rows, 2)
  expect_equal(gaps$start_value, 2)
  expect_equal(gaps$end_value, 5)
  expect_equal(gaps$change_value, gaps$end_value - gaps$start_value)
  expect_equal(gaps$time_start, gaps$start_row)
  expect_equal(gaps$time_first_na, gaps$start_row + 1)
  expect_equal(gaps$time_end, gaps$end_row)
  expect_equal(gaps$na_duration, gaps$na_rows)
  expect_equal(gaps$change_time, gaps$time_end - gaps$time_start)
  expect_equal(gaps$sd_change, 0.0001)

  # two gaps
  gaps <- find_gaze_gaps(df, z)
  expect_equal(gaps$.var, c("z", "z"))
  expect_equal(gaps$start_row, c(1, 4))
  expect_equal(gaps$end_row, c(3, 6))
  expect_equal(gaps$na_rows, c(1, 1))
  expect_equal(gaps$start_value, c(1, 4) * 10)
  expect_equal(gaps$end_value, c(3, 6) * 10)
  expect_equal(gaps$change_value, gaps$end_value - gaps$start_value)
  expect_equal(gaps$time_start, gaps$start_row)
  expect_equal(gaps$time_first_na, gaps$start_row + 1)
  expect_equal(gaps$time_end, gaps$end_row)
  expect_equal(gaps$na_duration, gaps$na_rows)
  expect_equal(gaps$change_time, gaps$time_end - gaps$time_start)
  expect_equal(gaps$sd_change, c(0.0001, 0.0001))
})

test_that("finding gaze gaps works with time column and no groups", {
  ## one gaps
  gaps <- find_gaze_gaps(df, x, time_var = time)
  gaps_no_time <- find_gaze_gaps(df, x)

  # time related things differ
  expect_equal(gaps$.time_var, "time")
  expect_equal(gaps$time_start, 6)
  expect_equal(gaps$time_first_na, 9)
  expect_equal(gaps$time_end, 15)
  expect_equal(gaps$change_time, 15 - 6)
  expect_equal(gaps$na_duration, 15 - 9)

  # no other changes
  expect_equal(gaps[c(names_no_times)], gaps_no_time[c(names_no_times)])

  ## two gaps
  gaps <- find_gaze_gaps(df, z, time_var = time)
  gaps_no_time <- find_gaze_gaps(df, z)

  # time related things differ
  expect_equal(gaps$.time_var, c("time", "time"))
  expect_equal(gaps$time_start, c(3, 12))
  expect_equal(gaps$time_first_na, c(6, 15))
  expect_equal(gaps$time_end, c(9, 18))
  expect_equal(gaps$change_time, c(6, 6))
  expect_equal(gaps$na_duration, c(3, 3))

  # no other changes
  expect_equal(gaps[c(names_no_times)], gaps_no_time[c(names_no_times)])
})


test_that("finding gaze gaps ignores gap if spread across two groups", {
  df_grouped <- dplyr::group_by(df, trial)
  gaps <- find_gaze_gaps(df_grouped, x)
  expect_equal(nrow(gaps), 0)
})


test_that("finding gaze gaps handles grouped dataframes", {
  df_grouped <- dplyr::group_by(df, day, trial)
  ungrouped_gaps <- find_gaze_gaps(df, z)
  gaps <- find_gaze_gaps(df_grouped, z)

  # new column, otherwise everything else unchanged
  expect_named(gaps, c("day", "trial", names(ungrouped_gaps)))
  expect_equal(gaps[, c(-1, -2, -14)], ungrouped_gaps[-12])

  # with a time variable
  gaps <- find_gaze_gaps(df_grouped, z, time_var = time)
  ungrouped_gaps <- find_gaze_gaps(df, z, time_var = time)

  expect_named(gaps, c("day", "trial", names(ungrouped_gaps)))
  expect_equal(gaps[, c(-1, -2, -14)], ungrouped_gaps[-12])
})


