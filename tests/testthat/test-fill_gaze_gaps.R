context("fill_gaze_gaps()")

library(testthat)
df <- data.frame(
  day = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
  trial = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
  time = c(3, 6, 9, 12, 15, 18, 21, 24, 27),
  # sd of changes is 5.5
  w = c(3, NA, 4, 4, NA, 5, 5, NA, 15),
  # single gap split across two trials
  x1 = c(1, 2, NA, NA, 5, 6, 7, 8, 9),
  # gaps of varying length
  x2 = c(1, 2, NA, NA, 5, NA, NA, NA, 9),
  # one gap across, one gap within groups
  x3 = c(1, 2, NA, NA, 5, 6, 7, NA, 9),
  # no gaps
  y1 = c(NA, 2, 3, 4, 5, 6, 7, NA, NA),
  y2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  y3 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  # gaps within trials
  z = c(10, NA, 30, 40, NA, 60, 70, NA, 90)
)

df_everything_filled <- data.frame(
  day = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
  trial = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
  time = c(3, 6, 9, 12, 15, 18, 21, 24, 27),
  # sd of changes is 5.5
  w = c(3, 3.5, 4, 4, 4.5, 5, 5, 10, 15),
  # single gap split across two trials
  x1 = c(1, 2, 3.5, 3.5, 5, 6, 7, 8, 9),
  # gaps of varying length
  x2 = c(1, 2, 3.5, 3.5, 5, 7, 7, 7, 9),
  # one gap across, one gap within groups
  x3 = c(1, 2, 3.5, 3.5, 5, 6, 7, 8, 9),
  # no gaps
  y1 = c(NA, 2, 3, 4, 5, 6, 7, NA, NA),
  y2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  y3 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  # gaps within trials
  z = c(10, 20, 30, 40, 50, 60, 70, 80, 90)
)

names_no_times <- c(".var", "start_row", "end_row", "na_rows",
                    "start_value", "end_value", "change_value")


test_that("fill every gap by default", {
  expect_equal(fill_gaze_gaps(df, w:z), df_everything_filled)
})


test_that("fill gaps within dataframe groups", {
  df_grouped <- dplyr::group_by(df, day, trial)
  df_filled <- fill_gaze_gaps(df_grouped, w:z)

  # filled within groups
  expect_equal(df_filled$w, df_everything_filled$w)
  expect_equal(df_filled$z, df_everything_filled$z)

  # no filling across groups
  expect_identical(df_filled$x1, df$x1)
  expect_identical(df_filled$x2, df$x2)

  # partial filling: first is across groups, second is within
  expect_identical(df_filled$x3, c(1, 2, NA, NA, 5, 6, 7, 8, 9))
})


test_that("fill gaps less than max row limit", {
  # nothing
  df_filled_0 <- fill_gaze_gaps(df, w:z, max_na_rows = 0)
  expect_equal(df_filled_0, df)

  # everything
  df_filled_3 <- fill_gaze_gaps(df, w:z, max_na_rows = 3)
  df_filled_4 <- fill_gaze_gaps(df, w:z, max_na_rows = 4)
  expect_equal(df_filled_3, df_everything_filled)
  expect_equal(df_filled_4, df_everything_filled)

  # one row - filled
  df_filled_1 <- fill_gaze_gaps(df, w:z, max_na_rows = 1)
  expect_equal(df_filled_1$w, df_everything_filled$w)
  expect_equal(df_filled_1$z, df_everything_filled$z)

  # one row - not filled
  expect_equal(df_filled_1$x1, df$x1)
  expect_equal(df_filled_1$x2, df$x2)

  # one row - partial
  expect_identical(df_filled_1$x3, c(1, 2, NA, NA, 5, 6, 7, 8, 9))

  # two row - filled
  df_filled_2 <- fill_gaze_gaps(df, w:z, max_na_rows = 2)
  expect_equal(df_filled_2$w, df_everything_filled$w)
  expect_equal(df_filled_2$x1, df_everything_filled$x1)
  expect_equal(df_filled_2$x3, df_everything_filled$x3)
  expect_equal(df_filled_2$z, df_everything_filled$z)

  # one row - partial
  expect_identical(df_filled_2$x2, c(1, 2, 3.5, 3.5, 5, NA, NA, NA, 9))
})


test_that("fill gaps less than max time duration", {
  # nothing
  df_filled_0 <- fill_gaze_gaps(df, w:z, time_var = time, max_duration = 0)
  expect_equal(df_filled_0, df)

  # everything
  df_filled_15 <- fill_gaze_gaps(df, w:z, time_var = time, max_duration = 15)
  expect_equal(df_filled_15, df_everything_filled)

  # one row - filled
  df_filled_3 <- fill_gaze_gaps(df, w:z, time_var = time, max_duration = 3)
  expect_equal(df_filled_3$w, df_everything_filled$w)
  expect_equal(df_filled_3$z, df_everything_filled$z)

  # one row - not filled
  expect_equal(df_filled_3$x1, df$x1)
  expect_equal(df_filled_3$x2, df$x2)

  # one row - partial
  expect_identical(df_filled_3$x3, c(1, 2, NA, NA, 5, 6, 7, 8, 9))

  # two row - filled
  df_filled_6 <- fill_gaze_gaps(df, w:z, time_var = time, max_duration = 6)
  expect_equal(df_filled_6$w, df_everything_filled$w)
  expect_equal(df_filled_6$x1, df_everything_filled$x1)
  expect_equal(df_filled_6$x3, df_everything_filled$x3)
  expect_equal(df_filled_6$z, df_everything_filled$z)

  # one row - partial
  expect_identical(df_filled_6$x2, c(1, 2, 3.5, 3.5, 5, NA, NA, NA, 9))
})

test_that("fill gaps less than sd value", {
  # nothing
  df_filled_0 <- fill_gaze_gaps(df, w:z, time_var = time, max_sd = 0)
  expect_equal(df_filled_0, df)

  # everything
  df_filled_10 <- fill_gaze_gaps(df, w:z, time_var = time, max_sd = 10)
  expect_equal(df_filled_10, df_everything_filled)

  # target the change in w
  df_filled_1 <- fill_gaze_gaps(df, w:z, time_var = time, max_sd = 1)
  expect_equal(df_filled_1$z, df_everything_filled$z)

  # partial fill
  expect_equal(df_filled_1$w, c(3, 3.5, 4, 4, 4.5, 5, 5, NA, 15))
})
