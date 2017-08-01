
<!-- README.md is generated from README.Rmd. Please edit that file -->
fillgaze
========

The goal of fillgaze is to provide helper functions for interpolating missing eyetracking data.

Installation
------------

You can install fillgaze from github with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/fillgaze")
```

Example
-------

This package was created to deal with a very strange file of eyetracking data.

``` r
df <- readr::read_csv("inst/test-gaze.csv")
```

Here is the problem with this file:

``` r
library(dplyr, warn.conflicts = FALSE)
#> Warning: package 'dplyr' was built under R version 3.4.1
library(ggplot2)

ggplot(head(df, 40)) + 
  aes(x = Time - min(Time)) + 
  geom_hline(yintercept = 0, size = 2, color = "white") + 
  geom_point(aes(y = GazeX, color = "GazeX")) +
  geom_point(aes(y = GazeY, color = "GazeY")) + 
  labs(colour = "Variable", y = "Screen location (pixels)")
```

![](fig/README-unnamed-chunk-2-1.png)

Every second or third point is incorrectly offscreen, indicated by a negative gaze value.

I would like to interpolate spans of missing data using the neighboring points. That's the point of this package. The steps to solve the problem involve:

-   \[x\] Converting offscreen values into proper NAs
-   \[ \] Identifying gaps of missing values (streaks of successive NAs)
-   \[ \] Interpolating the values in a gap

### Setting values in several columns to NA

I need to mark offscreen points as properly missing data. `set_values_to_na()` takes a dataframe and named filtering predicates. Here's the basic usage.

``` r
set_values_to_na(dataframe, {col_name} = {function to determine NA values})
```

The values that are `TRUE` for each function are replaced with `NA` values. That is, `set_values_to_na(df, var1 = ~ .x < 0)` would:

-   look for the column `var1` in the dataframe,
-   check which values of `.x < 0` are true where `.x` is a placeholder/pronou for the values in `var1`,
-   replace those values where test is `TRUE` with `NA`.

``` r
library(fillgaze)
before <- head(df$GazeX)

df <- df %>% 
  set_values_to_na(
    GazeX = ~ .x < -100, 
    GazeY = ~ .x < -100, 
    LEyeCoordX = ~ .x < -.1, 
    LEyeCoordY = ~ .x < -.1,
    REyeCoordX = ~ .x < -.1, 
    REyeCoordY = ~ .x < -.1)

# Before and after on some of the GazeX values
data_frame(before, after = head(df$GazeX))
#> # A tibble: 6 x 2
#>      before    after
#>       <dbl>    <dbl>
#> 1  1176.452 1176.452
#> 2 -1920.000       NA
#> 3 -1920.000       NA
#> 4  1184.452 1184.452
#> 5  1224.841 1224.841
#> 6 -1920.000       NA
```

Now, those offscreen points are not being plotted because they are NA.

``` r
ggplot(head(df, 40)) + 
  aes(x = Time - min(Time)) + 
  geom_hline(yintercept = 0, size = 2, color = "white") + 
  geom_point(aes(y = GazeX, color = "GazeX")) +
  geom_point(aes(y = GazeY, color = "GazeY")) + 
  labs(colour = "Variable", y = "Value")
#> Warning: Removed 15 rows containing missing values (geom_point).

#> Warning: Removed 15 rows containing missing values (geom_point).
```

![](fig/README-unnamed-chunk-5-1.png)
