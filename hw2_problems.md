Homework 2 Problems
================
Zicheng Wang
2024-09-25

Initialization of packages.

## Problem 1

Import subway E&T data, clean the names and retain line, station, name,
station latitude / longitude, routes served, entry, vending, entrance
type, and ADA compliance. In addition, the entry has been changed from
chr to logic.

``` r
subway_df = 
  read_csv(file = "./NYC_Transit_Subway_Entrance_And_Exit_Data.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  select(line:entry, vending, ada) |>
  mutate(
      entry = case_match(
        entry,
        "YES" ~ TRUE,
        "NO" ~ FALSE
      )
  )
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Before the cleaning, the raw data had 32 variables, after the cleaning,
the dataset had **19** variables and **1868** observations (dimension:
1868\*19). The dataset is tidied according to the instrcutions.

``` r
distinct_station = nrow(distinct(subway_df, line, station_name))
```

There are **465** distinct stations in the dataset.

``` r
subway_df = 
  subway_df |>
  distinct(line, station_name, .keep_all = TRUE)
```

Calculate how many station that are ADA compliant, and the proportion of
stations that not support vending in entry/exit.

``` r
ada_support = sum(subway_df$ada == TRUE)
vending_not_support = sum(subway_df$vending == "NO")
proportion_not_support = vending_not_support / distinct_station * 100
```

There are **84** ADA compliant. There are **1.9354839** % proportion of
station that not support vending in entry/exit.

Reformat data so that route number and route name are distinct
variables.

``` r
subway_tidy_df = 
  subway_df |>
  mutate(
    route8 = as.character(route8),
    route9 = as.character(route9),
    route10 = as.character(route10),
    route11 = as.character(route11)
  ) |>
  pivot_longer(
    route1:route11,
    names_to = "route_number",
    names_prefix = "route",
    values_to = "route_names"
  ) |>
  filter(!is.na(route_names))
```

Calculate how many distinct stations serve the A train, and in these
station how many are ADA compliant.

``` r
Station_with_A_route = sum(subway_tidy_df$route_names == "A")
A_route = filter(subway_tidy_df, subway_tidy_df$route_names == "A")
ADA_compliant_A = sum(A_route$ada == TRUE)
```

There are **60** stations serve the A train, and among these stations,
**17** stations are ADA compliant.

## Problem 2

Import data of Mr trash wheel

``` r
mr_trash_wheel_df = 
  read_excel("./202309 Trash Wheel Collection Data.xlsx",
             sheet = "Mr. Trash Wheel",
             range = "A2:N586",
             na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    sports_balls = as.numeric(round(sports_balls, digits = 0)),
    trash_wheel = "mr_trash_wheel",
    year = as.double(year)
  )
```

Show the head of Mr trash wheel

``` r
head(mr_trash_wheel_df, 5)
```

    ## # A tibble: 5 × 15
    ##   dumpster month  year date                weight_tons volume_cubic_yards
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ## 1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ## 2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ## 3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ## 4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ## 5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ## # ℹ 9 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, sports_balls <dbl>, homes_powered <dbl>, trash_wheel <chr>

Import data of professor trash wheel

``` r
prof_trash_wheel_df = 
  read_excel("./202309 Trash Wheel Collection Data.xlsx",
             sheet = "Professor Trash Wheel",
             range = "A2:M108",
             na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    trash_wheel = "prof_trash_wheel"
  )
```

Show the head of Professor trash wheel

``` r
head(prof_trash_wheel_df, 5)
```

    ## # A tibble: 5 × 14
    ##   dumpster month     year date                weight_tons volume_cubic_yards
    ##      <dbl> <chr>    <dbl> <dttm>                    <dbl>              <dbl>
    ## 1        1 January   2017 2017-01-02 00:00:00        1.79                 15
    ## 2        2 January   2017 2017-01-30 00:00:00        1.58                 15
    ## 3        3 February  2017 2017-02-26 00:00:00        2.32                 18
    ## 4        4 February  2017 2017-02-26 00:00:00        3.72                 15
    ## 5        5 February  2017 2017-02-28 00:00:00        1.45                 15
    ## # ℹ 8 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, trash_wheel <chr>

Import data of gwynnda trash wheel

``` r
gwynnda_trash_wheel_df = 
  read_excel("./202309 Trash Wheel Collection Data.xlsx",
             sheet = "Gwynnda Trash Wheel",
             range = "A2:L157",
             na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    trash_wheel = "gwynnda_trash_wheel"
  )
```

Show the head of Gwynnda trash wheel

``` r
head(gwynnda_trash_wheel_df, 5)
```

    ## # A tibble: 5 × 13
    ##   dumpster month  year date                weight_tons volume_cubic_yards
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ## 1        1 July   2021 2021-07-03 00:00:00        0.93                 15
    ## 2        2 July   2021 2021-07-07 00:00:00        2.26                 15
    ## 3        3 July   2021 2021-07-07 00:00:00        1.62                 15
    ## 4        4 July   2021 2021-07-16 00:00:00        1.76                 15
    ## 5        5 July   2021 2021-07-30 00:00:00        1.53                 15
    ## # ℹ 7 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, plastic_bags <dbl>, wrappers <dbl>,
    ## #   homes_powered <dbl>, trash_wheel <chr>

Combine three existing trash wheel data into one data frame

``` r
trash_wheel_tidy =
  bind_rows(mr_trash_wheel_df, prof_trash_wheel_df, gwynnda_trash_wheel_df) |>
  janitor::clean_names() |>
  relocate(trash_wheel)
```

Calculate the total weight of professor trash wheel and total number of
cigarette butts collected by Gwynnda trash wheel in June of 2022.

``` r
total_weight = sum(trash_wheel_tidy$weight_tons[trash_wheel_tidy$trash_wheel == "prof_trash_wheel"],
                   na.rm = TRUE)

cigarette_butt = sum(trash_wheel_tidy$cigarette_butts[
  trash_wheel_tidy$trash_wheel == "gwynnda_trash_wheel" &
  trash_wheel_tidy$month == "June" &
  trash_wheel_tidy$year == 2022],
  na.rm = TRUE)
```

The total weight of professor trash wheel is **216.26** tons, and total
number of cigarette butts collected by Gwynnda trash wheel in June of
2022 is **1.812^{4}** counts.
