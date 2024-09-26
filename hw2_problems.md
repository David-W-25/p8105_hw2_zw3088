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

## Problem 3

Import data of bakers

``` r
bakers_df =
  read_csv("./gbb_datasets/bakers.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  separate(
    baker_name, into = c("baker", "last_name"), sep = " "
  )
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Import data of bakes

``` r
bakes_df =
  read_csv("./gbb_datasets/bakes.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  mutate(
    baker = replace(baker, baker == '"Jo"', "Jo")
  )
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Import data of results

``` r
results_df =
  read_csv("./gbb_datasets/results.csv", skip = 2, na = c("NA", "", ".")) |>
  janitor::clean_names()
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Join the data frame into one using `left_join`

``` r
baker_show_df = left_join(results_df, bakers_df, by = c("baker", "series")) |>
  left_join(bakes_df, by = c("baker", "series", "episode")) |>
  relocate(baker, last_name, baker_age, baker_occupation, hometown, signature_bake, show_stopper) |>
  arrange(baker) |>
  mutate(result = str_to_lower(result)) |>
  filter(!is.na(result))
```

Export dataframe

``` r
write_csv(baker_show_df, "./gbb_datasets/baker_show.csv")
```

Show winners and star bakers from session 5 to 10.

``` r
baker_winner =
  baker_show_df |>
  filter(series >= 5 & series <= 10) |>
  filter(result == "winner" | result == "star baker") |>
  relocate(series, episode, result) |>
  arrange(series, episode)
head(baker_winner, 5)
```

    ## # A tibble: 5 × 11
    ##   series episode result     baker  last_name baker_age baker_occupation hometown
    ##    <dbl>   <dbl> <chr>      <chr>  <chr>         <dbl> <chr>            <chr>   
    ## 1      5       1 star baker Nancy  Birtwhis…        60 Retired Practic… Barton-…
    ## 2      5       2 star baker Richa… Burr             38 Builder          Mill Hi…
    ## 3      5       3 star baker Luis   Troyano          42 Graphic Designer Poynton…
    ## 4      5       4 star baker Richa… Burr             38 Builder          Mill Hi…
    ## 5      5       5 star baker Kate   Henry            41 Furniture Resto… Brighto…
    ## # ℹ 3 more variables: signature_bake <chr>, show_stopper <chr>, technical <dbl>

Some star bakers win the runner-up, which exemplify their skills and
talent (eg Luis Troyano), while some star bakers were out soon after
(Chetna Makan). Also, some winners did not won at least one star baker
in previous episode (eg David Atherton), which is a twist for TV show.

Import viewership data

``` r
viewers_df =
  read_csv("./gbb_datasets/viewers.csv", na = c("NA", "", ".")) |>
  janitor::clean_names()
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Show the first ten rows of the data

``` r
head(viewers_df, 10)
```

    ## # A tibble: 10 × 11
    ##    episode series_1 series_2 series_3 series_4 series_5 series_6 series_7
    ##      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1       1     2.24     3.1      3.85     6.6      8.51     11.6     13.6
    ##  2       2     3        3.53     4.6      6.65     8.79     11.6     13.4
    ##  3       3     3        3.82     4.53     7.17     9.28     12.0     13.0
    ##  4       4     2.6      3.6      4.71     6.82    10.2      12.4     13.3
    ##  5       5     3.03     3.83     4.61     6.95     9.95     12.4     13.1
    ##  6       6     2.75     4.25     4.82     7.32    10.1      12       13.1
    ##  7       7    NA        4.42     5.1      7.76    10.3      12.4     13.4
    ##  8       8    NA        5.06     5.35     7.41     9.02     11.1     13.3
    ##  9       9    NA       NA        5.7      7.41    10.7      12.6     13.4
    ## 10      10    NA       NA        6.74     9.45    13.5      15.0     15.9
    ## # ℹ 3 more variables: series_8 <dbl>, series_9 <dbl>, series_10 <dbl>

Calculate average viewership in session 1 and 5

``` r
session_1 = round(mean(viewers_df$series_1, na.rm = TRUE), 2)
session_5 = round(mean(viewers_df$series_5, na.rm = TRUE), 2)
```

The average viewership in session 1 is **2.77**, and in session 5 is
**10.04**.
