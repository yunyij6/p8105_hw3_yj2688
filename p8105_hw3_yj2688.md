p8105_hw3_yj2688
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Problem 1

``` r
data("instacart")


#Find number of aisles and aisle with most sales
instacart %>% 
  group_by(aisle_id) %>% 
  summarise(number_of_items = n()) %>%
  arrange(number_of_items) %>% 
  mutate(aisle_id = as.character(aisle_id)) %>%
  filter(number_of_items > 10000) %>%
  ggplot(aes(x = aisle_id, y = number_of_items)) + geom_col()+
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  labs(title = "Number of Items Ordered in Aisles", x = "Aisle ID", y = "Number of Items")
```

<img src="Untitled_files/figure-gfm/load dataset p8105-1.png" width="90%" />
There are 134 aisles. Aisle \#24 has the most items ordered.

``` r
instacart %>% 
  filter(aisle == "baking ingredients" | aisle == "dog food care" | aisle == "packaged vegetables fruits") %>%
  select(product_id, product_name, aisle) %>% 
  group_by(aisle, product_name) %>%
  summarise(times_ordered = n()) %>%
  arrange(desc(times_ordered)) %>% 
  top_n(n=3)
```

    ## `summarise()` has grouped output by 'aisle'. You can override using the
    ## `.groups` argument.
    ## Selecting by times_ordered

    ## # A tibble: 9 × 3
    ## # Groups:   aisle [3]
    ##   aisle                      product_name                                times…¹
    ##   <chr>                      <chr>                                         <int>
    ## 1 packaged vegetables fruits Organic Baby Spinach                           9784
    ## 2 packaged vegetables fruits Organic Raspberries                            5546
    ## 3 packaged vegetables fruits Organic Blueberries                            4966
    ## 4 baking ingredients         Light Brown Sugar                               499
    ## 5 baking ingredients         Pure Baking Soda                                387
    ## 6 baking ingredients         Cane Sugar                                      336
    ## 7 dog food care              Snack Sticks Chicken & Rice Recipe Dog Tre…      30
    ## 8 dog food care              Organix Chicken & Brown Rice Recipe              28
    ## 9 dog food care              Small Dog Biscuits                               26
    ## # … with abbreviated variable name ¹​times_ordered

Make a table showing the mean hour of the day at which Pink Lady Apples
and Coffee Ice Cream are ordered on each day of the week; format this
table for human readers (i.e. produce a 2 x 7 table).

``` r
instacart %>% 
  filter(product_name == "Pink Lady Apples" | product_name == "Coffee Ice Cream") %>%
  select(order_dow, order_hour_of_day, product_name) %>%
  group_by(product_name, order_dow) %>%
  summarise(mean_hour = mean(order_hour_of_day)) %>%
  pivot_wider(
    names_from  = "product_name",
    values_from = "mean_hour"
  ) %>%
  knitr::kable(digits = 2)
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the
    ## `.groups` argument.

| order_dow | Coffee Ice Cream | Pink Lady Apples |
|----------:|-----------------:|-----------------:|
|         0 |            13.77 |            13.44 |
|         1 |            14.32 |            11.36 |
|         2 |            15.38 |            11.70 |
|         3 |            15.32 |            14.25 |
|         4 |            15.22 |            11.55 |
|         5 |            12.26 |            12.78 |
|         6 |            13.83 |            11.94 |

## Problem 2

``` r
accel = read_csv(file = "./data/accel_data.csv", 
                 col_types = cols(.default = "c")) %>% 
  janitor::clean_names() %>%
  mutate(weekday_vs_weekend = if_else(day == 'Saturday'| day == 'Sunday', 'weekend', 'weekday'))
```

Describe the resulting data set: The resulting data set has 35 columns
and 1444 rows. The weekday_vs_weekend column has 2 values, weekday or
weekend, which represent the nature of the day when the activities were
recorded.

``` r
total = accel %>% 
  group_by(day_id) %>%
  mutate(total_activity = sum(activity_1:activity_1440)) %>%
  select(day_id, total_activity, weekday_vs_weekend)
```

Yes. The daily total activity count decreased over time, and the man is
more active on weekends than on some weekdays.

``` r
accel %>% 
  pivot_longer(activity_1:activity_1440, 
               names_to = 'minute',
               values_to = 'activity_count') %>%
  separate(col=minute, into=c('a', 'minute'), sep='_') %>%
  select(week, day_id, day, weekday_vs_weekend, minute, activity_count) %>%
  mutate(minute = as.numeric(minute), activity_count = as.numeric(activity_count), hour = minute/60) %>%
  mutate(hour = case_when(hour<=1 ~ 1, hour<=2 ~ 2, hour<=3 ~ 3, hour<=4 ~ 4,
                          hour<=5 ~ 5, hour<=6 ~ 6, hour<=7 ~ 7, hour<=8 ~ 8,
                          hour<=9 ~ 9, hour<=10 ~ 10, hour<=11 ~ 11, hour<=12 ~ 12,
                          hour<=13 ~ 13, hour<=14 ~ 14, hour<=15 ~ 15, hour<=16 ~ 16,
                          hour<=17 ~ 17, hour<=18 ~ 18, hour<=19 ~ 19, hour<=20 ~ 20,
                          hour<=21 ~ 21, hour<=22 ~ 22, hour<=23 ~ 23, hour<=24 ~ 24)) %>%
  select(-minute) %>%
  group_by(week, day_id, day, weekday_vs_weekend, hour) %>%
  summarize(activity_hour = sum(activity_count)) %>% 
  ggplot(aes(x = hour, y = activity_hour, color = day)) + geom_line() + 
  labs(
    title = "Activity Count Across 24 Hours Per Day",
    x = "Hour of the day",
    y = "Total Activity Per Hour") + 
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
               21, 22, 23, 24), 
    labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
               "17","18","19","20","21","22","23","24"))
```

    ## `summarise()` has grouped output by 'week', 'day_id', 'day',
    ## 'weekday_vs_weekend'. You can override using the `.groups` argument.

<img src="Untitled_files/figure-gfm/make a single panel plot-1.png" width="90%" />

\##Problem 3

``` r
data("ny_noaa")
ny_no = ny_noaa %>% 
  separate(col=date, into=c('year', 'month', 'day'), sep='-') %>%
  mutate_at(c('tmin', 'tmax'), as.numeric) %>%
  replace(is.na(.), 0) %>% 
  group_by(id, year, month) %>% 
  summarise(mean_max = mean(tmax)) %>% 
  filter(month == "01" | month == "07")
```

    ## `summarise()` has grouped output by 'id', 'year'. You can override using the
    ## `.groups` argument.

``` r
ny_no
```

    ## # A tibble: 14,111 × 4
    ## # Groups:   id, year [7,409]
    ##    id          year  month mean_max
    ##    <chr>       <chr> <chr>    <dbl>
    ##  1 US1NYAB0001 2008  01           0
    ##  2 US1NYAB0001 2008  07           0
    ##  3 US1NYAB0001 2009  01           0
    ##  4 US1NYAB0001 2009  07           0
    ##  5 US1NYAB0001 2010  01           0
    ##  6 US1NYAB0001 2010  07           0
    ##  7 US1NYAB0006 2009  01           0
    ##  8 US1NYAB0006 2009  07           0
    ##  9 US1NYAB0006 2010  01           0
    ## 10 US1NYAB0006 2010  07           0
    ## # … with 14,101 more rows
