library(tidyverse, quietly = TRUE)
#> Warning: package 'tidyverse' was built under R version 3.5.3
#> Warning: package 'ggplot2' was built under R version 3.5.3
#> Warning: package 'tibble' was built under R version 3.5.3
#> Warning: package 'tidyr' was built under R version 3.5.3
#> Warning: package 'readr' was built under R version 3.5.2
#> Warning: package 'purrr' was built under R version 3.5.3
#> Warning: package 'dplyr' was built under R version 3.5.3
#> Warning: package 'stringr' was built under R version 3.5.2
#> Warning: package 'forcats' was built under R version 3.5.2

my_df<-data.frame(ID=c("A","A","A","A","A","A","B","B","B","C","C","C","C","C"),
                  Measured_DT_TM=as.POSIXct(c("2018-08-01 08:00:00","2018-08-01 08:20:00","2018-08-01 08:30:00","2018-08-01 08:35:00","2018-08-01 11:00:00","2018-08-01 11:30:00","2018-08-01 14:10:00","2018-08-01 15:40:00","2018-08-01 15:00:00","2018-08-01 13:00:00","2018-08-01 13:05:00","2018-08-01 13:30:00","2018-08-01 13:55:00","2018-08-01 14:40:00")),
                  blood_pressure=c(115,115,120,130,140,130,120,125,125,150,160,130,130,131)) %>%
  group_by(ID) %>% 
  arrange(Measured_DT_TM, .by_group=TRUE) %>% 
  mutate(time_since_first_measure=difftime(Measured_DT_TM, first(Measured_DT_TM), units = c("mins")),
         time_since_prev_measure=difftime(Measured_DT_TM, lag(Measured_DT_TM, n=1), units = c("mins")))

# steps broken out for readability
my_df %>% 
  mutate(all_measures_by_ID = list(time_since_first_measure),
         cutoff_60 = time_since_first_measure - 60, 
         check_for_measures_within_prev_60m = pmap(list(all_measures_by_ID, time_since_first_measure, cutoff_60), ~(..1 < ..2 & ..1 >= ..3)),
         no_measures_in_prev_60m = map(check_for_measures_within_prev_60m, sum)) %>%
  View()

# results in one line and no extra columns
my_df %>% 
  mutate(no_measures_in_prev_60m = pmap(list(list(time_since_first_measure), time_since_first_measure, time_since_first_measure - 60), 
                                        ~sum(..1 < ..2 & ..1 >= ..3))) %>%
  unnest(no_measures_in_prev_60m) %>%
  select(no_measures_in_prev_60m, everything())
#> # A tibble: 14 x 6
#> # Groups:   ID [3]
#>    no_measures_in_~ ID    Measured_DT_TM      blood_pressure
#>               <int> <fct> <dttm>                       <dbl>
#>  1                0 A     2018-08-01 08:00:00            115
#>  2                1 A     2018-08-01 08:20:00            115
#>  3                2 A     2018-08-01 08:30:00            120
#>  4                3 A     2018-08-01 08:35:00            130
#>  5                0 A     2018-08-01 11:00:00            140
#>  6                1 A     2018-08-01 11:30:00            130
#>  7                0 B     2018-08-01 14:10:00            120
#>  8                1 B     2018-08-01 15:00:00            125
#>  9                1 B     2018-08-01 15:40:00            125
#> 10                0 C     2018-08-01 13:00:00            150
#> 11                1 C     2018-08-01 13:05:00            160
#> 12                2 C     2018-08-01 13:30:00            130
#> 13                3 C     2018-08-01 13:55:00            130
#> 14                1 C     2018-08-01 14:40:00            131
#> # ... with 2 more variables: time_since_first_measure <drtn>,