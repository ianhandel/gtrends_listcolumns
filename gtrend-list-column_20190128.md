gtrends in list columns
================
Ian Handel
29/01/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0           ✔ purrr   0.2.5      
    ## ✔ tibble  2.0.99.9000     ✔ dplyr   0.7.8      
    ## ✔ tidyr   0.8.2           ✔ stringr 1.3.1      
    ## ✔ readr   1.3.1           ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(gtrendsR)
library(fs) # for path_sanitise
library(lubridate) # for now()
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

### Build search table

try adding multi-term searches

``` r
# The signs to search for
signs <- c(
  "",
  "ill",
  "diarrhoea",
  "base-jumping-on-acid"
)

# The dog types to search for
dogs <- c(
  "puppy",
  "dog"
)

# All combinations of above (remove leading/trailing spaces)

searches <- tibble(dogs) %>%
  crossing(signs) %>%
  mutate(
    search = str_glue("{dogs} {signs}"),
    search = str_squish(search)
  ) %>% 
  group_by(dogs) %>%
  summarise(search = list(search)) %>% 
  ungroup()

head(searches)
```

    ## # A tibble: 2 x 2
    ##   dogs  search   
    ##   <chr> <list>   
    ## 1 dog   <chr [4]>
    ## 2 puppy <chr [4]>

### For each search do a gtrends call

``` r
searches <- searches %>%
  mutate(gtrend = map(search, gtrends,
    geo = "GB",
    time = "2011-01-01 2018-01-01"
  ))

head(searches)
```

    ## # A tibble: 2 x 3
    ##   dogs  search    gtrend       
    ##   <chr> <list>    <list>       
    ## 1 dog   <chr [4]> <S3: gtrends>
    ## 2 puppy <chr [4]> <S3: gtrends>

### Extract some dataframes into their own columns

``` r
searches <- searches %>%
  mutate(
    iot = map(gtrend, "interest_over_time"),
    ibr = map(gtrend, "interest_by_region"),
    rt = map(gtrend, "related_topics")
  )

head(searches)
```

    ## # A tibble: 2 x 6
    ##   dogs  search    gtrend       iot                 ibr                rt   
    ##   <chr> <list>    <list>       <list>              <list>             <lis>
    ## 1 dog   <chr [4]> <S3: gtrend… <data.frame [340 ×… <data.frame [16 ×… <NUL…
    ## 2 puppy <chr [4]> <S3: gtrend… <data.frame [340 ×… <data.frame [16 ×… <NUL…

### make a safe filename and save

``` r
rds_path <- str_glue("gtrend_searches_{now()}.rds") %>%
  path_sanitize() %>%
  str_replace(" ", "_")

write_rds(searches, rds_path)
```

### Unnest, say iot, then try a plot

uses [this solution](https://stackoverflow.com/questions/47224831/using-tidyr-unnest-with-null-values) to deal with NULL dataframes

uses `right_join(searches %>% select_if(~!is_list(.)))` to include null search info

``` r
iot <- searches %>%
  filter(!map_lgl(iot, is.null)) %>% 
  unnest(iot) %>% 
  right_join(searches %>% select_if(~!is_list(.))) %>% 
  mutate(hits = if_else(hits == "<1", 0, parse_number(hits)))
```

    ## Joining, by = "dogs"

``` r
ggplot(iot) +
  aes(date, hits, colour = keyword) +
  geom_line() +
  facet_wrap(~keyword, scale = "free") +
  theme(legend.position = "none")
```

![](gtrend-list-column_20190128_files/figure-markdown_github/unnamed-chunk-6-1.png)
