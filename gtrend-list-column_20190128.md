gtrends in list columns
================
Ian Handel
29/01/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0           ✔ purrr   0.2.5      
    ## ✔ tibble  2.0.99.9000     ✔ dplyr   0.7.8      
    ## ✔ tidyr   0.8.2           ✔ stringr 1.3.1      
    ## ✔ readr   1.3.1           ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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

``` r
# The signs to search for
signs <- c(
  "diarrhoea",
  "base-jumping-on-acid"
)

# The dog types to search for
dogs <- c(
  "puppies",
  "dog"
)

# All combinations of above (remove leading/trailing spaces)
searches <- tibble(dogs) %>%
  crossing(signs) %>%
  mutate(
    search = str_glue("{dogs} {signs}"),
    search = str_squish(search)) %>%
  mutate(search = list(c(search, "listsarefun")))

head(searches)
```

    ## # A tibble: 4 x 3
    ##   dogs    signs                search   
    ##   <chr>   <chr>                <list>   
    ## 1 puppies base-jumping-on-acid <chr [5]>
    ## 2 puppies diarrhoea            <chr [5]>
    ## 3 dog     base-jumping-on-acid <chr [5]>
    ## 4 dog     diarrhoea            <chr [5]>

### For each search do a gtrends call

``` r
searches <- searches %>%
  mutate(gtrend = map(search, gtrends,
    geo = "GB",
    time = "2011-01-01 2018-01-01"
  ))



head(searches)
```

    ## # A tibble: 4 x 4
    ##   dogs    signs                search    gtrend       
    ##   <chr>   <chr>                <list>    <list>       
    ## 1 puppies base-jumping-on-acid <chr [5]> <S3: gtrends>
    ## 2 puppies diarrhoea            <chr [5]> <S3: gtrends>
    ## 3 dog     base-jumping-on-acid <chr [5]> <S3: gtrends>
    ## 4 dog     diarrhoea            <chr [5]> <S3: gtrends>

### Extract some dataframes into their own columns

``` r
searches <- searches %>%
  mutate(
    iot = map(gtrend, "interest_over_time"),
    ibr = map(gtrend, "interest_by_region"),
    rq = map(gtrend, "related_queries")) %>%
  mutate(iot = map_if(iot, ~!is.null(.), ~mutate_all(.x, as.character)))

#BROKEN HERE

head(searches)
```

    ## # A tibble: 4 x 7
    ##   dogs   signs       search   gtrend    iot         ibr         rq         
    ##   <chr>  <chr>       <list>   <list>    <list>      <list>      <list>     
    ## 1 puppi… base-jumpi… <chr [5… <S3: gtr… <data.fram… <data.fram… <data.fram…
    ## 2 puppi… diarrhoea   <chr [5… <S3: gtr… <data.fram… <data.fram… <data.fram…
    ## 3 dog    base-jumpi… <chr [5… <S3: gtr… <data.fram… <data.fram… <data.fram…
    ## 4 dog    diarrhoea   <chr [5… <S3: gtr… <data.fram… <data.fram… <data.fram…

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
  mutate(date = anytime::anytime(date),
         hits = parse_number(hits))
```

    ## Joining, by = c("dogs", "signs")

``` r
ggplot(iot) + aes(date, hits, colour = keyword) +
  geom_line() +
  facet_wrap(~keyword) +
  theme(legend.position = "none")
```

![](gtrend-list-column_20190128_files/figure-markdown_github/unnamed-chunk-6-1.png)
