library(tidyverse)
library(gtrendsR)
library(fs) # for path_sanitise
library(lubridate) # for now()


# The signs to earch for
signs <- c(
  "",
  "ill",
  "diarrhoea",
  "vomiting",
  "coughing"
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
  )

# For each search do a gtrends call
searches <- searches %>%
  mutate(gtrend = map(search, gtrends,
    geo = "GB",
    time = "2011-01-01 2018-01-01"
  ))

# Extract some dataframes into their own columns
searches <- searches %>%
  mutate(
    iot = map(gtrend, "interest_over_time"),
    ibr = map(gtrend, "interest_by_region"),
    rt = map(gtrend, "related_topics")
  )


# make a safe filename and save
rds_path <- str_glue("gtrend_searches_{now()}.rds") %>% 
  path_sanitize() %>% 
  str_replace(" ", "_")

write_rds(searches, rds_path)


# Unnest, say iot, then try a plot
iot <- searches %>% 
  unnest(iot)

  ggplot(iot) +
  aes(date, hits, colour = search) +
  geom_line() +
  facet_wrap(~ search) +
  theme(legend.position = "none")
