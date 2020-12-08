#---Load libraries---------------------------------------------
library(tidyverse)

#---Load data--------------------------------------------------

input <- read_delim(here::here("2020", "raw_data", "day_1.txt"),
                      delim = "\t",
                    col_names = "entry")

#---Main-------------------------------------------------------

#---Part 1---

# Determine difference
input_diff <- input %>%
  dplyr::mutate(diff = 2020 - entry)

# Find entries that match diff
# Summarise by multiplying
product_p1 <- input %>%
  dplyr::filter(entry %in% input_diff$diff) %>%
  dplyr::summarise(prod = prod(entry)) %>%
  .[["prod"]]

print(str_c("Product of entries summing to 2020 is: ", product_p1))

#---Part 2---

# Create data frame of all possible combinations and calculate diff
combinations <-
  combn(x = input$entry,
        m = 2) %>%
  t() %>%
  as_tibble() %>%
  dplyr::rename(entry = V1, entry_dup = V2) %>%
  dplyr::mutate(diff = 2020 - entry - entry_dup)

product_p2 <- input %>%
  dplyr::filter(entry %in% combinations$diff) %>%
  dplyr::summarise(prod = prod(entry)) %>%
  .[["prod"]]

print(str_c("Product of entries summing to 2020 is: ", product_p2))

