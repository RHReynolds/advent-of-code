#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

adapters <-
  readr::read_delim(here::here("2020", "raw_data", "day_10.txt"),
                    delim = " ",
                    col_names = "joltage")

#---Main-------------------------------------------------------

#---Part 1---

# Tidy joltage table
adapters_tidy <-
  adapters %>%
  dplyr::add_row(joltage = max(.$joltage) + 3) %>%
  dplyr::arrange(joltage) %>%
  dplyr::mutate(diff = joltage - lag(joltage, default = 0))

ans_p1 <-
  adapters_tidy %>%
  dplyr::count(diff) %>%
  dplyr::summarise(product = prod(n)) %>%
  .[["product"]]

print(str_c("The number of 1-jolt differences multiplied by the number of 3-jolt differences: ", ans_p1))

#---Part 2---

# Only appear to have differences of 1 and 3 between adapters
adapters_tidy %>%
  .[["diff"]] %>%
  unique()

# When a difference of 3 is hit, this does not allow for any possibilities
# But contiguous 1s can provide a number of combinations, depending on their length
# So what is the distribution of contiguous 1s are there?
adapters_contig <-
  adapters_tidy %>%
  dplyr::filter(joltage != max(.$joltage)) %>%
  dplyr::mutate(group = case_when(diff == 3 ~ 1,
                                  TRUE ~ 0)) %>%
  dplyr::mutate(group = cumsum(group) + 1) %>%
  dplyr::filter(diff != 3) %>%
  dplyr::count(group, name = "n_contig_one")

adapters_contig %>%
  ggplot(aes(x = n_contig_one)) +
  geom_histogram()

# Can now create a tibble with the n_poss per length of contiguous 1s
# There must be a way of determining this mathematically, but couldn't figure it out
n_poss <-
  adapters_contig %>%
  dplyr::distinct(n_contig_one) %>%
  dplyr::mutate(n_poss = case_when(n_contig_one == 1 ~ 1,
                                   n_contig_one == 2 ~ 2, # 11, 2
                                   n_contig_one == 3 ~ 4, # 111, 12, 21, 3
                                   n_contig_one == 4 ~ 7)) # 1111, 112, 121, 211, 22, 13, 31

# Get the product of n_poss to get the number of distinct possibilities
ans_p2 <-
  adapters_contig %>%
  dplyr::inner_join(n_poss) %>%
  dplyr::summarise(prod = prod(n_poss)) %>%
  .[["prod"]]

print(str_c("Total number of distinct ways you can arrange the adapters: ", format(ans_p2, scientific = FALSE)))

