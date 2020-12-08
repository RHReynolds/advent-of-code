#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

passwords <- read_delim(here::here("2020", "raw_data", "day_2.txt"),
                    delim = " ",
                    col_names = c("min_max", "letter", "password"))

#---Main-------------------------------------------------------

#---Part 1---

# Start by splitting columns and tidying input
# Ensure min and max are numeric
passwords <- passwords %>%
  tidyr::separate(col = min_max,
                  into = c("min", "max"),
                  sep = "-") %>%
  dplyr::mutate(letter = str_remove(letter, ":"),
                min = as.numeric(min),
                max = as.numeric(max))

# Count the number of times a required letter occurs in a password
# Add a true/false as to whether a password complies
valid_passwords_p1 <- passwords %>%
  dplyr::mutate(letter_count = str_count(password,
                                         pattern = letter),
                valid = case_when(letter_count >= min &
                                    letter_count <= max ~ TRUE,
                                  TRUE ~ FALSE))

print(str_c("Number of valid passwords: ", sum(valid_passwords_p1$valid)))

#---Part 2---

# Extract string in min/max position
# Check whether string in nth position matches letter
# Filter for rows with only 1 matching letter
valid_passwords_p2 <- passwords %>%
  dplyr::mutate(pos_1_letter = str_sub(password, min, min),
                pos_2_letter = str_sub(password, max, max),
                n_valid_letters = case_when(pos_1_letter == letter &
                                              pos_2_letter == letter ~ 2,
                                            pos_1_letter == letter |
                                              pos_2_letter == letter ~ 1,
                                            TRUE ~ 0)) %>%
  dplyr::filter(n_valid_letters == 1)

print(str_c("Number of valid passwords: ", nrow(valid_passwords_p2)))
