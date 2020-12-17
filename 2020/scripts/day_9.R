#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

xmas_cypher <-
  readr::read_delim(here::here("2020", "raw_data", "day_9.txt"),
                    delim = " ",
                    col_names = "numbers")

#---Main-------------------------------------------------------

#---Part 1---

# Write function to find invalid number
find_invalid_number <- function(xmas_cypher, window = 25){

  index <- window + 1
  next_number <- xmas_cypher$numbers[index]
  xmas_cypher_subset <- xmas_cypher[(index - window):index, ]
  error <- FALSE

  while(error == FALSE){

    # Sum all possible values
    # Filter for all combinations that sum to next number
    valid_rows <-
      xmas_cypher_subset %>%
      dplyr::mutate(numbers_dup = numbers) %>%
      tidyr::expand(numbers, numbers_dup) %>%
      dplyr::mutate(sum = numbers + numbers_dup) %>%
      dplyr::filter(sum == next_number) %>%
      nrow()

    # If no combinations sum to next number, return error and break loop
    if(valid_rows == 0){

      error <- TRUE
      invalid_number <- next_number
      invalid_index <- index

    }

    index <- index + 1
    xmas_cypher_subset <- xmas_cypher[(index - window):index, ]
    next_number <- xmas_cypher$numbers[index]

  }

  return(tibble(invalid_number = invalid_number,
                invalid_index = invalid_index))

}

ans_p1 <- find_invalid_number(xmas_cypher, window = 25)

print(str_c("First invalid number is: ", ans_p1$invalid_number))

#---Part 2---

find_encryption_weakness <- function(xmas_cypher, invalid_number, invalid_index){

  # Subset up until index of invalid_number
  # Work from bottom upwards
  xmas_cypher_susbet <- xmas_cypher[1:invalid_index,]
  index <- 1
  cum_sum <- 0

  # Now must find contiguous set of > 2 numbers that sum to invalid number
  while(cum_sum != invalid_number){

    cum_sum_df <-
      xmas_cypher_susbet[index:nrow(xmas_cypher_susbet), ] %>%
      dplyr::mutate(cum_sum = cumsum(numbers)) %>%
      dplyr::filter(cum_sum <= invalid_number)

    cum_sum <- max(cum_sum_df$cum_sum)

    index <- index + 1

  }

  return(min(cum_sum_df$numbers) + max(cum_sum_df$numbers))

}

ans_p2 <-
  find_encryption_weakness(xmas_cypher,
                           invalid_number = ans_p1$invalid_number,
                           invalid_index = ans_p1$invalid_index)

print(str_c("The encryption weakness is: ", ans_p2))

