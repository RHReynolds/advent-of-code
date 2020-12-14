#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

seat_codes <-
  readr::read_delim(here::here("2020", "raw_data", "day_5.txt"),
                    col_names = "seat_code",
                    delim = " ")

#---Main-------------------------------------------------------

#---Part 1---

# Let's convert F/B and L/R to the same system of "L" for lower half and "U" for upper half
# And let's separating seat rows and seats number within rows
seats_tidy <-
  seat_codes %>%
  dplyr::mutate(seat_rows = str_extract(seat_code, "^.......") %>%
                  str_replace_all("F", "L") %>%
                  str_replace_all("B", "U"),
                seat_num = str_extract(seat_code, "...$") %>%
                  str_replace_all("R", "U"))

# Write a function to decode seat row/number
decode <- function(codes, max_seat, min_seat){

  seats <- vector(mode = "integer", length = length(codes))

  for(i in 1:length(codes)){

    code_split <-
      codes[i] %>%
      str_split("") %>%
      unlist()

    max_seat_current <- max_seat
    min_seat_current <- min_seat

    for(j in code_split){

      mid_seat <- mean(c(min_seat_current, max_seat_current))

      if(j == "L"){

        max_seat_current <- floor(mid_seat)

      } else if(j == "U"){

        min_seat_current <- ceiling(mid_seat)

      }

    }

    seats[i] <- min_seat_current

  }

  return(seats)

}

seats_decoded <- seats_tidy %>%
  dplyr::mutate(seat_row_decode = decode(seat_rows, max_seat = 127, min_seat = 0),
                seat_num_decode = decode(seat_num, max_seat = 7, min_seat = 0),
                seat_id = (seat_row_decode * 8) + seat_num_decode)

print(str_c("Highest seat id is: ", max(seats_decoded$seat_id)))

#---Part 2---

# Generate all possible seat ids from min/max seat id
seats_all <- tibble(seat_id = seq(min(seats_decoded$seat_id), max(seats_decoded$seat_id)))

# Find seat id by filtering for seat id not in seats_decoded
my_seat <- seats_all %>%
  dplyr::filter(!seat_id %in% seats_decoded$seat_id)

print(str_c("My seat id is: ", my_seat$seat_id))
