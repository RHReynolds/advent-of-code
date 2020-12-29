#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

seats <-
  readr::read_delim(here::here("2020", "raw_data", "day_11.txt"),
                    delim = " ",
                    col_names = "seats")


seats_ex <-
  tibble(seats = c(
    "L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL", "L.LL.LL.LL",
    "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL", "L.LLLLLL.L", "L.LLLLL.LL"))

#---Main-------------------------------------------------------

#---Part 1-----------------------------------------------------

# Tidy layout such that:
# 1. Each seat (as defined by row and column) has a separate row
# 2. Unoccupied seats (L) convert to 0 and floor space (.) converted to NA
# 3. For each seat there is an id for its row, column, left diagonal and right diagonal
seats_tidy <-
  # seats_ex %>%
  seats %>%
  dplyr::mutate(row_id = 1:n()) %>%
  tidyr::separate_rows(seats, sep = "") %>%
  filter(seats != "") %>%
  dplyr::mutate(occupied = case_when(
    seats == "." ~ NA_integer_,
    TRUE ~ 1L)) %>%
  dplyr::group_by(row_id) %>%
  dplyr::mutate(col_id = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(l_diag_id = row_id - col_id,
                r_diag_id = col_id + row_id)

# For each seat, want to determine the occupation status of adjacent seats
# This can be done for each of the groups of seats (i.e. for each row, each column, etc.)
# As this will need to be separately for each group, make a function
get_adj_seats_by_group <- function(seats_tidy, ...){

  seats_tidy %>%
    dplyr::group_by(...) %>%
    # Lead/lag will find previous or next value
    # If previous or next value is NA (i.e. floor), simply replace this with 0
    dplyr::mutate(lag = occupied %>%
                    dplyr::lag(n = 1L) %>%
                    tidyr::replace_na(., 0L),
                  lead = occupied %>%
                    dplyr::lead(n  = 1L) %>%
                    tidyr::replace_na(., 0L),
                  n_occupied = n_occupied + lag + lead) %>%
    dplyr::select(-lag, -lead) %>%
    dplyr::ungroup()

}

# Now include function to get occupation status of adjacent seats in a function update all seats
update_seat_occupation <- function(seats_tidy){

  seats_tidy %>%
    dplyr::mutate(n_occupied = 0L) %>%
    dplyr::arrange(row_id, col_id) %>%
    get_adj_seats_by_group(col_id) %>%
    get_adj_seats_by_group(row_id) %>%
    get_adj_seats_by_group(l_diag_id) %>%
    get_adj_seats_by_group(r_diag_id) %>%
    dplyr::mutate(updated_occupied = case_when(
      n_occupied == 0 & occupied == 0L ~ 1L,
      n_occupied >= 4 & occupied == 1L ~ 0L,
      is.na(occupied) ~ occupied,
      TRUE ~ occupied
    ))

}

# Finally, create function to count number of occupied seats after chaos stabilises
get_stable_layout <- function(seats_tidy){

  round <- update_seat_occupation(seats_tidy)
  chaos_stable <- identical(round$occupied, round$updated_occupied)

  while(chaos_stable == FALSE){

    print(str_c(Sys.time(), ": Updating layout"))

    round <-
      round %>%
      dplyr::mutate(occupied = updated_occupied) %>%
      update_seat_occupation()

    chaos_stable <- identical(round$occupied, round$updated_occupied)

  }

  n_seats <-
    round %>%
    dplyr::count(occupied) %>%
    dplyr::filter(occupied == 1) %>%
    .[["n"]]

  return(n_seats)

}

ans_p1 <- get_stable_layout(seats_tidy)

print(str_c("Number of seats occupied: ", ans_p1))

#---Part 2-----------------------------------------------------

# Will need to modify get_adj_seats_by_group to accommodate principle of "first visible" seat
# I.e. Lead/lag functions now need to see beyond NAs (i.e. floor)
get_visible_seats_by_group <- function(seats_tidy, ...){

  # Remove NA rows prior to lead/lag
  # For non-existent rows, replace value with 0L
  visible_seats <-
    seats_tidy %>%
    dplyr::select(seats, row_id, occupied, col_id, l_diag_id, r_diag_id, n_occupied) %>%
    tidyr::drop_na(occupied) %>%
    dplyr::group_by(...) %>%
    # Lead/lag will find previous or next value
    dplyr::mutate(lag = occupied %>%
                    dplyr::lag(n = 1L, default = 0L),
                  lead = occupied %>%
                    dplyr::lead(n  = 1L, default = 0L),
                  n_occupied = n_occupied + lag + lead) %>%
    dplyr::select(-lag, -lead) %>%
    dplyr::ungroup()

  # Join back to original data frame
  seats_tidy %>%
    dplyr::select(-n_occupied) %>%
    dplyr::left_join(visible_seats , by = c("seats", "row_id", "occupied", "col_id", "l_diag_id", "r_diag_id"))

}

# Change rules of update_seat_occupation
update_seat_occupation <- function(seats_tidy){

  seats_tidy %>%
    dplyr::mutate(n_occupied = 0L) %>%
    dplyr::arrange(row_id, col_id) %>%
    get_visible_seats_by_group(col_id) %>%
    get_visible_seats_by_group(row_id) %>%
    get_visible_seats_by_group(l_diag_id) %>%
    get_visible_seats_by_group(r_diag_id) %>%
    dplyr::mutate(updated_occupied = case_when(
      n_occupied == 0 & occupied == 0L ~ 1L,
      n_occupied >= 5 & occupied == 1L ~ 0L,
      is.na(occupied) ~ occupied,
      TRUE ~ occupied
    ))

}

ans_p2 <- get_stable_layout(seats_tidy)

print(str_c("Number of seats occupied: ", ans_p2))
