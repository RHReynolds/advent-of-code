#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

answers <-
  readr::read_lines(here::here("2020", "raw_data", "day_6.txt"),
                    skip_empty_rows = FALSE)

#---Main-------------------------------------------------------

#---Part 1---

# Start by tidying input
answers_tidy <-
  tibble(answers = answers,
         group = case_when(answers == "" ~ 1,
                           TRUE ~ 0)) %>%
  dplyr::mutate(group = cumsum(group) + 1) %>%
  dplyr::filter(answers != "") %>%
  dplyr::mutate(passenger_id = 1:length(answers),
                answers = str_split(answers, "")) %>%
  tidyr::unnest(answers)


# Find distinct group/answers
n_answers <- answers_tidy %>%
  dplyr::distinct(answers, group) %>%
  nrow()

print(str_c("The number of questions to which anyone answered yes: ", n_answers))

#---Part 2---

# Now need to find answers to which everyone in a group answered yes
# Count number of passengers in a group
# Count number of times answer occurs in group
# Find those answers where n_answer = n_passengers
n_passengers <-
  answers_tidy %>%
  dplyr::distinct(group, passenger_id) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(n_passenger = n())

n_answers <-
  answers_tidy %>%
  dplyr::group_by(group, answers) %>%
  dplyr::summarise(n_answers = n()) %>%
  dplyr::inner_join(n_passengers) %>%
  dplyr::filter(n_answers == n_passenger) %>%
  nrow()

print(str_c("The number of questions to which everyone in a group answered yes: ", n_answers))
