#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

passports <-
  readr::read_lines(here::here("2020", "raw_data", "day_4.txt"),
                    skip_empty_rows = FALSE)

#---Main-------------------------------------------------------

#---Part 1---

# Passports are separated by empty row
# Use empty row to mark start of new group with 1
# Use cumulative sum + 1 to shift groups
passport_df <-
  tibble(passports = passports,
         group = case_when(passports == "" ~ 1,
                           TRUE ~ 0)) %>%
  dplyr::mutate(group = cumsum(group) + 1) %>%
  dplyr::filter(passports != "") %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(passports = str_c(passports, collapse = " "))

# Passport is valid either by having all of the following
# Do not cid for now
required <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

# Split passport fields into list of strings and remove "value"
# Check whether string is present in required and sum across logicals
passport_valid <-
  passport_df %>%
  dplyr::mutate(passports = str_split(passports, " ") %>%
                  lapply(., function(str) str %>% str_remove(., ":.*")),
                n_required = passports %>%
                  lapply(., function(str) str %in% required) %>%
                  lapply(., function(lgl) sum(lgl)) %>%
                  unlist()) %>%
  dplyr::filter(n_required == 7)

n_valid_p1 <- passports_valid %>% nrow()

print(str_c("Number of valid passports is: ", n_valid_p1))

#---Part 2---

# Filter for passports with required fields
passport_valid <-
  passport_df %>%
  dplyr::filter(group %in% passport_valid$group)

# Ideally want to use tidyr::separate to split passport info
# But two issues (i) fields not sorted and (ii) some passports have > 7 fields
# Sort and remove "cid"
passports_sorted <- passport_valid$passports %>%
  str_split(" ") %>%
  lapply(., sort) %>%
  lapply(., function(vec) vec[!str_detect(vec, "cid")])

# Check length of all passport info is 7
all(lapply(passports_sorted, length) %>% unlist() == 7)

# Split by fields
# Split height into value and units
# Split hair colour into # and str
# Turn appropriate fields into integers
passports_tidy <-
  passport_valid %>%
  dplyr::mutate(passports_sorted = passports_sorted %>%
                  lapply(., function(str) str %>% str_remove(., ".*:")) %>%
                  lapply(., function(str) str %>% str_c(collapse = " ")) %>%
                  unlist()) %>%
  tidyr::separate(col = "passports_sorted", into = c(sort(required)), sep = " ") %>%
  dplyr::select(-passports)

# Filter for passports with present and valid fields
n_valid_p2 <-
  passports_tidy %>%
  dplyr::mutate(hgt_value = readr::parse_number(hgt),
                hgt_unit = str_extract(hgt, "[aA-zZ]+"),
                hcl_hash = str_extract(hcl, "^."),
                hcl_value =  str_replace(hcl, "^.", ""),
                byr = as.integer(byr),
                eyr = as.integer(eyr),
                iyr = as.integer(iyr)) %>%
  dplyr::filter(str_count(byr) == 4, byr >= 1920, byr <= 2002,
                str_count(iyr) == 4, iyr >= 2010, iyr <= 2020,
                str_count(eyr) == 4, eyr >= 2020, eyr <= 2030,
                hgt_unit %in% c("cm", "in"),
                (hgt_unit == "cm" & hgt_value >= 150) | (hgt_unit == "in" & hgt_value >= 59),
                (hgt_unit == "cm" & hgt_value <= 193) | (hgt_unit == "in" & hgt_value <= 76),
                hcl_hash == "#",
                str_count(hcl_value) == 6,
                str_detect(hcl_value, "[a-f]+|[0-9]+"),
                ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
                is.integer(as.integer(pid)),
                str_count(pid) == 9) %>%
  nrow()

print(str_c("Number of valid passports is: ", n_valid_p2))

