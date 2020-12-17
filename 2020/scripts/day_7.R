#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

bags <-
  readr::read_lines(here::here("2020", "raw_data", "day_7.txt"),
                    skip_empty_rows = FALSE)

#---Main-------------------------------------------------------

#---Part 1---

# Turn rules into a tibble with parent bags and associated child bags
bags_tidy <-
  tibble(bag_rules = bags) %>%
  tidyr::separate(col = "bag_rules", into = c("parent_bag", "child_bag"), sep = "bags contain") %>%
  dplyr::mutate(parent_bag = str_squish(parent_bag),
                child_bag = str_split(child_bag,"bags|bag"),
                child_bag = child_bag %>%
                  lapply(., function(str){

                    str %>%
                      str_remove(",") %>%
                      str_remove("\\.") %>%
                      str_squish()

                  })) %>%
  tidyr::unnest(child_bag) %>%
  dplyr::filter(child_bag != "") %>%
  # Parse "no other" as NA
  dplyr::mutate(n_child_bag = readr::parse_number(child_bag, na = "no other"),
                child_bag = str_remove(child_bag, as.character(n_child_bag)) %>%
                  str_squish())

# Write a function to find all possible parent_bags starting with one child_bag
find_parents <- function(bags_tidy, colour){

  parents <- bags_tidy %>%
    dplyr::filter(child_bag == colour, n_child_bag >= 1) %>%
    .[["parent_bag"]] %>%
    unique()

  parents_to_return <- parents

  while(nrow(bags_tidy %>%
             dplyr::filter(child_bag %in% parents, n_child_bag >= 1)) > 0){

    parents <-
      bags_tidy %>%
      dplyr::filter(child_bag %in% parents, n_child_bag >= 1) %>%
      .[["parent_bag"]] %>%
      unique()

    parents_to_return <- c(parents_to_return, parents)

  }

  return(unique(parents_to_return))

}

n_bags_p1 <- find_parents(bags_tidy, colour = "shiny gold") %>%
  length()

print(str_c("Number of bag colours that can eventually contain >= 1 shiny gold bag: ", n_bags_p1))

#---Part 2---

# Write a function to find all child bags when starting with one parent
find_children <- function(bags_tidy, colour){

  children_df <-
    bags_tidy %>%
    dplyr::filter(parent_bag == colour,
                  !is.na(n_child_bag))

  children_to_search <-
    children_df %>%
    .[["child_bag"]] %>%
    unique()

  # Tidyr has a very nice function that can help us "uncount" a data frame
  # It duplicates rows according to a weighting variable (i.e. n_child_bag)
  children_to_return <-
    children_df %>%
    tidyr::uncount(., weights = n_child_bag, .remove = FALSE)

  # Will also need parent weights
  parent_weights <-
    children_df %>%
    dplyr::select(parent_bag = child_bag,
                  n_parent_bag = n_child_bag)

  while(nrow(bags_tidy %>%
             dplyr::filter(parent_bag %in% children_to_search,
                           !is.na(n_child_bag))) > 0){

    # Filter for children_to_search
    children_df <-
      bags_tidy %>%
      dplyr::filter(parent_bag %in% children_to_search,
                    !is.na(n_child_bag))

    # Update children_to_search using new children_df
    children_to_search <-
      children_df %>%
      .[["child_bag"]] %>%
      unique()

    # Update children_to_return
    # Duplicate child bag rows in children_df by n_child_bag and by the number of parent bags
    children_to_return <-
      children_to_return %>%
      bind_rows(children_df %>%
                  tidyr::uncount(., weights = n_child_bag, .remove = FALSE) %>%
                  dplyr::inner_join(parent_weights) %>%
                  tidyr::uncount(., weights = n_parent_bag, .remove = FALSE))

    # Need to update the parent weights
    parent_weights <-
      children_df %>%
      dplyr::inner_join(parent_weights) %>%
      dplyr::mutate(updated_weight = n_child_bag * n_parent_bag) %>%
      dplyr::select(parent_bag = child_bag,
                    n_parent_bag = updated_weight)

  }

  return(children_to_return)

}

n_bags_p2 <- find_children(bags_tidy, colour = "shiny gold") %>%
  nrow()

print(str_c("Number of bags required inside single shiny gold bag: ", n_bags_p2))

#---Extra----

## Minimal example from day 7 instructions
# bags_tidy <-
#   tibble(bag_rules =
#            c("shiny gold bags contain 2 dark red bags.",
#              "dark red bags contain 2 dark orange bags.",
#             "dark orange bags contain 2 dark yellow bags.",
#              "dark yellow bags contain 2 dark green bags.",
#              "dark green bags contain 2 dark blue bags.",
#              "dark blue bags contain 2 dark violet bags.",
#              "dark violet bags contain no other bags.")) %>%
#   tidyr::separate(col = "bag_rules", into = c("parent_bag", "child_bag"), sep = "bags contain") %>%
#   dplyr::mutate(parent_bag = str_squish(parent_bag),
#                 child_bag = str_split(child_bag,"bags|bag"),
#                 child_bag = child_bag %>%
#                   lapply(., function(str){
#
#                     str %>%
#                       str_remove(",") %>%
#                       str_remove("\\.") %>%
#                       str_squish()
#
#                   })) %>%
#   tidyr::unnest(child_bag) %>%
#   dplyr::filter(child_bag != "") %>%
#   # Parse "no other" as NA
#   dplyr::mutate(n_child_bag = readr::parse_number(child_bag, na = "no other"),
#                 child_bag = str_remove(child_bag, as.character(n_child_bag)) %>%
#                   str_squish())
