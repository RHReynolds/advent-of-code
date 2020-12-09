#---Load libraries---------------------------------------------
library(purrr)
library(tidyverse)
library(stringr)
library(qdapTools)

#---Load data--------------------------------------------------

map <-
  readr::read_delim(here::here("2020", "raw_data", "day_3.txt"),
                    delim = " ",
                    col_names = "map")

#---Main-------------------------------------------------------

#---Part 1---

# Inputs
start <- 1
n_right <- 3

# Start by determining number of times string has to be repeated
# to expand map to encompass number of rows
n_rep <- ceiling(nrow(map)/str_count(map$map[1]) * 3)

# Expand map
expanded_map <-
  map %>%
  dplyr::mutate(expanded_map = str_dup(map, n_rep)) %>%
  dplyr::select(map = expanded_map)

# Create empty list for loop output
map_list <- vector(mode = "list", length = nrow(expanded_map))

# Loop across rows and extract entry at relevant position on map
for(i in 1:nrow(expanded_map)){

  pos_to_extract <- start + (n_right * (i-1))

  map_list[[i]] <-
    expanded_map[i, ] %>%
    dplyr::mutate(map_entry = str_sub(map, pos_to_extract, pos_to_extract))

}

# Sum number of trees encountered
n_trees_p1 <-
  map_list %>%
  qdapTools::list_df2df(col1 = "X1") %>%
  dplyr::select(map_entry) %>%
  dplyr::mutate(tree = case_when(map_entry == "#" ~ 1,
                                 map_entry == "." ~ 0)) %>%
  dplyr::summarise(n_trees = sum(tree)) %>%
  .[["n_trees"]]

print(str_c("Number of trees encountered: ", n_trees_p1))

#---Part 2---

# Let's wrap the above into a function!
counting_trees <- function(map, start, n_right, n_down){

  n_rep <- ceiling(nrow(map)/str_count(map$map[1]) * n_right)

  expanded_map <-
    map %>%
    dplyr::mutate(expanded_map = str_dup(map, n_rep)) %>%
    dplyr::select(map = expanded_map)

  # Extract relevant rows using n_down
  expanded_map <-
    expanded_map[seq(nrow(expanded_map), from = start, by = n_down), ]

  map_list <- vector(mode = "list", length = nrow(expanded_map))

  for(i in 1:nrow(expanded_map)){

    pos_to_extract <- start + (n_right * (i-1))

    map_list[[i]] <-
      expanded_map[i,] %>%
      dplyr::mutate(map_entry = str_sub(map, pos_to_extract, pos_to_extract))

  }

  n_trees <-
    map_list %>%
    qdapTools::list_df2df(col1 = "X1") %>%
    dplyr::select(map_entry) %>%
    dplyr::mutate(tree = case_when(map_entry == "#" ~ 1,
                                   map_entry == "." ~ 0)) %>%
    dplyr::summarise(n_trees = sum(tree)) %>%
    .[["n_trees"]]

  return(n_trees)

}

# Create data frame with various combinations
combinations <- tibble(n_right = c(1,3,5,7,1),
                       n_down = c(1,1,1,1,2))

# Create empty vector
n_trees_p2 <- vector(mode = "numeric", length = nrow(combinations))

# Run loop with count tree function
for(i in 1:nrow(combinations)){

  n_trees_p2[i] <-
    counting_trees(map = map,
                   start = 1,
                   n_right = combinations$n_right[i],
                   n_down = combinations$n_down[i])

}

print(str_c("Product of the number of trees encountered: ", prod(n_trees_p2)))

