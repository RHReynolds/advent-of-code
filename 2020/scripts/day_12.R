#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

instructions <-
  readr::read_delim(here::here("2020", "raw_data", "day_12.txt"),
                    delim = " ",
                    col_names = "instruction")


instructions_ex <-
  tibble(instruction = c(
    "F10", "N3", "F7", "R90", "F11"))

#---Main-------------------------------------------------------

#---Part 1-----------------------------------------------------

instructions_tidy <-
  # instructions_ex %>%
  instructions %>%
  dplyr::mutate(direction = str_extract(instruction, "[aA-zZ]+"),
                value = readr::parse_number(instruction))

# Function to update facing depending on command
update_facing <- function(facing, direction, value){

  compass <-
    tibble(poles = c("N", "E", "S", "W"),
           degrees = c(0, 90, 180, 270))

  if(direction %in% c("L", "R")){

    facing_deg <-
      compass %>%
      dplyr::filter(poles == facing)

    if(direction == "L"){

      updated_degrees <- facing_deg$degrees - value

    }

    if(direction == "R"){

      updated_degrees <- facing_deg$degrees + value

    }

    # The above can result in negative or values above 360
    # Need to modify these such that can filter compass tibble
    if(updated_degrees < 0){

      updated_degrees <- 360 - abs(updated_degrees)

    }

    if(updated_degrees >= 360){

      updated_degrees <- updated_degrees - 360

    }

    new_facing <-
      compass %>%
      dplyr::filter(degrees == updated_degrees) %>%
      .[["poles"]]

  }

  if(direction %in% c("F", compass$poles)){

    new_facing <- facing

  }

  return(new_facing)

}

update_position <- function(position, facing, direction, value){

  if(direction %in% c("L", "R")){

    updated_position <- position

  }

  if(direction == "F"){

    updated_position <-
      position %>%
      dplyr::mutate(units = case_when(poles == facing ~ units + value,
                                      TRUE ~ units))

  }

  if(direction %in% position$poles){

    updated_position <-
      position %>%
      dplyr::mutate(units = case_when(poles == direction ~ units + value,
                                      TRUE ~ units))

  }

  return(updated_position)

}

# Run loop to determine position
facing <- "E"
position <-
  tibble(poles = c("N", "E", "S", "W"),
         units = c(0, 0, 0, 0),
         group = c("NS", "EW", "NS", "EW"))

for(i in 1:nrow(instructions_tidy)){

  facing <-
    update_facing(facing = facing,
                  direction = instructions_tidy$direction[i],
                  value = instructions_tidy$value[i])

  position <-
    update_position(position = position,
                    facing = facing,
                    direction = instructions_tidy$direction[i],
                    value = instructions_tidy$value[i])

}

ans_p1 <-
  position %>%
  dplyr::mutate(units = case_when(poles %in% c("S", "W") ~ -units,
                                  TRUE ~ units)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(group_sum = sum(units)) %>%
  dplyr::summarise(dist = sum(abs(group_sum))) %>%
  .[["dist"]]

print(str_c("Manhattan distance is: ", ans_p1))


#---Part 2-----------------------------------------------------

update_waypoint <- function(waypoint, direction, value){

  compass <-
    tibble(poles = c("N", "E", "S", "W"),
           degrees = c(0, 90, 180, 270))

  waypoint <-
    waypoint %>%
    dplyr::inner_join(compass, by = "poles")

  if(direction %in% c("L", "R")){

    updated_waypoint <-
      waypoint %>%
      dplyr::filter(units != 0) %>%
      dplyr::select(-poles)

    if(direction == "L"){

      updated_waypoint <-
        updated_waypoint %>%
        dplyr::mutate(updated_degrees = degrees - value)

    }

    if(direction == "R"){

      updated_waypoint <-
        updated_waypoint %>%
        dplyr::mutate(updated_degrees = degrees + value)

    }

    # The above can result in negative or values above 360
    # Need to modify these such that can filter compass tibble
    updated_waypoint <-
      updated_waypoint %>%
      dplyr::mutate(updated_degrees =
                      case_when(updated_degrees < 0 ~ 360 - abs(updated_degrees),
                                updated_degrees >= 360 ~ updated_degrees - 360,
                                TRUE ~ updated_degrees)) %>%
      dplyr::select(units, updated_degrees)

    updated_waypoint <-
      compass %>%
      dplyr::left_join(updated_waypoint , by = c("degrees" = "updated_degrees")) %>%
      tidyr::replace_na(list(units = 0))

  }

  if(direction %in% c(compass$poles)){

    updated_waypoint <-
      waypoint %>%
      dplyr::mutate(units = case_when(poles == direction ~ units + value,
                                      TRUE ~ units))

  }

  if(direction == "F"){

    updated_waypoint <- waypoint

  }

  updated_waypoint <-
    updated_waypoint %>%
    dplyr::select(poles, units)

  return(updated_waypoint)

}

update_position <- function(position, waypoint, direction, value){

  if(direction == "F"){

    position <-
      position %>%
      dplyr::inner_join(waypoint, by = "poles") %>%
      dplyr::mutate(units = units.x + (units.y * value)) %>%
      dplyr::select(poles, units, group)

  }

  return(position)

}

waypoint <-
  tibble(poles = c("N", "E", "S", "W"),
         units = c(1, 10, 0, 0))

position <-
  tibble(poles = c("N", "E", "S", "W"),
         units = c(0, 0, 0, 0),
         group = c("NS", "EW", "NS", "EW"))

for(i in 1:nrow(instructions_tidy)){

  # print(str_c("Direction: ", instructions_tidy$direction[i], "; value: ", instructions_tidy$value[i]))

  waypoint <-
    update_waypoint(waypoint =  waypoint,
                    direction = instructions_tidy$direction[i],
                    value = instructions_tidy$value[i])

  # print(waypoint)

  position <-
    update_position(position = position,
                    waypoint = waypoint,
                    direction = instructions_tidy$direction[i],
                    value = instructions_tidy$value[i])

  # print(position)

}

ans_p2 <-
  position %>%
  dplyr::mutate(units = case_when(poles %in% c("S", "W") ~ -units,
                                  TRUE ~ units)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(group_sum = sum(units)) %>%
  dplyr::summarise(dist = sum(abs(group_sum))) %>%
  .[["dist"]]

print(str_c("Manhattan distance is: ", ans_p2))
