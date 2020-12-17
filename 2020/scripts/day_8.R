#---Load libraries---------------------------------------------
library(tidyverse)
library(stringr)

#---Load data--------------------------------------------------

boot_code <-
  readr::read_lines(here::here("2020", "raw_data", "day_8.txt"),
                    skip_empty_rows = FALSE)

#---Main-------------------------------------------------------

#---Part 1---

# Start by tidying instructions
# Add column to identify next operation_id
boot_code_tidy <-
  tibble(instructions = boot_code) %>%
  tidyr::separate(col = "instructions", into = c("operation", "argument"), sep = " ", remove = FALSE) %>%
  dplyr::mutate(argument = as.integer(argument),
                operation_id = row_number(),
                next_operation_id = case_when(operation == "jmp" ~ operation_id + argument,
                                              TRUE ~ operation_id + 1L))

# This is going to be another while loop
get_accumulator_value <- function(boot_code_tidy){

  acc_value <- 0
  current_id <- 1
  error = FALSE
  visited_instructions <- tibble()

  while(error == FALSE){

    current_instruction <-
      boot_code_tidy %>%
      dplyr::filter(operation_id == current_id)

    visited_instructions <-
      visited_instructions %>%
      dplyr::bind_rows(current_instruction)

    if(current_instruction$operation == "acc"){

      acc_value <- acc_value + current_instruction$argument

    }

    current_id <- current_instruction$next_operation_id

    if(current_id %in% visited_instructions$operation_id){

      error <- TRUE

    }

  }

  return(acc_value)

}

ans_p1 <- get_accumulator_value(boot_code_tidy)

print(str_c("The accumulator value is: ", ans_p1))

#---Part 2---

# Re-visit function to add a termination status i.e. pass or fail
get_accumulator_value <- function(boot_code_tidy){

  acc_value <- 0
  current_id <- 1
  error = FALSE
  visited_instructions <- tibble()
  termination_status <- "fail"

  while(error == FALSE){

    current_instruction <-
      boot_code_tidy %>%
      dplyr::filter(operation_id == current_id)

    visited_instructions <-
      visited_instructions %>%
      dplyr::bind_rows(current_instruction)

    if(current_instruction$operation == "acc"){

      acc_value <- acc_value + current_instruction$argument

    }

    current_id <- current_instruction$next_operation_id

    if(current_id %in% visited_instructions$operation_id){

      error <- TRUE
      termination_status <- "fail"

    }

    if(current_id == nrow(boot_code_tidy) + 1){

      # Still need to kill the loop, even if program successfully ran
      error <- TRUE
      termination_status <- "pass"

    }

  }

  return(tibble(acc_value = acc_value,
                operation_id = current_id - 1,
                termination_status = termination_status))

}

# Now we need to build a function to switch nop/jmp and try each combo
find_passing_boot_code <- function(boot_code_tidy){

  jmp_nop_df <-
    boot_code_tidy %>%
    dplyr::filter(operation %in% c("jmp", "nop")) %>%
    dplyr::mutate(new_operation = case_when(operation == "jmp" ~ "nop",
                                            operation == "nop" ~ "jmp"))

  termination_df <- tibble(acc_value = 0,
                           operation_id = 0,
                           termination_status = "fail")
  index <- 1

  while(termination_df$termination_status == "fail" &
        index <= nrow(jmp_nop_df)){

    # Extract necessary row from jmp_nop_df
    current_jmp_nop <- jmp_nop_df[index, ]

    # Update next operation id based on new operation
    # Run scenario
    altered_boot_code <-
      boot_code_tidy %>%
      dplyr::mutate(operation = case_when(operation_id == current_jmp_nop$operation_id ~ current_jmp_nop$new_operation,
                                          TRUE ~ operation),
                    next_operation_id = case_when(operation == "jmp" ~ operation_id + argument,
                                                  TRUE ~ operation_id + 1L))

    termination_df <- get_accumulator_value(altered_boot_code)

    # Update index
    index <- index + 1

  }

  return(termination_df)

}

ans_p2 <- find_passing_boot_code(boot_code_tidy)

print(str_c("The accumulator value for a passing run is: ", ans_p2$acc_value))
