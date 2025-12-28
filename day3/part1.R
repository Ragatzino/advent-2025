setwd("day3/")
banks <- readLines("puzzle-input.txt")
library(stringr)
find_biggest_number_and_index_in_line <- function(line_str,remove_number = -1,init_index = 1,is_first_digit=TRUE){
  characters <- strsplit(line_str,"")[[1]]
  biggest_number <- 0
  index <- init_index
  max_index <- 1
  for(char in characters){
    char_num <- as.numeric(char)
    if (char_num != remove_number){
      if (char_num > biggest_number){
        biggest_number <- char_num
        max_index<-index
      }
      
    }
    index <- index + 1
    
  }
  # case last character is the highest => we restart
  if (max_index == length(characters) && is_first_digit){
    return(find_biggest_number_and_index_in_line(line_str = line_str, remove_number = biggest_number, init_index = 1))
  }
  return (list(biggest_number=biggest_number,max_index=max_index))
}

get_max_number_of_line <- function(line){
  first_digit_number_index <- find_biggest_number_and_index_in_line(line)
  subline_after_index <- str_sub_all(line, start = first_digit_number_index$max_index+1, end = -1L)[[1]]
  second_digit_number_index <- find_biggest_number_and_index_in_line(subline_after_index, init_index = first_digit_number_index$max_index +1, is_first_digit=FALSE)
  first_digit <- unname(first_digit_number_index$biggest_number)
  second_digit <- unname(second_digit_number_index$biggest_number)
  return (first_digit*10 + second_digit)
}
get_max_number_of_line(line = banks[3])
joltage_per_bank <- sapply(banks, get_max_number_of_line)
print("total number of jolts :")
sum(unname(joltage_per_bank))      
