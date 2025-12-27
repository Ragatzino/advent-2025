library(stringr)
puzzle_input <- readLines("day2/puzzle-input.txt")
splitted_input <- unlist(str_split(puzzle_input,","))
clean_splitted <- Filter(function(x) x != "", splitted_input)
get_all_character_of_range <- function(range_str){
  print(range_str)
  parts <- str_split(range_str, "-")[[1]]
  if (length(parts) != 2) {
    stop("range_str is not a str with an unique '-' inside")
  }
  range_init <- as.numeric(parts[1])
  range_end <- as.numeric(parts[2])
  return(lapply(range_init:range_end,as.character))
}
repeated_pattern <- function(number_str){
  taille <- nchar(number_str)
  if (taille %% 2 != 0){
    return(0)
  }
  if (number_str[1] == "0"){
    return(0)
  }
  before_number_str <- substr(number_str,1,taille/2)
  after_number_str <- substr(number_str,taille/2 + 1, taille)
  if (before_number_str == after_number_str){
    return(as.numeric(number_str))
  }
  return(0)
}
print(clean_splitted)
characters_of_range_for_each_range <- lapply(clean_splitted,get_all_character_of_range)
computed_matches <- lapply(characters_of_range_for_each_range, function(sublist) { lapply(sublist,repeated_pattern)})
sum_each <- lapply(computed_matches, function(sublist) sum(unlist(sublist)))
total_sum <- Reduce(`+`, sum_each)      
print(total_sum)
