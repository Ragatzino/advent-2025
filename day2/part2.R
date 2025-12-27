library(stringr)
puzzle_input <- readLines("day2/puzzle-input.txt")
splitted_input <- unlist(str_split(puzzle_input,","))
clean_splitted <- Filter(function(x) x != "", splitted_input)
get_all_numbers_of_range <- function(range_str) {
  parts <- str_split(range_str, "-")[[1]]
  if (length(parts) != 2) stop("Invalid range")
  range_init <- as.numeric(parts[1])
  range_end <- as.numeric(parts[2])
  return(as.character(range_init:range_end))
}

# Check if a number string is composed of repeated patterns
is_repeated <- function(s) {
  if (substr(s, 1, 1) == "0") return(FALSE)
  # Regex explanation:
  # ^([0-9]+)\1+$ -> starts with some digits, then that same group repeats until the end
  return(grepl("^([0-9]+)\\1+$", s))
}

# Generate all numbers from all ranges
all_numbers <- unlist(lapply(clean_splitted, get_all_numbers_of_range))

# Filter numbers that have repeated patterns and sum them
repeated_numbers <- as.numeric(all_numbers[sapply(all_numbers, is_repeated)])
total_sum <- sum(repeated_numbers)
print(total_sum)
