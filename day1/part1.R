setwd("day1/")
puzzle_input <- readLines("puzzle-input.txt")
print(puzzle_input)
dial_start <- 50
is_valid_move <- function(move){
    grepl("^[RL][0-9]+$",move)
}
valids <- sapply(puzzle_input, is_valid_move)
all(valids)
move_dial <- function(dial, move, number_of_zeros=0) {
    direction <- substr(move, 1, 1)
    value <- substr(move, 2, nchar(move))
    if (direction == "R") {
        dial <- (dial + as.numeric(value)) %% 100
        if (dial == 0) {
            number_of_zeros <- number_of_zeros + 1
        }
    }
    if (direction == "L") {
        dial <- (dial - as.numeric(value)) %% 100
        if (dial == 0) {
            number_of_zeros <- number_of_zeros + 1
        }
    }
    list(
        dial = dial,
        number_of_zeros = number_of_zeros
    )
}
number_of_zeros <- 0
for (move in puzzle_input) {
    result <- move_dial(dial_start, move, number_of_zeros)
    dial_start <- result$dial
    number_of_zeros <- result$number_of_zeros
}
print(number_of_zeros)