setwd("day1/")
puzzle_input <- readLines("puzzle-input.txt")
print(puzzle_input)
dial_start <- 50
is_valid_move <- function(move){
    grepl("^[RL][0-9]+$",move)
}
valids <- sapply(puzzle_input, is_valid_move)
all(valids)
move_dial <- function(dial, move, number_of_quotient_zero=0) {
    direction <- substr(move, 1, 1)
    value <- substr(move, 2, nchar(move))

    if (direction == "R") {
        before <- dial %/% 100
        after <- (dial + as.numeric(value)) %/% 100
        reste <- (dial + as.numeric(value)) %% 100
        print(paste("before:", before, "dial:",dial, "afterdial:", abs((dial + as.numeric(value)) %% 100), "after:", after, "move:",move, "number_of_quotient_zero:", number_of_quotient_zero))
        dial <- abs((dial + as.numeric(value)) %% 100)
        if (before != after) {
            number_of_quotient_zero <- number_of_quotient_zero + abs(after - before)
        }
        else {
        if (reste == 0) {
                number_of_quotient_zero <- number_of_quotient_zero + 1
                print(paste("reste:",0, "number_of_quotient_zero:", number_of_quotient_zero))
            }
        }
    }
    if (direction == "L") {
        before <- dial %/% 100
        after <- (dial - as.numeric(value)) %/% 100
        reste <- (dial - as.numeric(value)) %% 100
        print(paste("before:", before, "dial:",dial, "afterdial:", abs((dial - as.numeric(value)) %% 100), "after:", after, "move:",move, "number_of_quotient_zero:", number_of_quotient_zero))
        dial <- abs((dial - as.numeric(value)) %% 100)
        if (before != after) {
            number_of_quotient_zero <- number_of_quotient_zero + abs(after - before)
        }
        else {
            if (reste == 0) {
                number_of_quotient_zero <- number_of_quotient_zero + 1
                print(paste("reste:",0, "number_of_quotient_zero:", number_of_quotient_zero))
            }
        }
    }
    list(
        dial = dial,
        number_of_quotient_zeros = number_of_quotient_zero
    )
}
number_of_quotient_zeros <- 0
for (move in puzzle_input) {
    print(dial_start)
    result <- move_dial(dial_start, move, number_of_quotient_zeros)
    dial_start <- result$dial
    number_of_quotient_zeros <- result$number_of_quotient_zeros
}
print(number_of_quotient_zeros)