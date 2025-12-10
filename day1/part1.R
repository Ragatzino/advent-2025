# --- Configuration -----------------------------------------------------------
setwd("day1/")
puzzle_input <- readLines("puzzle-input.txt")
print(puzzle_input)
dial_start <- 50

# --- Constantes ---------------------------------------------------------------
DIAL_MODULO <- 100
dial <- 50
zero_crossings <- 0

# --- Fonctions ----------------------------------------------------------------

is_valid_move <- function(move){
    grepl("^[RL][0-9]+$",move)
}

parse_move <- function(move) {
  list(
    direction = substr(move, 1, 1),
    value = as.numeric(substr(move, 2, nchar(move)))
  )
}

# Applique un mouvement au cadran
move_dial <- function(dial, move, zero_crossings) {
  m <- parse_move(move)

  # Calcul de la nouvelle position
  new_dial <- if (m$direction == "R") {
    dial + m$value
  } else {
    dial - m$value
  }

  # Calcul du reste
  remainder <- new_dial %% DIAL_MODULO

  # Mise à jour du cadran (entre 0 et 99)
  dial_mod <- abs(remainder)

  # Détection des passages par un multiple de 100
  if (remainder == 0) {
    zero_crossings <- zero_crossings + 1
  }

  # Debug propre (facultatif)
  message(sprintf(
    "move=%s | before=%d | after=%d | dial=%d | dial_after=%d | zero_crossings=%d",
    move, dial, new_dial, dial_mod, after_q, zero_crossings
  ))

  list(
    dial = dial_mod,
    zero_crossings = zero_crossings
  )
}

# --- Validation inputs
stopifnot(all(sapply(puzzle_input, is_valid_move)))

# --- Résolution 

for (move in puzzle_input) {
  result <- move_dial(dial, move, zero_crossings)
  dial <- result$dial
  zero_crossings <- result$zero_crossings
}

print(zero_crossings)
