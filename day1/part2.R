# --- Configuration -----------------------------------------------------------
setwd("day1/")


args <- commandArgs(trailingOnly = TRUE)

choice <- args[1]

input_file <- switch(choice,
                     "1" = "puzzle-input.txt",
                     "2" = "test-input.txt",
                     stop("Choix invalide. Tape 1 ou 2.")
)

puzzle_input <- readLines(input_file)

# --- Constantes ---------------------------------------------------------------
dial_start <- 50
DIAL_MODULO <- 100
dial <- 50
zero_crossings <- 0

# --- Fonctions ----------------------------------------------------------------

is_valid_move <- function(move){
    grepl("^[RL][0-9]+$",move)
}

parse_move <- function(move) {
    direction <- substr(move, 1, 1)
    value <- as.numeric(substr(move, 2, nchar(move)))
  list(
    direction = direction,
    value = value
  )
}

compute_moves <- function(dial, move) {
    m <- parse_move(move)
    new_dial <- if (m$direction == "R") {
        dial + m$value
    } else {
        dial - m$value
    }
    return(dial,new_dial)
}
compute_clicks <- function(dial, new_dial) {
  # Calcul des quotients et restes
  before_q <- dial %/% DIAL_MODULO
  after_q  <- new_dial %/% DIAL_MODULO
  remainder <- new_dial %% DIAL_MODULO
  
}
# Applique un mouvement au cadran
move_dial <- function(dial, move, zero_crossings) {
  dial_and_new <- compute_moves(dial, move)

  # Mise à jour du cadran (entre 0 et 99)
  dial_mod <- abs(remainder)
  zero_crossing_before=zero_crossings

  # Détection des passages par un multiple de 100
  if (before_q != after_q) {
    zero_crossings <- zero_crossings + abs(after_q - before_q)
  } else if (remainder == 0) {
    zero_crossings <- zero_crossings + abs(after_q - before_q) 
  }

  message(sprintf(
    "move=%s | before=%d | after=%d | dial=%d | before_q=%d | modif_zero=%d | zero_crossings_before=%d | zero_crossings=%d",
    move, dial, new_dial, dial_mod, before_q, after_q, zero_crossing_before, zero_crossings
  ))

  list(
    dial = dial_mod,
    zero_crossings = zero_crossings
  )
}

# --- Validation inputs
stopifnot(all(sapply(puzzle_input, is_valid_move)))

# --- Résolution 
 
 message(sprintf("Début des mouvements : %s", dial_start))


for (move in puzzle_input) {
  result <- move_dial(dial, move, zero_crossings)
  dial <- result$dial
  zero_crossings <- result$zero_crossings
}

print(zero_crossings)
