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
    list(dial=dial,new_dial_flat=new_dial)
}
compute_clicks <- function(dial, new_dial) {
  DIAL_MODULO <- 100
  # Calcul des quotients et restes
  before_quotient_hor <- dial %/% DIAL_MODULO
  before_quotient_anti <- (dial -1) %/% DIAL_MODULO
  after_quotient_hor  <- new_dial %/% DIAL_MODULO
  after_quotient_anti <- (new_dial -1 )%/% DIAL_MODULO
  remainder <- new_dial %% DIAL_MODULO
  clicks <- 0
  if (new_dial >0) {
    clicks <- after_quotient_anti - before_quotient_hor
  }else{
    clicks <- before_quotient_anti - after_quotient_hor
  }
  list(clicks=clicks, remainder=remainder)
}
# Applique un mouvement au cadran
move_dial <- function(dial, move, zero_crossings) {
  dial_and_newpos <- compute_moves(dial, move)
  clicks_and_new_dial <- compute_clicks(dial_and_newpos$dial, dial_and_newpos$new_dial_flat)

  message(sprintf(
    "move=%s | before=%d | after=%d | dial=%d | zero_crossings_before=%d | zero_crossings=%d",
    move, dial, dial_and_newpos$new_dial_flat, clicks_and_new_dial$remainder, zero_crossings, zero_crossings + clicks_and_new_dial$clicks
  ))

  list(
    dial = clicks_and_new_dial$remainder,
    zero_crossings = zero_crossings + clicks_and_new_dial$clicks
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
