# load data ---------------------------------------------------------------
input <- "day05/input"
input <- unlist(read.csv(input, header = FALSE), use.names = FALSE)

# load intcode computer
library(intcode)

# run the intcode computer with no additional input
run_intcode <- function(state) {
    finished = FALSE
    output <- NULL
    while(!finished) {
        state <- intcode(state)
        finished <- state$finished
    }
    state
}

# default state settings
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input


# part one ----------------------------------------------------------------
tmp <- state
tmp$value <- 1
run_intcode(tmp)$output


# Answer to part two ------------------------------------------------------
tmp <- state
tmp$value <- 5
run_intcode(tmp)$output
