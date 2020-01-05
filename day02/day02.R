# load data ---------------------------------------------------------------
input <- "day02/input"
input <- unlist(read.csv(input, header = FALSE), use.names = FALSE)

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)

# default state settings
state <- list()
state$value <- 0
state$relative_base <- 0
state$index <- 1
state$input <- input

# run the intcode computer with no additional input
run_intcode <- function(state) {
    finished = FALSE
    output <- NULL
    while(!finished) {
        state <- intcode(state)
        finished <- state$finished
        if(!finished) {
            output <- c(output, state$output)
        }
    }
    state
}


# part 1 ------------------------------------------------------------------
# set the initial state
# remember R indexes from 1 but AoC using index from 0 for this problem
tmp <- state
tmp$input[2] <- 12
tmp$input[3] <- 2

# calculate the value in position 0 (index 1) after intcode_computer halts
intcode(tmp)$input[1]


# part two ----------------------------------------------------------------

# noun / verb combos
possible_pairs <- expand.grid(0:99, 0:99)

# initial setup
output <- -1
index <- 1
target <- 19690720

# continue until output is the desired value
while (output != target) {
    tmp <- state
    pair <- possible_pairs[index, ]
    noun <- pair[ , 1]
    verb <- pair[ , 2]
    tmp$input[2] <- noun
    tmp$input[3] <- verb
    output <- intcode(tmp)$input[1]
    index <- index + 1
}

# What is 100 * noun + verb?
100 * noun + verb