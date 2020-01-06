# load data ---------------------------------------------------------------
input <- "day09/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
library(intcode)

# set a default state
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input

# part one ----------------------------------------------------------------
state$value <- 1
res <- intcode(state)
res$output

# part two ----------------------------------------------------------------
state$value <- 2
res <- intcode(state)
res$output
