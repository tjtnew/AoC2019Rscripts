# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day13/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)

# part one ----------------------------------------------------------------
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
instructions <- single_intcode_computer(state)$all_output
tmp <- split(instructions, ceiling(seq_along(instructions)/3))
sum(unlist(lapply(tmp, `[`, 3)) == 2)

# part two ----------------------------------------------------------------
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
state$input[1] <- 2
state$value <- NULL

finished = FALSE
codes <- NULL

while(!finished) {
    state <- generic_intcode_computer(state)
    
    while (state$io == "out") {
        finished <- state$finished
        if (finished) break
        codes <- c(codes, state$output)
        state <- generic_intcode_computer(state)
    }
    
    finished <- state$finished
    if (finished) break
    
    tmp <- split(codes, ceiling(seq_along(codes)/3))
    codes <- NULL
    x <- unlist(lapply(tmp, `[`, 1))
    y <- unlist(lapply(tmp, `[`, 2))
    z <- unlist(lapply(tmp, `[`, 3))
    
    ball_pos <- which(z == 4)
    ball_pos <- x[ball_pos]
    
    paddle_pos <- which(z == 3)
    paddle_pos <- x[paddle_pos]
    paddle_pos_y <- y[paddle_pos]
    
    if (ball_pos < paddle_pos) {
        state$value <- -1
    } else if (ball_pos > paddle_pos) {
        state$value <- 1
    } else {
        state$value <- 0
        codes <- c(paddle_pos, paddle_pos_y, 3)
    }
}

# answer
codes[length(codes)]









