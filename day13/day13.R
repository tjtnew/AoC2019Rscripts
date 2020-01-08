# load data ---------------------------------------------------------------
input <- "day13/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)

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
    state$all_output <- output
    state
}

# function to put plotting coordinates and values in to dataframe
instructions_to_dat <- function(x) {
    tmp <- split(x, (seq_along(x) - 1) %% 3 + 1)
    data.frame(x = tmp[[1]],
               y = -tmp[[2]], # flip for screen
               z = tmp[[3]])
}

# part one ----------------------------------------------------------------
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
instructions <- run_intcode(state)$all_output
sum(instructions[c(FALSE, FALSE, TRUE)] == 2)

# part two ----------------------------------------------------------------
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
state$input[1] <- 2
state$value <- NULL

finished = FALSE
codes <- NULL

# initialise score
score = 0

while(!finished) {
    state <- intcode(state)
    
    # get information on how the game as progessed
    while (state$io == "out") {
        finished <- state$finished
        if (finished) break
        codes <- c(codes, state$output)
        state <- intcode(state)
    }
    
    # put info in data frame
    plot_coords <- instructions_to_dat(codes)
    
    # update score
    tmp <- plot_coords[plot_coords$x == -1 & plot_coords$y == 0, 3]
    if (length(tmp) == 1) {
        score <- tmp
    }
    
    finished <- state$finished
    if (finished) break
    
    # play the game automatically
    codes <- NULL
    ball_pos_x <- plot_coords$x[which(plot_coords$z == 4)]
    paddle_idx <- which(plot_coords$z == 3)
    paddle_pos_x <- plot_coords$x[paddle_idx]
    paddle_pos_y <- plot_coords$y[paddle_idx]
    
    if (ball_pos_x < paddle_pos_x) {
        state$value <- -1
    } else if (ball_pos_x > paddle_pos_x) {
        state$value <- 1
    } else {
        state$value <- 0
        codes <- c(paddle_pos_x, paddle_pos_y, 3)
    }
}

# answer
score
