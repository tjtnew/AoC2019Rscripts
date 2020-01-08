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
vis <- TRUE # set false if you do not want the vis
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
state$input[1] <- 2
state$value <- NULL

finished = FALSE
codes <- NULL

# base R version of Adam Gruer's vis (https://twitter.com/AdamGruer/status/1205677788541247488)
# colours from hyperBubble_palette (https://github.com/moldach/vapoRwave)
bg_col <- "#852942"
wall_col <- "#96BDE4" 
block_col <- "#F599A6"
paddle_col <- "#ECE976"
ball_col <- "#44B05B"

# coordinates for initial layout
plot_coords <- instructions_to_dat(instructions)

# initialise score
score = 0

if (vis) {
    # start x11 window with no buffering (RStudio's built in plot buffers badly)
    x11(bg = bg_col, type = "nbcairo", height = 5)
    
    # create empty plot with correct dimensions
    plot(plot_coords[,1:2], type="n", bty = "n", 
         xaxt = "n", yaxt="n", xlab = "", ylab = "", asp = 1)
    
    # plot initial layout of the screen
    points(plot_coords[,c("x","y")], 
           pch = 15, 
           col = c(bg_col, wall_col, block_col, paddle_col, ball_col)[plot_coords$z + 1],
           cex = 1.25)
    
    # DO NOT CHANGE GRAPH DIMENSIONS AFTER THIS STEP
    # hack - to stop the title shadowing when we update the plot I draw a 
    # rectangle over the top on of my previous title 
    # (see https://stackoverflow.com/a/35527994). Not necessary if using ggplot
    # IF GRAPH DIMENSIONS CHANGE TITLE MAY SHADOW
    coord <- par("usr")
    y_mid <- par("mai")[3] / 2
    height <- 0.5
    conv <- diff(grconvertY(y = 0:1, from = "inches", to = "user"))
    
}

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
    
    if (vis) {
        # remove score from plot coords
        plot_coords <- plot_coords[!(plot_coords$x == -1 & plot_coords$y == 0), ]
        
        # plot rectangle over title
        rect(xleft = coord[1],
             xright = coord[2],
             ybottom = coord[4] + (y_mid * (1 - height) * conv) - 1,
             ytop = coord[4] + (y_mid * (1 + height) * conv),
             xpd = TRUE, border = NA,
             col="#852942")
        
        # plot changes on graph
        points(plot_coords[,c("x","y")], 
               pch = 15, 
               col = c(bg_col, wall_col, block_col, paddle_col, ball_col)[plot_coords$z + 1],
               cex = 1.25)
        
        # update title
        title(paste("Score:", score), col.main = paddle_col, cex.main = 3)    
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
    
    # if printing to screen slow the game down
    if (vis) {
        Sys.sleep(0.01)    
    }
}

# answer
score
