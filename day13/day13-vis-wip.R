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

# colours from hyperBubble_palette (https://github.com/moldach/vapoRwave)
bg_col <- "#852942"
wall_col <- "#96BDE4" 
block_col <- "#F599A6"
paddle_col <- "#ECE976"
ball_col <- "#44B05B"

plot_coords <- split(instructions, (seq_along(instructions) - 1) %% 3 + 1)
plot_coords <- data.frame(x = plot_coords[[1]],
                          y = -plot_coords[[2]],
                          z = plot_coords[[3]])

x11(bg = bg_col, type = "nbcairo", height = 5)
plot(plot_coords[,1:2], type="n", xaxt = "n", yaxt="n", asp = 1,
     xlab = "", ylab = "", bty = "n")
points(plot_coords[,c("x","y")], 
       pch = 15, 
       col = c(bg_col, wall_col, block_col, paddle_col, ball_col)[plot_coords$z + 1],
       cex = 1.25)

# to stop the title shadowing when we update the plot I draw a rectangle over 
# the top on of my previous title (see https://stackoverflow.com/a/35527994)
# if using something like ggplot this would not be necessary
coord <- par("usr")
y_mid <- par("mai")[3] / 2
height <- 0.5
conv <- diff(grconvertY(y = 0:1, from = "inches", to = "user"))

while(!finished) {
    state <- intcode(state)
    
    while (state$io == "out") {
        finished <- state$finished
        if (finished) break
        codes <- c(codes, state$output)
        state <- intcode(state)
    }
    
    plot_coords <- split(codes, (seq_along(codes) - 1) %% 3 + 1)
    x <- plot_coords[[1]]
    y <- plot_coords[[2]]
    z <- plot_coords[[3]]
    plot_coords <- data.frame(x = x, y = -y, z = z)
    tmp <- plot_coords[plot_coords$x == -1 & plot_coords$y == 0, 3]
    if (length(tmp) == 1) {
        score <- tmp
    }
    
    plot_coords <- plot_coords[!(plot_coords$x == -1 & plot_coords$y == 0), ]
    rect(xleft = coord[1],
         xright = coord[2],
         ybottom = coord[4] + (y_mid * (1 - height) * conv) - 1,
         ytop = coord[4] + (y_mid * (1 + height) * conv),
         xpd = TRUE, border = NA,
         col="#852942")
    points(plot_coords[,c("x","y")], 
           pch = 15, 
           col = c(bg_col, wall_col, block_col, paddle_col, ball_col)[plot_coords$z + 1],
           cex = 1.25)
    title(paste("Score:", score), col.main = paddle_col, cex.main = 3)
    
    finished <- state$finished
    if (finished) break
    
    codes <- NULL
    
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
    Sys.sleep(0.01)
}

# answer
score
