# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day19/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)


# part one ----------------------------------------------------------------
coordinates <- expand.grid(1:50,1:50)

is_working <- function(coordinates, input) {
    #initialise state for intcode
    state <- list()
    state$relative_base <- 0
    state$index <- 1
    state$input <- input
    
    # coordinates to check
    x <- coordinates[[1]]
    y <- coordinates[[2]]
    
    # check both
    state$value <- x - 1
    state <- generic_intcode_computer(state)
    state$value <- y - 1
    state <- generic_intcode_computer(state)
    
    # return result
    state$output
}

res <- apply(coordinates, 1, is_working, input)

# part one answer
sum(res)

# part two ----------------------------------------------------------------

# first plot the beam so we can see what is happening
plot(coordinates[,1], coordinates[, 2], 
     col = c("black","black")[res + 1],
     bg = c("white", 7)[res + 1],
     pch = c(22, 22)[res + 1], 
     asp = 1, cex = 1.5, 
     xlim=c(0,50))

# Working below works on the assumption that the the beam follows the same
# spread patternt that we see in the above graph

# function to track the lower right side of the beam
track_beam <- function(coords, input) {
    x <- coords[1]
    y <- coords[2]
    right <- c(x + 1, y)
    up <- c(x , y + 1)
    up_right <- c(x+1 , y + 1)
    if (is_working(right, input)) {
        return(right)
    } else if (is_working(up, input)) {
        return(up)
    } else if (is_working(up_right, input)) {
        return(up_right)
    } else {
        stop("lost beam")
    }
}

# function to test whether other corners of the beam are in the square.  I'm
# not testing the initial coordinate as assuming that this has come from the
# output of the track beam function
test_square <- function(coords, input) {
    x <- coords[1]
    y <- coords[2]
    
    bl <- c(x - 99, y)
    tr <- c(x , y + 99)
    tl <- c(x - 99, y + 99)
    
    if(is_working(bl, input) && is_working(tr, input) && is_working(tl, input)) {
        TRUE
    } else {
        FALSE
    }
}

# we start our loop from the second point away from the beam as it is
# continuous (atleast diagonally after this point)
points_x <- coordinates[, 1][res == 1]
points_y <- coordinates[, 2][res == 1]
coords <- c(points_x[2], points_y[2])

found_square <- FALSE
while (!found_square) {
    coords <- track_beam(coords, input)
    found_square <- test_square(coords, input)
}

# find the bottom left corner of the square
square_x <- coords[1] - 1 - 99
square_y <- coords[2] - 1

# answer part two
10000 * square_x + square_y
