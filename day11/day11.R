# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day11/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# source intcode computer (copied from day 09) ----------------------------
source("~/projects/AoC2019Rscripts/day11/intcode.R")

# helper functions --------------------------------------------------------
# calculate new coordinates based on current direction and rotation
new_coordinates <- function(coords, direction, rotation) {
    switch(direction,
           N = data.frame(x = coords$x + (-1)^(rotation + 1), y = coords$y),
           W = data.frame(x = coords$x, y = coords$y + (-1)^(rotation + 1)),
           S = data.frame(x = coords$x + (-1)^rotation, y = coords$y),
           E = data.frame(x = coords$x, y = coords$y + (-1)^rotation),
           stop("something went wrong in anti-clockwise rotation"))
}

# calculate new directions based on current direction and rotation
new_direction <- function(direction, rotation) {
    directions <- c("N", "E", "S", "W")
    index <- which(directions == direction)
    move <- (-1)^(rotation + 1)
    index <- ((index + move - 1) %% 4) + 1
    directions[index]
}

# concatanate coordinates in to string
string_coordinate <- function(coordinate) {
    paste0(coordinate$x, "x", coordinate$y)
}

# robot function
run_robot<- function(starting_colour) {
    # initial coordinates (origin) and direction (N)
    current_coordinates <- data.frame(x = 0, y = 0)
    direction <- "N"
    
    # keep track of painted panels
    black <- character()
    white <- character()
    
    # add origin to black panels
    black <- c(black, string_coordinate(current_coordinates))
    
    # initialise intcode computer
    state <- list()
    state$relative_base <- 0
    state$index <- 1
    state$input <- input
    state$value <- starting_colour
    
    finished = FALSE
    while(!finished) {
        # run intcode computer twice to get both values
        state <- generic_intcode_computer(state)
        state <- generic_intcode_computer(state)
        finished <- state$finished
        
        # get colour and rotation
        tmp <- tail(state$output, 2)
        colour <- tmp[1]
        rotation <- tmp[2]
        
        # paint squares and remove from other list
        tmp <- string_coordinate(current_coordinates)
        if (colour) {
            white <- c(white, tmp)
            if (tmp %in% black) {
                black <- black[!(black %in% tmp)]
            }
        } else {
            black <- c(black, tmp)
            if (tmp %in% white) {
                white <- white[!(white %in% tmp)]
            }
        }
        
        # calculate new_cordinates
        current_coordinates <- new_coordinates(current_coordinates,
                                               direction,
                                               rotation)
        
        # calculate new direction
        direction <- new_direction(direction, rotation)
        
        # check if new position already seen and take camera shot
        tmp <- string_coordinate(current_coordinates)
        if (tmp %in% white) {
            state$value <- 1
        } else {
            state$value <- 0
        }
    }
    return(list(black = unique(black),
                white = unique(white)))
}

# part one ----------------------------------------------------------------
result <- run_robot(0)
length(result$black) + length(result$white)

# part two ----------------------------------------------------------------
result <- run_robot(1)
white <- strsplit(result$white, "x")
white <- lapply(white, as.integer)
x <- unlist(lapply(white, `[`, 1))
y <- unlist(lapply(white, `[`, 2))
plot(x,y,pch=15, asp = 1, cex=2)
