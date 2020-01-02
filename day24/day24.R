# load data ---------------------------------------------------------------
input <- "day24/input"
instructions <- unlist(strsplit(readLines(input),""))

# put layout in to a grid and add some empty space
layout <- matrix(instructions, nrow = 5, ncol = 5, byrow = TRUE)
layout <- rbind(rep(".", 5), layout, rep(".", 5))
layout <- cbind(rep(".", 7), layout, rep(".", 7))

# function to count number of bugs in adjacent cells
num_bugs <- function(x, y, grid) {
    neighbours <- c(grid[x - 1, y], grid[x + 1, y], 
                    grid[x, y - 1], grid[x, y + 1])
    sum(neighbours == "#")
}

# function to update the layout
update_layout <- function(layout) {
    new_layout <- layout
    for (i in 2:6) {
        for (j in 2:6) {
            if (layout[i, j] == "#") {
                if (num_bugs(i, j, layout) != 1) {
                    new_layout[i, j] = "."
                }    
            } else {
                if (num_bugs(i, j, layout) == 1 || num_bugs(i, j, layout) == 2) {
                    new_layout[i, j] = "#"
                }
            }
        }
    }
    new_layout
}

# function to extract the middle of the grid
extract_layout <- function(grid) {
    paste0(as.character(grid[2:6, 2:6]), collapse = "")
}

# initialise record of layouts
layout_record <- list()
tmp <- extract_layout(layout)
layout_record[[tmp]] <- 1

# loop until a repeated layout is found
repeated = FALSE
while(!repeated) {
    layout <- update_layout(layout)
    tmp <- extract_layout(layout)
    if (length(layout_record[[tmp]]) == 0) {
        layout_record[[tmp]] <- 1
    } else {
        layout_record[[tmp]] <- layout_record[[tmp]] + 1
        repeated = TRUE
    }
}

# part one answer - calculate biodiversity rating
powers <- matrix(2^(0:24), nrow = 5, ncol = 5, byrow = TRUE)
tmp <- layout[2:6, 2:6]
sum(powers[which(tmp == "#", arr.ind = TRUE)])


# part two ----------------------------------------------------------------

up_neighbours <- function(layers, id, x, y) {
    if (x == 1) {
        layers[[id + 1]][2, 3]
    } else if (x == 4 && y == 3) {
        layers[[id - 1]][5, ]
    } else {
        layers[[id]][x - 1, y]
    }
}

down_neighbours <- function(layers, id, x, y) {
    if (x == 5) {
        layers[[id + 1]][4, 3]
    } else if (x == 2 && y == 3) {
        layers[[id - 1]][1, ]
    } else {
        layers[[id]][x + 1, y]
    }
    
}

left_neighbours <- function(layers, id, x, y) {
    if (y == 1) {
        layers[[id + 1]][3, 2]
    } else if (x == 3 && y == 4) {
        layers[[id - 1]][, 5]
    } else {
        layers[[id]][x, y - 1]
    }
}

right_neighbours <- function(layers, id, x, y) {
    if (y == 5) {
        layers[[id + 1]][3, 4]
    } else if (x == 3 && y == 2) {
        layers[[id - 1]][, 1]
    } else {
        layers[[id]][x, y + 1]
    }
}


# function to count number of bugs in adjacent cells
num_bugs <- function(layers, id, x, y) {
    neighbours <- c(up_neighbours(layers, id, x, y),
                    down_neighbours(layers, id, x, y),
                    left_neighbours(layers, id, x, y),
                    right_neighbours(layers, id, x, y))
    
    sum(neighbours == "#")
}

# function to update the layout
update_layer <- function(layers, id) {
    layer <- layers[[id]]
    new_layer <- layers[[id]]
    for (i in 1:5) {
        for (j in 1:5) {
            if (i != 3 || j != 3) {
                n <- num_bugs(layers, id, i, j)
                if (layer[i, j] == "#") {
                    if (n != 1) {
                        new_layer[i, j] = "."
                    }
                } else {
                    if (n == 1 || n == 2) {
                        new_layer[i, j] = "#"
                    }
                }
            }
        }
    }
    new_layer
}

# original layout
layout <- matrix(instructions, nrow = 5, ncol = 5, byrow = TRUE)

# initialise layers 
# (initial layer + 200 empty layers either side + 2 additional empty layers)
layers <- lapply(1:403, function(x) matrix(".", nrow = 5, ncol = 5))
layers[[202]] <- layout

# run for 200 minutes
for (minutes in 1:200) {
    new_layers <- layers
    # each minute potentially brings another pair of layers in to play
    for (i in (202 - minutes):(202 + minutes)) {
        new_layers[[i]] <- update_layer(layers, i)
    }
    layers <- new_layers
}

# part two answer
sum(sapply(layers, function(x) sum(x == "#")))
