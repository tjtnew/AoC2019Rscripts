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

powers <- matrix(2^(0:24), nrow = 5, ncol = 5, byrow = TRUE)
tmp <- layout[2:6, 2:6]
sum(powers[which(tmp == "#", arr.ind = TRUE)])


    

