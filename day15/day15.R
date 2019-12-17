# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day15/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)

# part one ----------------------------------------------------------------
# Heavily adapted version of Lee algorithm
# https://www.techiedelight.com/lee-algorithm-shortest-path-in-a-maze/

# N, S, E, W
row <- c(1, -1,  0, 0) 
col <- c(0,  0, -1, 1)

# visited is a dataframe of coordinates that have been visited (i.e. only those
# those without a wall or with oxygen).  has_visited checks whether particular
# coordinates are contained in visited
has_visited <- function(visited, i, j) {
    lgl <- (visited$x == j) & (visited$y == i)
    if (all(!lgl)) {
        FALSE
    } else {
        TRUE
    }
}

# queue functions (using dataframe for queue)
queue_push <- function(x,y,distance, queue, state) {
    tmp <- data.frame(x=x,y=y,distance=distance)
    tmp$state = list(state)
    rbind(queue, tmp)
}

queue_front <- function(queue) {
    queue[nrow(queue),]
}

queue_pop <- function(queue) {
    queue[-nrow(queue),]
}

# initial values (assume starting at origin which is not a wall)
i = 0
j = 0
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
state$output <- 1 # assu
queue <- data.frame(x = i, y = j, distance = 0L)
queue$state <- list(state)
visited <- data.frame(x=integer(), y = integer())
visited <- rbind(visited, list(x = j, y = i))

# keep track of all coordinates and their types
maze <- data.frame(x=i, y = j, type = 1)

# run until we find
while (nrow(queue) > 0) {
    node <- queue_front(queue)
    queue <- queue_pop(queue)
    
    i = node$y 
    j <- node$x
    distance <- node$distance
    state <- node$state[[1]]
    
    if (state$output == 2) {
        min_dist <- distance
        break
    }
    
    state_old <- state
    
    for (k in 1:4) {
        state <- generic_intcode_computer(state_old)
        state$value <- k
        state <- generic_intcode_computer(state)
        if ((state$output > 0) && (!has_visited(visited, i + row[k], j + col[k]))) {
            visited <- rbind(visited, list(x = j + col[k], y = i + row[k]))
            queue <- queue_push(j + col[k], i + row[k], distance + 1L, queue, state = state)
        }
        maze <- rbind(maze, list(x = j + col[k], y = i + row[k], type=state$output))
    }
}

# part one answer
min_dist



# part two ----------------------------------------------------------------

# pull out the coordinates of paths and oxygen
maze <- unique(maze)
oxygen <- maze[maze$type == 2, 1:2]
paths <- maze[maze$type == 1, 1:2]
current <- oxygen

# function to find neighbours that are within distance 1 of a set of coordinates
neighbours <- function(row, paths) {
    which(apply(paths, 1, function(x) dist(rbind(x, row), "manhattan") == 1),
          useNames = FALSE)
}

# percolate and remove paths until paths are empy
idx <- 0
while(nrow(paths) > 0) {
    tmp <- unlist(apply(current, 1, neighbours, paths))
    tmp <- unique(tmp)
    current <- paths[tmp,]
    paths <- paths[-tmp,]
    idx=idx+1
}

# answer two
idx


