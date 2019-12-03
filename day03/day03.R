# load data ---------------------------------------------------------------
input_file <- "day03/input"

# open connection, read input, close connection
# https://stackoverflow.com/a/4106976/10746205
# thank you Joshua Ulrich and JD Long
con <- file(input_file, open = "r")
wires=list()
while(length(one_line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    directions <- strsplit(one_line, ",")
    wires <- c(wires, directions)
}
close.connection(con)



# part one ----------------------------------------------------------------

# generate coordinates based on an origin point and a path
extra_coords <- function(origin, path) {
    direction <- substr(path, 1, 1)
    distance <- as.integer(substr(path, 2, nchar(path)))
    if (direction == "L") {
        lapply(1:distance, function(x) c(origin[1] - x, origin[2]))
    } else if (direction == "R") {
        lapply(1:distance, function(x) c(origin[1] + x, origin[2]))
    } else if (direction == "U") {
        lapply(1:distance, function(x) c(origin[1], origin[2] + x))
    } else if (direction == "D") {
        lapply(1:distance, function(x) c(origin[1], origin[2] - x))
    } else {
        stop("Unknown direction")
    }
}

# generate coordinates for the entire wire path
generate_wire <- function(wire) {
    path = list()
    path[[1]] = c(0, 0)
    for (i in 1:length(wire)) {
        coords <- extra_coords(path[[length(path)]], wire[i])
        path <- c(path, coords)
    }
    path
}

wire_1 <- generate_wire(wires[[1]])
wire_2 <- generate_wire(wires[[2]])

# we're not interested in self-intersections
unique_wire_1 <- unique(wire_1)
unique_wire_2 <- unique(wire_2)

# coordinates of both wires
combined_wire <- c(unique_wire_1, unique_wire_2)

# intersections are the duplicated coordinates
intersections <- combined_wire[duplicated(combined_wire)]

# function to calculate manhattan distance (2d only)
manhattan <- function(x, y) {
    distance = abs(x[1] - y[1]) + abs(x[2] - y[2])
}

# answer to part one
distances <- lapply(intersections, manhattan, c(0,0))
distances <- distances[distances > 0]
min(unlist(distances))



# part two ----------------------------------------------------------------

# function to calculate minimum steps to a point in a wire
min_steps_to_point <- function(wire, point) {
    tmp <- lapply(wire, function(x) identical(x, point))
    steps <- which(unlist(tmp) == TRUE)[1]
    min_steps <- steps - 1 # don't count the origin as a step
    min_steps
}

# work out the minimum total steps for each intersection
min_steps_list <- vector("list", length(intersections))
for (i in 1:length(intersections)) {
    wire_1_steps <- min_steps_to_point(wire_1, intersections[[i]])
    wire_2_steps <- min_steps_to_point(wire_2, intersections[[i]])
    min_steps_list[[i]] <- wire_1_steps + wire_2_steps
}

# remove steps to origin and return minimum
min_steps_list <- min_steps_list[min_steps_list > 0]
min(unlist(min_steps_list))