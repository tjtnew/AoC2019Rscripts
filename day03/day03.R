# load data 
input_file <- "day03/input"

# open connection, read input, close connection
# https://stackoverflow.com/a/4106976/10746205
con <- file(input_file, open = "r")
wires=list()
while(length(one_line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    directions <- strsplit(one_line, ",")
    wires <- c(wires, directions)
}
close.connection(con)

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
    path <- list()
    path[[1]] <- c(0, 0)
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
    abs(x[1] - y[1]) + abs(x[2] - y[2])
}

# answer to part one
distances <- lapply(intersections, manhattan, c(0,0))
distances <- distances[distances > 0]
min(unlist(distances))

# answer to part two
# ignore the origin when calculating both intersections and steps
min((match(intersections[-1], wire_1) + match(intersections[-1], wire_2))) - 2
