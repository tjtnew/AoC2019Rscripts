# load data ---------------------------------------------------------------
input_file <- "day03/input"
wires <- strsplit(readLines(input_file), ",")


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

# answer
distances <- lapply(intersections, manhattan, c(0,0))
distances <- distances[distances > 0]
min(unlist(distances))


# part two ----------------------------------------------------------------

# answer - ignore the origin when calculating both intersections and steps
min((match(intersections[-1], wire_1) + match(intersections[-1], wire_2))) - 2


# visualisations ----------------------------------------------------------

# convert to matrix for plotting
wire_1 <- do.call(rbind, wire_1)
wire_2 <- do.call(rbind, wire_2)
intersections <- do.call(rbind, intersections)

# function for plotting (calls x11 as rstudio plot window is slow)
wire_vis <- function(w1, w2, intersections, static = TRUE) {
    x11(bg="black", type = "nbcairo")
    plot(rbind(w1, w2), type="n", xaxt = "n", yaxt="n", asp = 1)
    if (static) {
        lines(w1, col = "yellow")
        lines(w2, col = "red")
        points(intersections, col = "white", pch = 15, cex = 0.5)    
    } else {
        min_len <- min(nrow(w1), nrow(w2))
        max_len <- max(nrow(w1), nrow(w2))
        w1 <- as.data.frame(w1)
        w2 <- as.data.frame(w2)
        for (i in 1:min_len) {
            points(w1[i,], col = "red", pch = ".")
            points(w2[i,], col = "yellow", pch = ".")
        }
        if (nrow(w1) > min_len) {
            for (i in (min_len + 1):max_len) {
                points(w1[i,], col = "red", pch = ".")
            }   
        } else if (nrow(w2) > min_len) {
            for (i in (min_len + 1):max_len) {
                points(w2[i,], col = "yellow", pch = ".")
            }
        }
    }
}
    
# static vis
wire_vis(wire_1, wire_2, intersections, static = TRUE)

# dynamic vis
wire_vis(wire_1, wire_2, intersections, static = FALSE)

