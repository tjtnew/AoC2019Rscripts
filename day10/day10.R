# load data ---------------------------------------------------------------
input <- "day10/input"
input <- readLines(input)

# space dimensions
width <- nchar(input[[1]])
height <- length(input)

# convert in to array
input <- unlist(strsplit(input, ""))
space <- matrix(input, nrow = height, ncol = width, byrow = TRUE)

# function to calculate angle between to points (from north and clockwise)
# cheers K. Miller (stackoverflow https://math.stackexchange.com/a/1596518)
atan2_mod <- function(a1, a2, b1, b2) {
    #theta <- atan2(b1 - a1, b2 - a2)
    theta <- atan2(b1 - a1, a2-b2)
    if (theta < 0) {
        theta = theta + 2*pi
    }
    theta
}

# asteroids visible from point
point_asteroids <- function(x, y, width, height, space) {
    angles <- list()
    for (i in 1:height) {
        for (j in 1:width) {
            if ((space[i, j] == "#") && ((i != y) || (j != x))) {
                angles <- union(angles, atan2_mod(x, y, j, i))
            }
        }
    }
    length(angles)
}

# part one ----------------------------------------------------------------
max_num_visible <- 0
for (i in 1:height) {
    for (j in 1:width) {
        if (space[i, j] == "#") {
            num_visible <- point_asteroids(j, i, width, height, space)
            if (num_visible > max_num_visible) {
                max_num_visible <- num_visible
                x_coord = j
                y_coord = i
            }
        }
    }
}
x_coord
y_coord
max_num_visible


# part two ----------------------------------------------------------------
asteroids <- list()
for (i in 1:height) {
    for (j in 1:width) {
        if ((space[i, j] == "#") && ((i != y_coord) || (j != x_coord))) {
            angle <- atan2_mod(x_coord, y_coord, j, i)
            distance <- sqrt((x_coord - j)^2 + (y_coord - i)^2)
            asteroids <- c(asteroids, list(c(angle, distance, j, i)))
        }
    }
}

# pull out angles, distances and coordinates
angles <- unlist(lapply(asteroids, `[`, 1))
distances <- unlist(lapply(asteroids, `[`, 2))
x_coords <- unlist(lapply(asteroids, `[`, 3))
y_coords <- unlist(lapply(asteroids, `[`, 4))

# put in dataframe for easy sorting
results <- data.frame(angles = angles, 
                      distances = distances,
                      x = x_coords,
                      y = y_coords)

results <- results[with(results, order(angles, distances)), ]

# need to add counter so we can ensure we count an angle once on each rotation
results$counter <- with(results, ave(angles, angles, FUN = seq_along))
results <- results[with(results, order(counter,angles, distances)), ]

# answer!
x = results[200, 3]
y = results[200, 4]
100*(x-1) + y-1

