# load data ---------------------------------------------------------------
input <- "day10/input"
input <- readLines(input)

# space dimensions
width <- nchar(input[[1]])
height <- length(input)

# convert in to array
input <- unlist(strsplit(input, ""))
space <- matrix(input, nrow = height, ncol = width, byrow = TRUE)

# angles of other asteroids from station (relative to north)
# cheers K. Miller (stackoverflow https://math.stackexchange.com/a/1596518)
asteroid_angles <- function(base_x, base_y, asteroids) {
    roids <- asteroids[asteroids[, 1] != base_y | asteroids[, 2] != base_x, ]
    x <- roids[, 2] - base_x
    y <- roids[, 1] - base_y
    theta <- atan2(x, -y)
    theta[theta < 0] <- theta[theta < 0] + 2*pi
    theta
}

# part one ----------------------------------------------------------------
asteroids <- which(space == "#", arr.ind = TRUE)

angles <- mapply(asteroid_angles, asteroids[, 2], asteroids[, 1],
                 MoreArgs = list(asteroids = asteroids), SIMPLIFY = FALSE)

num_visible <- lapply(angles, function(x) length(unique(x)))
max(unlist(num_visible))


# part two ----------------------------------------------------------------
x_coord <- asteroids[which.max(unlist(num_visible)), 2]
y_coord <- asteroids[which.max(unlist(num_visible)), 1]

# calculate distances for each asteroid
idx <- asteroids[, 1] != y_coord | asteroids[, 2] != x_coord
other_asteroids <- asteroids[idx, ]
other_asteroid_angles <- angles[[which.max(unlist(num_visible))]]
others <- cbind(other_asteroids, other_asteroid_angles)
distances <- sqrt((others[, 1] - y_coord)^2 + (others[, 2] - x_coord)^2)
others <- cbind(others, distances)

# put in dataframe for easy manipulation
results <- as.data.frame(others)
colnames(results) <- c("y", "x", "angles", "distances")

# need to add counter so we can ensure we count an angle once on each rotation
results <- results[with(results, order(angles, distances)), ]
results$counter <- with(results, ave(angles, angles, FUN = seq_along))
results <- results[with(results, order(counter,angles, distances)), ]

# answer!
x = results$x[200]
y = results$y[200]
100*(x-1) + y-1

