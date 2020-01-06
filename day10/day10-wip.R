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
asteroid_angles <- function(base_x, base_y, asteroids) {
    roids <- asteroids[asteroids[, 1] != base_y | asteroids[, 2] != base_x, ]
    x <- roids[, 2] - base_x
    y <- roids[, 1] - base_y
    tmp <- atan2(x, y)
    ifelse (tmp >= 0, tmp, 2 * pi + tmp)
}


# part one ----------------------------------------------------------------
asteroids <- which(space == "#", arr.ind = TRUE)

angles <- mapply(asteroid_angles, asteroids[, 2], asteroids[, 1],
                 MoreArgs = list(asteroids = asteroids), SIMPLIFY = FALSE)

num_visible <- lapply(angles, function(x) length(unique(x)))
max(unlist(num_visible))


# part two ----------------------------------------------------------------
x_coord <- asteroids[which.max(num_visible), 2]
y_coord <- asteroids[which.max(num_visible), 1]

other_asteroids <- asteroids[asteroids[, 1] != y_coord | asteroids[, 2] != x_coord, ]
other_asteroid_angles <- angles[[which.max(num_visible)]]
others <- cbind(other_asteroids, other_asteroid_angles)

# Below still needs changing

# asteroids <- list()
# for (i in 1:height) {
#     for (j in 1:width) {
#         if ((space[i, j] == "#") && ((i != y_coord) || (j != x_coord))) {
#             angle <- atan2_mod(x_coord, y_coord, j, i)
#             distance <- sqrt((x_coord - j)^2 + (y_coord - i)^2)
#             asteroids <- c(asteroids, list(c(angle, distance, j, i)))
#         }
#     }
# }
# 
# # pull out angles, distances and coordinates
# angles <- unlist(lapply(asteroids, `[`, 1))
# distances <- unlist(lapply(asteroids, `[`, 2))
# x_coords <- unlist(lapply(asteroids, `[`, 3))
# y_coords <- unlist(lapply(asteroids, `[`, 4))
# 
# # put in dataframe for easy sorting
# results <- data.frame(angles = angles, 
#                       distances = distances,
#                       x = x_coords,
#                       y = y_coords)
# 
# results <- results[with(results, order(angles, distances)), ]
# 
# # need to add counter so we can ensure we count an angle once on each rotation
# results$counter <- with(results, ave(angles, angles, FUN = seq_along))
# results <- results[with(results, order(counter,angles, distances)), ]
# 
# # answer!
# x = results[200, 3]
# y = results[200, 4]
# 100*(x-1) + y-1
# 
