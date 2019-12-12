# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day12/input"
input <- readLines(input)

# match positive or negative characters and convert to integers
matches <- gregexpr("(-|+)?\\d+", input)
values <- as.integer(unlist(regmatches(input, matches)))

# number of moons and coordinates per moon
num_moons <- length(matches)
num_coords <- 3

# put original positions in to dataframe for ease of working
positions_original <- matrix(values,  num_moons, num_coords, byrow = TRUE)
positions_original <- as.data.frame(positions_original)
colnames(positions_original) <- c("x", "y", "z")
positions_original$moon <- 1:4

# original velocity
velocity_original <- data.frame(x = rep(0,4), y = rep(0,4), z = rep(0,4))

# function to work out effect of gravity on positions x
velocity_change <- function(positions) {
    tmp <- merge(positions, positions, by = character())
    tmp_diff <- tmp[c(1:3)]
    colnames(tmp_diff) <- c("x", "y", "z")
    tmp_diff[tmp[1:3] < tmp[5:7]] <- 1
    tmp_diff[tmp[1:3] == tmp[5:7]] <- 0
    tmp_diff[tmp[1:3] > tmp[5:7]] <- -1
    tmp_diff$moon <- tmp$moon.x
    changes <- aggregate(x=tmp_diff[1:3], 
                         by = list(moon = tmp_diff$moon), 
                         FUN = sum)
    changes[c("x","y","z", "moon")]
}


# part one ----------------------------------------------------------------

# initialise velocity and positions# initialise velocity and positions
velocity <- velocity_original
positions <- positions_original

# 1000 steps
for (i in 1:1000) {
    velocity <- velocity + velocity_change(positions)[ , 1:3]
    positions[ , 1:3] <- positions[, 1:3] + velocity
}

# answer to part one
sum(rowSums(abs(velocity)) * rowSums(abs(positions[,1:3])))


# part two ----------------------------------------------------------------

# can consider positions and velocities on each axis independently and then
# look for common multiple of the number of steps for each axis

# initialise velocity and positions
velocity <- velocity_original
positions <- positions_original
x_pos_vel_original <- c(positions[, 1], velocity[, 1])
y_pos_vel_original <- c(positions[, 2], velocity[, 2])
z_pos_vel_original <- c(positions[, 3], velocity[, 3])

# keep track of when repeat found
x_match <- FALSE
y_match <- FALSE
z_match <- FALSE
id <- 1

# we only need to check the return to the initial position and velocity as
# knowing the current uniquely determines the previous one. Thus the first
# repeat will always be the initial position.
while(!(x_match && y_match && z_match)) {

    # update velocities and positions
    velocity <- velocity + velocity_change(positions)[ , 1:3]
    positions[ , 1:3] <- positions[, 1:3] + velocity
    
    # only calculate if x id not yet found
    if (!x_match) {
        x_pos_vel <- c(positions[, 1], velocity[, 1])
        if (all(x_pos_vel == x_pos_vel_original)) {
            print(paste("first repeat of x after ", id, "iterations"))
            x_match <- TRUE
            x <- id
        }    
    }
    
    # only calculate if y id not yet found
    if(!y_match) {
        y_pos_vel <- c(positions[, 2], velocity[, 2])
        if (all(y_pos_vel == y_pos_vel_original)) {
            print(paste("first repeat of y after ", id, "iterations"))
            y_match <- TRUE
            y <- id
        }
    }
    
    # only calculate if z id not yet found
    if (!z_match) {
        z_pos_vel <- c(positions[, 3], velocity[, 3])
        if (all(z_pos_vel == z_pos_vel_original)) {
            print(paste("first repeat of z after ", id, "iterations"))
            z_match <- TRUE
            z <- id
        }
    }
    id <- id + 1
}

# greatest common divisor function (needed for least common multiple)
gcd <- function(x, y) {
    while(y != 0) {
        r <- x %% y
        x <- y
        y <- r
    }
    x
}

# least common multiple function
lcm <- function(x,y) {
    x * y / gcd(x, y)
}

# answer to part two
options(scipen = 999)
Reduce(lcm, c(x, y, z))
