# load data ---------------------------------------------------------------
input <- "day08/input"
input <- as.integer(unlist(strsplit(readLines(input), "")))

# parameters --------------------------------------------------------------
width = 25
height = 6
layer_pixels <- width * height
num_layers <- length(input) / layer_pixels

# array of layers (remember R fills column-wise!!!!!!!!!)
layers <- array(input, dim = c(width, height, num_layers))

# part one ----------------------------------------------------------------
zero_counts <- apply(layers, 3, function(x) sum(x == 0))
id <- which.min(zero_counts)
sum(layers[,,id] == 1) * sum(layers[,,id] == 2)

# part two ---------------------------------------------------------------
picture <- matrix(nrow = height, ncol = width)
for (i in 1:height) {
    for (j in 1:width) {
        idx <- 1
        # remember layers array was filled column-wise!!!!!!!!!!!!!
        while((layers[j, i, idx] == 2)) {
            idx = idx + 1
        }
        # remember layers array was filled column-wise!!!!!!!!!!!!!
        picture[i, j] <- as.integer(layers[j, i, idx])
    }
}

# need to rotate 90 degrees before plotting
rotate <- function(x) t(apply(x, 2, rev))
a = image(x = 1:width,
      y = 1:height,
      rotate(picture),
      col = c("white", "black"), 
      asp = 1, xaxt = "n", yaxt = "n", ann = FALSE, bty = "n")
