# load data ---------------------------------------------------------------
input <- "day23/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)

# function to initialise state
init_state <- function(x, input) {
    state <- list()
    state$relative_base <- 0
    state$index <- 1
    state$input <- input
    state$value <- x
    generic_intcode_computer(state)
}

# initialise network of 50
network <- lapply(0:49, init_state, input)

# initialise x_queue
x_queue <- vector("list", 50)

# initialise x_queue
y_queue <- vector("list", 50)

# initialise NAT
NAT <- list()

# keep track of NAT y outputs
NAT_y <- numeric()

# function to check for repeated value in NAT_y
repeated <- function(x) {
    tmp <- rle(x)
    any(tmp$lengths == 2)
}


# parts one and two -------------------------------------------------------
while (!repeated(NAT_y)) {
    for (i in 1:50) {
        if (network[[i]]$io == "in") {
            if (length(x_queue[[i]]) == 0) {
                network[[i]]$value = -1
                network[[i]] <- generic_intcode_computer(network[[i]])
            } else {
                network[[i]]$value = x_queue[[i]][1]
                network[[i]] <- generic_intcode_computer(network[[i]])
                network[[i]]$value = y_queue[[i]][1]
                network[[i]] <- generic_intcode_computer(network[[i]])
                x_queue[[i]] <- x_queue[[i]][-1]
                y_queue[[i]] <- y_queue[[i]][-1]
            }
        } else if (network[[i]]$io == "out") {
            address <- network[[i]]$output
            network[[i]] <- generic_intcode_computer(network[[i]])
            x <- network[[i]]$output
            network[[i]] <- generic_intcode_computer(network[[i]])
            y <- network[[i]]$output
            network[[i]] <- generic_intcode_computer(network[[i]])
            
            if (address == 255) {
                NAT$x <- x
                NAT$y <- y
                break
            } else {
                x_queue[[address + 1]] <- c(x_queue[[address + 1]] ,x)
                y_queue[[address + 1]] <- c(y_queue[[address + 1]] ,y)    
            }
        }
    }
    
    # uncomment for answer to part one
    # if (exists("address") && (address == 255)) {
    #     print(y)
    #     break
    # }
    
    all_incoming <- all(sapply(network, function(x) x$io == "in"))
    all_empty <- all(sapply(x_queue, function(x) length(x) == 0))
    if (all_empty && all_incoming) {
        x_queue[[1]] <- NAT$x
        y_queue[[1]] <- NAT$y
        NAT_y <- c(NAT_y, NAT$y)
    }
}

# answer to part two
print(tail(NAT_y, 1))