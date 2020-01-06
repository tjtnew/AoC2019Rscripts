# load data ---------------------------------------------------------------
input <- "day07/input"
input <- unlist(read.csv(input, header = FALSE), use.names = FALSE)

# load intcode computer
library(intcode)

# single pass through amps (no feedback)
initial_pass_through <- function(amps, phases, input) {
    # all states
    states <- vector("list", amps)
    
    # initial default state for amps
    state <- list()
    state$input <- input
    state$index <- 1
    state$relative_base <- 0
    state$value <- c(phases[1])
    states[[1]] <- intcode(state)
    states[[1]]$value <- 0
    states[[1]] <- intcode(states[[1]])
    
    if (amps > 1) {
        for (i in 2:amps) {
            states[[i]] <- state
            states[[i]]$value <- phases[i] 
            states[[i]] <- intcode(states[[i]])
            states[[i]]$value <- states[[i-1]]$output
            states[[i]] <- intcode(states[[i]])
        }
    }
    states
}

# loop through with feedback from from last amp
loop_pass_through <- function(states) {
    amps <- length(states)
    id <- 1
    finished = states[[1]]$finished
    while(!finished) {
        idx <- ((id + amps - 2) %% amps) + 1
        
        states[[id]]$value <- states[[idx]]$output[length(states[[idx]]$output)]
        states[[id]] <- intcode(states[[id]])
        id <- ((id) %% amps) + 1
        finished <- states[[id]]$finished 
    }
    states
}

# part one ----------------------------------------------------------------

all <- expand.grid(lapply(1:5, function(x) 0:4))
perms <- all[apply(all, 1, function(x) {length(unique(x)) == 5}),]
val <- -9999
for (i in 1:nrow(perms)) {
    phase <- perms[i, ]
    res <- initial_pass_through(5, as.integer(phase), input)[[5]]$output
    if (res > val) {
        val <- res
    }
}
unlist(val)

# part two ----------------------------------------------------------------
all <- expand.grid(lapply(1:5, function(x) 5:9))
perms <- all[apply(all, 1, function(x) {length(unique(x)) == 5}),]
val <- -9999
for (i in 1:nrow(perms)) {
    phase <- perms[i, ]
    states <- initial_pass_through(5, as.integer(phase), input)
    states <- loop_pass_through(states)
    res <- states[[5]]$output[length(states[[5]]$output)]
    if (res > val) {
        val <- res
    }
}
unlist(val)
