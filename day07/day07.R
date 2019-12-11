# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day07/input"
input <- unlist(read.csv(input, header = FALSE), use.names = FALSE)


# helper functions --------------------------------------------------------

# convert number in to it's digits
digits <- function(number, units){ number %/% units %% 10}

# retrieve value for use by opcode function
get_value <- function(input, parcodes, parameters, n, relative_base) {
    if (parcodes[n] == 0) {
        # double memory if needed
        while((parameters[n] + 1) > length(input)) {
            input <- c(input, rep(0, length(input)))
        }
        input[parameters[n] + 1]
    } else if (parcodes[n] == 1) {
        parameters[n]
    } else if (parcodes[n] == 2) {
        # double memory if needed
        while((parameters[n] + 1 + relative_base) > length(input)) {
            input <- c(input, rep(0, length(input)))
        }
        input[parameters[n] + 1 + relative_base]
    }
    else {
        stop("unknown parameter code")
    }
}

# write value to input 
write_value <- function(input, parcodes, parameters, n, relative_base, value) {
    if (parcodes[n] == 0) {
        input[parameters[n] + 1] <- value
    } else if (parcodes[n] == 2) {
        input[parameters[n] + 1 + relative_base] <- value
    } else {
        stop("That shouldn't happen here")
    }
    
    # if we write to an area above current vector length it will fill the
    # empty spaces with NA.  Replace these by zero's.
    input[is.na(input)] <- 0
    input
}

# update state
state_update <- function(state, input, index, output = NULL, relative_base = NULL) {
    state$input <- input
    state$index <- index
    if (!is.null(output)) {
        state$output <- output
    }
    if (!is.null(relative_base)) {
        state$relative_base <- relative_base
    }
    state
}


# opcode functions --------------------------------------------------------

opcode_1 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    input <- write_value(input, parcodes, parameters, 3, relative_base, x + y)
    index <- index + 4
    
    state <- state_update(state, input, index)
}

opcode_2 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    input <- write_value(input, parcodes, parameters, 3, relative_base, x * y)
    index <- index + 4
    
    state <- state_update(state, input, index)
}

opcode_3 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    value <- state$value[[1]]
    state$value <- state$value[-1]
    
    parameters <- input[index + 1]
    input <- write_value(input, parcodes, parameters, 1, relative_base, value)
    index <- index + 2
    
    state <- state_update(state, input, index)
}

opcode_4 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    output <- state$output
    
    parameter <- input[index + 1]
    x <- get_value(input, parcodes, parameter, 1, relative_base)
    index <- index + 2
    state <- state_update(state, input, index, output = c(output, x))
}

opcode_5 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameters <- input[index + 1:2]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    if (x != 0) {
        y <- get_value(input, parcodes, parameters, 2, relative_base)
        index <- y + 1
    } else {
        index <- index + 3
    }
    
    state <- state_update(state, input, index)
}

opcode_6 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameters <- input[index + 1:2]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    if (x == 0) {
        y <- get_value(input, parcodes, parameters, 2, relative_base)
        index <- y + 1
    } else {
        index <- index + 3
    }
    
    state <- state_update(state, input, index)
}

opcode_7 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    if (x < y)  {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 1)
    } else {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 0)
    }
    index <- index + 4
    
    state <- state_update(state, input, index)
}

opcode_8 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    if (x == y)  {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 1)
    } else {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 0)
    }
    index <- index + 4
    
    state <- state_update(state, input, index)
}

opcode_9 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    
    parameter <- input[index + 1]
    x <- get_value(input, parcodes, parameter, 1, relative_base)
    relative_base <- relative_base + x
    index <- index + 2
    
    state <- state_update(state, input, index, relative_base = relative_base)
}

# intcode computer --------------------------------------------------------
generic_intcode_computer <- function(state) {
    code <- digits(state$input[state$index], 10^(4:0))
    opcode <- as.integer(paste0(code[4],code[5]))
    parcodes <- code[3:1]
    
    while(opcode != 99) {
        if (opcode == 1) {
            state <- opcode_1(state, parcodes)
        } else if (opcode == 2) {
            state <- opcode_2(state, parcodes)
        } else if (opcode == 3) {
            state <- opcode_3(state, parcodes)
        } else if (opcode == 4) {
            state <- opcode_4(state, parcodes)
            break
        } else if (opcode == 5) {
            state <- opcode_5(state, parcodes)
        } else if (opcode == 6) {
            state <- opcode_6(state, parcodes)
        } else if (opcode == 7) {
            state <- opcode_7(state, parcodes)
        } else if (opcode == 8) {
            state <- opcode_8(state, parcodes)
        } else if (opcode == 9) {
            state <- opcode_9(state, parcodes)
        } else {
            stop("unknown code")
        }
        
        code <- digits(state$input[state$index], 10^(4:0))
        opcode <- as.integer(paste0(code[4],code[5]))
        parcodes <- code[3:1]
    }
    
    state$finished = TRUE
    if (opcode == 4) {
        state$finished = FALSE
    }
    
    state
}


# single pass through amps (no feedback)
initial_pass_through <- function(amps, phases, input) {
    # all states
    states <- vector("list", amps)

    # initial default state for amps
    state <- list()
    state$input <- input
    state$index <- 1
    state$relative_base <- 0
    state$value <- c(phases[1], 0)
    
    states[[1]] <- generic_intcode_computer(state)
    if (amps > 1) {
        for (i in 2:amps) {
            states[[i]] <- state
            states[[i]]$value <- c(phases[i], states[[i-1]]$output)
            states[[i]] <- generic_intcode_computer(states[[i]])
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
        states[[id]]$value <- c(states[[id]]$value, states[[idx]]$output[length(states[[idx]]$output)])
        states[[id]] <- generic_intcode_computer(states[[id]])
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