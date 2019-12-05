# load data ---------------------------------------------------------------
input <- "day05/input"
input <- unlist(read.csv(input, header = FALSE), use.names = FALSE)


# helper functions --------------------------------------------------------

# calculate digits at particular unit in a number
digits <- function(number, units){ number %/% units %% 10}

# get_value function below from Maarten Demeyer 
# https://github.com/mpjdem/adventofcode2019/blob/master/aoc19_day5.R
# Function to retrieve the value of parameter N, depending on mode
# Do not use for write positions, those are directly given as positions
get_value <- function(input, parcodes, params, n) {
    if (parcodes[n] == 1) {
        params[n]
    } else {
        input[params[n] + 1]
    }
}

# opcode functions --------------------------------------------------------
opcode_1 <- function(input, index, parcode) {
    parameters <- input[index + 1:3]
    x <- get_value(input, parcode, parameters, 1)
    y <- get_value(input, parcode, parameters, 2)
    input[parameters[3] + 1] <- x + y
    index <- index + 4
    return(list(input = input, index = index))
}

opcode_2 <- function(input, index, parcode) {
    parameters <- input[index + 1:3]
    x <- get_value(input, parcode, parameters, 1)
    y <- get_value(input, parcode, parameters, 2)
    input[parameters[3] + 1] <- x * y
    index <- index + 4
    return(list(input = input, index = index))
}

opcode_3 <- function(input, index, value) {
    parameter <- input[index + 1]
    input[parameter + 1] <- as.integer(value)
    index <- index + 2
    return(list(input = input, index = index))
}

opcode_4 <- function(input, index, parcode) {
    x <- get_value(input, parcode, input[index + 1], 1)
    print(x)
    index <- index + 2
    return(list(input = input, index = index))
}

opcode_5 <- function(input, index, parcode) {
    parameters <- input[index + 1:2]
    x <- get_value(input, parcode, parameters, 1)
    if (x != 0) {
        y <- get_value(input, parcode, parameters, 2)
        index <- y + 1
    } else {
        index <- index + 3
    }
    return(list(input = input, index = index))
}

opcode_6 <- function(input, index, parcode) {
    parameters <- input[index + 1:2]
    x <- get_value(input, parcode, parameters, 1)
    if (x == 0) {
        y <- get_value(input, parcode, parameters, 2)
        index <- y + 1
    } else {
        index <- index + 3
    }
    return(list(input = input, index = index))
}

opcode_7 <- function(input, index, parcode) {
    parameters <- input[index + 1:3]
    x <- get_value(input, parcode, parameters, 1)
    y <- get_value(input, parcode, parameters, 2)
    if (x < y)  {
        input[parameters[3] + 1] <- 1
    } else {
        input[parameters[3] + 1] <- 0
    }
    index <- index + 4 
    return(list(input = input, index = index))
}

opcode_8 <- function(input, index, parcode) {
    parameters <- input[index + 1:3]
    x <- get_value(input, parcode, parameters, 1)
    y <- get_value(input, parcode, parameters, 2)
    if (x == y)  {
        input[parameters[3] + 1] <- 1
    } else {
        input[parameters[3] + 1] <- 0
    }
    index <- index + 4 
    return(list(input = input, index = index))
}

# intcode computer --------------------------------------------------------
intcode_computer <- function(input, value) {
    index <- 1
    code <- digits(input[index], 10^(4:0))
    opcode <- as.integer(paste0(code[4],code[5]))
    parcode <- code[3:1]
    
    while(opcode != 99) {
        if (opcode == 1) {
            result <- opcode_1(input, index, parcode)
        } else if (opcode == 2) {
            result <- opcode_2(input, index, parcode)
        } else if (opcode == 3) {
            result <- opcode_3(input, index, value)
        } else if (opcode == 4) {
            result <- opcode_4(input, index, parcode)
        } else if (opcode == 5) {
            result <- opcode_5(input, index, parcode)
        } else if (opcode == 6) {
            result <- opcode_6(input, index, parcode)
        } else if (opcode == 7) {
            result <- opcode_7(input, index, parcode)
        } else if (opcode == 8) {
            result <- opcode_8(input, index, parcode)
        } else {
            stop("unknown code")
        }
        
        input <- result$input
        index <- result$index
        code <- digits(input[index], 10^(4:0))
        opcode <- as.integer(paste0(code[4],code[5]))
        parcode <- code[3:1]
    }
}


# Answer to part one ------------------------------------------------------
intcode_computer(input, value = 1)


# Answer to part two ------------------------------------------------------
intcode_computer(input, value = 5)
