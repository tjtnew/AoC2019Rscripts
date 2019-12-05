# load data ---------------------------------------------------------------
input <- "day05/input"
input <- unlist(read.csv(input, header = FALSE), use.names = FALSE)


# helper functions --------------------------------------------------------

# calculate digits at particular unit in a number
digits <- function(number, units){ number %/% units %% 10}


# opcode functions --------------------------------------------------------
opcode_binary <- function(input, index, parcode, f) {
    index <- index + 1
    if (parcode[3] == 0) {
        x_position <- input[index]
        x <- input[x_position + 1]
    } else if(parcode[3] == 1) {
        x <- input[index]
    }
    index <- index + 1
    if (parcode[2] == 0) {
        y_position <- input[index]
        y <- input[y_position + 1]
    } else if(parcode[2] == 1) {
        y <- input[index]
    }
    index <- index + 1
    out_position <- input[index]
    input[out_position + 1] <- f(x, y)
    
    index <- index + 1
    
    return(list(input = input,
                index = index))
}

opcode_1 <- function(input, index, parcode) {
    opcode_binary(input, index, parcode, `+`)
}

opcode_2 <- function(input, index, parcode) {
    opcode_binary(input, index, parcode, `*`)
}

opcode_3 <- function(input, index, value) {
    index <- index + 1
    position <- input[index]
    input[position + 1] <- as.integer(value)
    index <- index + 1
    
    return(list(input = input,
                index = index))
}

opcode_4 <- function(input, index, parcode) {
    index <- index + 1
    if (parcode[3] == 0) {
        position <- input[index]
        print(input[position + 1])
    } else if (parcode[3] == 1) {
        print(input[index])
    }
    index <- index + 1
    
    return(list(input = input,
                index = index))
}

opcode_jump <- function(input, index, parcode, f) {
    if (parcode[3] == 0) {
        position <- input[index + 1]
        value <- input[position + 1]
    } else {
        value <- input[index + 1]
    }
    if (f(value, 0)) {
        if (parcode[2] == 0) {
            position <- input[index + 2]
            value <- input[position + 1]
            index <- value + 1
        } else {
            value <- input[index + 2]
            index <- value + 1
        }
    } else {
        index <- index + 3
    }
    
    return(list(input = input,
                index = index))
}

opcode_5 <- function(input, index, parcode) {
    opcode_jump(input, index, parcode, `!=`)
}

opcode_6 <- function(input, index, parcode) {
    opcode_jump(input, index, parcode, `==`)
}

opcode_compare <- function(input, index, parcode, f) {
    if (parcode[3] == 0) {
        position <- input[index + 1]
        value_1 <- input[position + 1]
    } else {
        value_1 <- input[index + 1]
    }
    
    if (parcode[2] == 0) {
        position <- input[index + 2]
        value_2 <- input[position + 1]
    } else {
        value_2 <- input[index + 2]
    }
    
    out_position <- input[index + 3]
    if (f(value_1, value_2))  {
        input[out_position + 1] <- 1
    } else {
        input[out_position + 1] <- 0
    }
    index <- index + 4
    
    return(list(input = input,
                index = index))
}

opcode_7 <- function(input, index, parcode) {
    opcode_compare(input, index, parcode, `<`)
}

opcode_8 <- function(input, index, parcode) {
    opcode_compare(input, index, parcode, `==`)
}

# build the Intcode computer ----------------------------------------------

intcode_computer <- function(input, value) {
    index <- 1
    code <- digits(input[index], 10^(4:0))
    opcode <- as.integer(paste0(code[4],code[5]))
    parcode <- code[1:3]
    
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
        parcode <- code[1:3]
        
    }
}


# Answer to part one ------------------------------------------------------
intcode_computer(input, value = 1)


# Answer to part two ------------------------------------------------------
intcode_computer(input, value = 5)



