# load data ---------------------------------------------------------------
input <- "day02/input.csv"
input <- unlist(read.csv(input, header = FALSE))

# build the Intcode computer ----------------------------------------------
intcode_computer <- function(input) {
    index = 1
    opcode = input[index]
    while (opcode != 99) {
        x_position = input[index + 1]
        y_position = input[index + 2]
        out_position = input[index + 3]
        
        x = input[x_position + 1]
        y = input[y_position + 1]
        if (opcode == 1) {
            input[out_position + 1] = x + y
        } else if (opcode == 2) {
            input[out_position + 1] = x * y
        } else {
            stop("unknown opcode")
        }
        
        index = index + 4
        opcode = input[index]
    }
    input
}


# part 1 ------------------------------------------------------------------
# set the initial state
# remember R indexes from 1 but AoC using index from 0 for this problem
input[2] = 12
input[3] = 2

# calculate the value in position 0 (index 1) after intcode_computer halts
part_one <- intcode_computer(input)[1]


# part two ----------------------------------------------------------------

# noun / verb combos
possible_pairs <- expand.grid(0:99, 0:99)

# initial setup
output <- -1
index = 1

# continue until output is the desired value
while (output != 19690720) {
    tmp <- input
    pair <- possible_pairs[index, ]
    noun = pair[ , 1]
    verb = pair[ , 2]
    tmp[2] = noun
    tmp[3] = verb
    output = intcode_computer(tmp)[1]
    index = index + 1
}

# What is 100 * noun + verb?
part_two <- 100 * noun + verb