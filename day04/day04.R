# This version is heavily inspired by Adam Gruer's
# (https://github.com/adam-gruer/aoc2019/blob/master/04.R)
# In particular it borrows two functions he used.  Firstly the digits function
# to convert an integer in to it's individual digits.  Secondly the 
# Run Length Encoding function (rle) that I'd not even heard of until I saw it
# in his solution.

# input -------------------------------------------------------------------

# integers to be tested
range = 183564:657474


# function definitions ----------------------------------------------------

# integer division then modulo division by 10 on result
digits <- function(number, units){ number %/% units %% 10}

# function to check if vector monotonically increasing
# from https://stackoverflow.com/a/13094801/10746205
is_increasing <- function(x) {
    all(x == cummax(x))
}

# function to determine valid vector of integers
is_valid_part_one <- function(x) {
    is_increasing(x) && anyDuplicated(x) > 0
}

# function to determine valid integer vectors in part two.  Assumes integer
# vectors have already been filtered in part one
is_valid_part_two <- function(x) {
    any(rle(x)$length == 2)
}


# part one ----------------------------------------------------------------

# convert to digits
range_digits <- lapply(range, digits, units = 10^c(5:0))

# valid integers (logical)
tmp = lapply(range_digits, is_valid_part_one)

# valid integer vectors
valid_integers_part_one <- range_digits[unlist(tmp)]

# How many are valid in part one
length(valid_integers_part_one)

# part two ----------------------------------------------------------------

# valid integers (logical)
tmp <- lapply(valid_integers_part_one, is_valid_part_two)

# valid integers
valid_integers_part_two <- valid_integers_part_one[unlist(tmp)]

# how many are valid in part two
length(valid_integers_part_two)