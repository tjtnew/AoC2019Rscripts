# input -------------------------------------------------------------------

# integers to be tested
range = 183564:657474


# part one ----------------------------------------------------------------

# function to determine valid integer in part one
is_valid_part_one <- function(x) {
    
    # convert to character
    x <- as.character(x)
    
    # pull out individual digits
    a <- substr(x, 1, 1)
    b <- substr(x, 2, 2)
    c <- substr(x, 3, 3)
    d <- substr(x, 4, 4)
    e <- substr(x, 5, 5)
    f <- substr(x, 6, 6)
    
    # check ordering
    if ((a <= b) && (b <= c) && (c <= d) && (d <= e) && (e <= f)) {
        
        # check for atleat one pair
        if (anyDuplicated(c(a, b, c, d, e, f))) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}

# indices of valid integers
tmp = lapply(range, is_valid_part_one)

# valid integers
valid_integers_part_one <- range[unlist(tmp)]

# How many are valid in part one
length(valid_integers_part_one)



# part two ----------------------------------------------------------------

# function to determine valid integer in part two.  This assumes integers have
# already been filtered from part one
is_valid_part_two <- function(x) {
    
    # convert to character
    x <- as.character(x)
    
    # pull out individual digits
    a <- substr(x, 1, 1)
    b <- substr(x, 2, 2)
    c <- substr(x, 3, 3)
    d <- substr(x, 4, 4)
    e <- substr(x, 5, 5)
    f <- substr(x, 6, 6)
    
    # generate all of the integer groupings in x
    digits <- c(a, b, c, d, e, f)
    groups <- vector("list", 6) # as up to 6 groups possible
    idx = 1
    for (i in 1:5) {
        groups[[idx]] = c(groups[[idx]], digits[i])
        if (digits[i] != digits[i + 1]) {
            idx = idx + 1
        }
    }
    groups[[idx]] = c(groups[[idx]], digits[6])
    
    # Are any groups of length 2
    #groups <- groups[lapply(groups, length) > 1]
    any(lapply(groups, length) == 2)
}

# indices of valid integers
tmp <- lapply(valid_integers_part_one, is_valid_part_two)

# valid integers
valid_integers_part_two <- valid_integers_part_one[unlist(tmp)]

# how many are valid in part two
length(valid_integers_part_two)