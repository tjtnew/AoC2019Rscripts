# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day14/input"
input <- readLines(input)

# pull out recipes and split in to amounts and chemicals
amounts <- regmatches(input, gregexpr("\\d+", input))
amounts <- lapply(amounts, as.integer)
chemicals <- regmatches(input, gregexpr("[A-Za-z]+", input))

# split into named lists of ingredients and output
names <- lapply(chemicals, function(x) x[length(x)])

output <- mapply(function(names, x, y) y[length(x)],
                 unlist(names), chemicals, amounts, 
                 SIMPLIFY = FALSE, USE.NAMES = TRUE)

ingredients <- mapply(function(names, x, y) list(x[1:(length(x) - 1)],
                                                 y[1:(length(x) - 1)]),
                      unlist(names), chemicals, amounts,
                      SIMPLIFY = FALSE, USE.NAMES = TRUE)

remainder <- rep(0, length(names))
names(remainder) <- names

# this needs speeding up for part two
ore_produced <- function(x, required, remainder, ingredients, output) {
    if (remainder[x] >= required) {
        remainder[x] <- remainder[x] - required
        return(list(ore = 0, remainder = remainder))
    } else {
        required <- required - remainder[x]
        remainder[x] <- 0
    }
    
    produced <- 0
    ore <- 0
    
    # need to improve this, should be able to work out how many times to run not loop
    while(produced < required) {
        produced <- produced + output[[x]]
        tmp <- ingredients[[x]][[1]]
        if (length(tmp) == 1 && tmp == "ORE") {
            ore <- ore + ingredients[[x]][[2]]
        } else {
            for (i in seq_along(ingredients[[x]][[1]])) {
                tmp <- ore_produced(ingredients[[x]][[1]][i], ingredients[[x]][[2]][i], remainder, ingredients, output)
                ore <- ore + tmp$ore
                remainder <- tmp$remainder
            }
        }
    }
    remainder[x] <- produced - required
    return(list(ore = ore, remainder = remainder))
}

ore_produced("FUEL", 1, remainder, ingredients, output)$ore
