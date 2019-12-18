# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day16/input"
input <- readLines(input)

# split and convert to integer
input <- unlist(strsplit(input,""))
input <- as.integer(input)

# pattern to repeat
pattern <- c(0,1,0,-1)


# part one ----------------------------------------------------------------
output_digit <- function(each, pattern, input) {
    tmp <- rep(pattern, each = each, length = length(input) + 1)
    tmp <- tmp[-1]
    abs(sum(tmp * input)) %% 10
}

output <- function(pattern, input) {
    each = seq_along(input)
    sapply(each, output_digit, pattern, input)
}

out <- input
for (i in 1:100) {
    out <- output(pattern, out)
}

# answer to part one
out[1:8]


# part two ----------------------------------------------------------------
input <- rep(input, 10000)
input <- input[(length(input)/2 + 1):length(input)]

output_part_2 <- function(input) {
    ending <- length(input)
    tmp <- abs(cumsum(input[ending:1])) %% 10
    rev(tmp)
}

out <- input
for (i in 1:100) {
    out <- output_part_2(out)
}

index <- as.integer(paste0(input[1:7], collapse=""))
index <- index - length(input)

# answer to part two
out[index + 1:8]
