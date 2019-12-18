# load data ---------------------------------------------------------------
input <- "~/projects/AoC2019Rscripts/day17/input"
input <- as.numeric(unlist(read.csv(input, header = FALSE), use.names = FALSE))

# load intcode computer
# devtools::install_github("tjtnew/intcode")
library(intcode)


# part one ----------------------------------------------------------------

# obtain view of scaffolding
state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
state_new <- single_intcode_computer(state)
scaffolding <- single_intcode_computer(state)$all_output

# convert to asci
asci <- intToUtf8(scaffolding)
tmp <- unlist(strsplit(asci, "\n"))
tmp <- strsplit(tmp,"")
tmp <- Reduce(rbind, tmp)
scaffolding_matrix <- as.matrix(tmp)

# find positions of hashes not on boundaries (no intersections on boundaries)
hashes <- which(scaffolding_matrix == "#", arr.ind = TRUE)
hashes <- hashes[hashes[, 1] > 1 & hashes[, 1] < nrow(scaffolding_matrix),]
hashes <- hashes[hashes[, 2] > 1 & hashes[, 2] < ncol(scaffolding_matrix),]

# function to check if coordinates match a value
is_intersection <- function(coords, graph, value) {
    rr <- coords[1]
    cc <- coords[2]
    positions <- rbind(c(rr + 1, cc), c(rr + 1, cc), c(rr, cc - 1), c(rr, cc + 1))
    values <- graph[positions]
    if (all(values == value)) {
        TRUE
    } else {
        FALSE
    }
}

# find intersections
intersections <- hashes[apply(hashes, 1, is_intersection, scaffolding_matrix, "#"), ]
intersections <- intersections - 1

# part one answer
sum(intersections[, 1] * intersections[, 2])


# part two ----------------------------------------------------------------

# view scaffolding to work out route
cat(asci)

# movements (calculated by pen and paper)
A <- "L,10,R,8,L,6,R,6\n"
A <- utf8ToInt(A)
B <- "L,8,L,8,R,8\n"
B <- utf8ToInt(B)
C <- "R,8,L,6,L,10,L,10\n"
C <- utf8ToInt(C)
main <- "A,B,A,C,A,B,C,B,C,B\n"
main <- utf8ToInt(main)
camera <- "n\n"
camera <- utf8ToInt(camera)
asci_input <- c(main, A, B, C, camera)

state <- list()
state$relative_base <- 0
state$index <- 1
state$input <- input
state$input[1] <- 2

finished = FALSE
output <- NULL
while (!finished) {
    state <- generic_intcode_computer(state)
    finished <- state$finished
    if (!finished) {
        #cat(intToUtf8(state$output))
        if(state$io == "in") {
            state$value <- asci_input[1]
            asci_input <- asci_input[-1]
        }
    }
}

# part two answer
state$output



