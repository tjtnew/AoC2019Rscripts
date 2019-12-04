# load input --------------------------------------------------------------
input <- "day01/input.txt"
modules <- unlist(read.csv(input, header = FALSE), use.names = FALSE)


# part 1 ------------------------------------------------------------------

# function to calculate fuel based on mass
fuel_needed <- function(mass) {
    floor(mass / 3) - 2
}

# fuel requirement
sum(fuel_needed(modules))


# part 2 ------------------------------------------------------------------

# additional fuel for additional fuel!
total <- 0
for (i in 1:length(modules)) {
    tmp <- fuel_needed(modules[i])
    while (tmp > 0) {
        total = total + tmp
        tmp = fuel_needed(tmp)
    }
}

# total fuel requirement is
total