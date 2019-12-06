# load data ---------------------------------------------------------------
input <- read.csv("day06/input", header = FALSE, stringsAsFactors = FALSE)
input <- unlist(input, use.names = FALSE)
input <- strsplit(input,")")


# adjacency list graph of orbits ------------------------------------------
orbits <- list()
for (i in seq_len(length(input))) {
    orbited <- input[[i]][1]
    orbitee <- input[[i]][2]
    if (is.null(orbits[[orbited]])) {
        orbits[[orbited]] <- orbitee
    } else {
        orbits[[orbited]] <- c(orbits[[orbited]], orbitee)
    }
}

# dijkstra's algorithm
dijkstra <- function(graph, source, vertices) {
    Q <- vertices
    distances <- rep_len(Inf, length(Q))
    names(distances) <- Q
    distances[source] <- 0
    
    while (length(Q) > 0) {
        u <- which.min(distances[Q])
        Q <- Q[-u]
        neigbours <- graph[[names(u)]]
        for (v in neigbours) {
            alt <- distances[names(u)] + 1
            if (alt < distances[v]) {
                distances[v] <- alt
            }
        }
    }
    distances
}


# part one ----------------------------------------------------------------

# As each objects orbits exactly one other object (save COM) we can use
# dijkstra's algorithm to calculate the shortest part from each vertex to COM
# and then take the sum of these
vertices <- unique(unlist(input))
distances <- dijkstra(orbits, "COM", vertices)
sum(distances)


# part two ----------------------------------------------------------------

# Now we are not worried about which planet orbits which, just how many orbits
# are between things.  For this reason we now turn how orbits in to a undirected
# adjacency list
orbits_undirected <- orbits
for (i in seq_len(length(orbits))) {
    orbited <- names(orbits[i])
    orbitees <- orbits[[i]]
    for (j in orbitees) {
        if(is.null(orbits[[j]])) {
            orbits_undirected[[j]] <- orbited
        } else {
            orbits_undirected[[j]] <- c(orbits[[j]], orbited)
        }
    }
}

# Now calculated distances from everything to YOU
distances <- dijkstra(orbits_undirected, "YOU", vertices) 

# Distance is the distance between YOU and SAN minus 2
distances[["SAN"]] - 2
