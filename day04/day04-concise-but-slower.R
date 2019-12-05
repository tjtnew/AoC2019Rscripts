range = 183564:657474

# base R - concise but slower than main solution --------------------------

# part one ----------------------------------------------------------------
tmp <- lapply(range, function(x) x %/% 10^(5:0) %% 10)
tmp <- Filter(anyDuplicated, tmp)
tmp <- Filter(function(x) all(x == cummax(x)), tmp)
length(tmp)

# part two ----------------------------------------------------------------
tmp <- Filter(function(x) any(rle(x)$length == 2), tmp)
length(tmp)


# tidy version without use of digits function------------------------------
library(purrr)
library(stringr)

# part one ----------------------------------------------------------------
tmp <- range %>%
    str_split("") %>%
    map(as.integer) %>%
    keep(~all(.x == cummax(.x))) %>%
    keep(~anyDuplicated(.x) > 0)
    
length(tmp)

# part two ----------------------------------------------------------------
tmp %>%
    keep(~any(rle(.x)$length == 2)) %>%
    length()


