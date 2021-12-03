# advent of code - day 1
# https://adventofcode.com/2021/day/1

library(tidyverse)

# Part 1 ------------------------------------------------------------------

# How many measurements are larger than the previous measurement?

measurements <- read_tsv(here::here("2021", "day-1", "input.txt"), col_names = FALSE) %>% pull()

increasing <- vector("logical", length(data))

for (i in seq_along(measurements)) {
    if (i == 1) {
        increasing[[i]] <- FALSE
    } else if (measurements[[i]] > measurements[[i-1]]) {
        increasing[[i]] <- TRUE
    } else {
        increasing[[i]] <- FALSE
    }
}

sum(increasing)


# Part 2 ------------------------------------------------------------------

# Consider sums of a three-measurement sliding window. How many sums are larger 
# than the previous sum?

measurements2 <- tibble(
    lead_0 = measurements,
    lead_1 = lead(measurements),
    lead_2 = lead(measurements, 2)
) %>% 
    rowwise() %>% 
    mutate(sum = sum(c(lead_0, lead_1, lead_2))) %>% 
    drop_na() %>% 
    pull(sum)



increasing <- vector("logical", length(measurements2))

for (i in seq_along(measurements2)) {
    if (i == 1) {
        increasing[[i]] <- FALSE
    } else if (measurements2[[i]] > measurements2[[i-1]]) {
        increasing[[i]] <- TRUE
    } else {
        increasing[[i]] <- FALSE
    }
}

sum(increasing)