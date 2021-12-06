# advent of code - day 3


dt <- read.fwf(here::here("2021", "day-3", "input.txt"), widths = rep(1, 12))


# part 1 ------------------------------------------------------------------

most_common <- function(x) {
    a <- sort(table(x))
    as.integer(names(which(a == max(a))))
}

least_common <- function(x) {
    a <- sort(table(x))
    as.integer(names(which(a == min(a))))
}

gamma_binary <- paste0(vapply(dt, most_common, integer(1)), collapse = "")
gamma <- strtoi(gamma_binary, base = 2)

beta_binary <- paste0(vapply(dt, least_common, integer(1)), collapse = "")
beta <- strtoi(beta_binary, base = 2)

gamma * beta



# part 2 ------------------------------------------------------------------

o2_rating <- function(x) {
    
    for (i in seq_along(x)) {
        
        keep_value <- most_common(x[, i])
        
        if (length(keep_value) > 1) {
            keep_value <- 1
        }
        
        rows_to_keep <- which(x[,i] == keep_value)
        x <- x[rows_to_keep, ]
        
        if (nrow(x) == 1) break
    }
    
    o2_binary <- paste0(x, collapse = "")
    strtoi(o2_binary, base = 2)
}

co2_rating <- function(x) {
    
    for (i in seq_along(x)) {
        
        keep_value <- least_common(x[, i])
        
        if (length(keep_value) > 1) {
            keep_value <- 0
        }
        
        rows_to_keep <- which(x[,i] == keep_value)
        x <- x[rows_to_keep, ]
        
        if (nrow(x) == 1) break
    }
    
    o2_binary <- paste0(x, collapse = "")
    strtoi(o2_binary, base = 2)
}

o2 <- o2_rating(dt)
co2 <- co2_rating(dt)

o2 * co2
