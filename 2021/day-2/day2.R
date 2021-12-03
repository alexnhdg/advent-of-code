# advent of code - day 2
# https://adventofcode.com/2021/day/2

library(tidyverse)

# Input data

commands <- read_delim(
    here::here("2021", "day-2", "input.txt"),
    col_names = c("direction", "units"),
    delim = " "
)

# Part 1 ------------------------------------------------------------------

current_coord <- c(0, 0)
names(current_coord) <- c("horizontal", "depth")

for (i in 1:nrow(commands)) {
    
    direction <- commands[[i, "direction"]]
    units <- commands[[i, "units"]]
    
    if (direction == "forward") {
        current_coord[["horizontal"]] <- current_coord[["horizontal"]] + units
        
    } else if (direction == "down") {
        current_coord[["depth"]] <- current_coord[["depth"]] + units
        
    } else if (direction == "up") {
        current_coord[["depth"]] <- current_coord[["depth"]] - units
    }
    
}

current_coord
prod(current_coord)



# Part 2 ------------------------------------------------------------------

current_coord <- c(0, 0, 0)
names(current_coord) <- c("horizontal", "depth", "aim")

for (i in 1:nrow(commands)) {
    
    direction <- commands[[i, "direction"]]
    units <- commands[[i, "units"]]
    
    if (direction == "forward") {
        current_coord[["horizontal"]] <- current_coord[["horizontal"]] + units
        current_coord[["depth"]] <- current_coord[["depth"]] + current_coord[["aim"]] * units
        
    } else if (direction == "down") {
        current_coord[["aim"]] <- current_coord[["aim"]] + units
        
    } else if (direction == "up") {
        current_coord[["aim"]] <- current_coord[["aim"]] - units
    }
    
}

current_coord
prod(current_coord[1:2])