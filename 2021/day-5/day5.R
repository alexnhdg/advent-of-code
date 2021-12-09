# advent of code - day 5
# https://adventofcode.com/2021/day/5


# setup -------------------------------------------------------------------

library(tidyverse)


# get data ----------------------------------------------------------------

segments <- read_lines(here::here("2021", "day-5", "input.txt")) %>% 
    tibble(input = .) %>% 
    separate(input, into = c("x1", "y1", "x2", "y2"), convert = TRUE)


# part 1 ------------------------------------------------------------------

count_gt1_overlaps <- function(x) {
    
    combos_by_segment <- vector("list", nrow(x))
    
    for (i in seq(nrow(x))) {
        
        x1 <- x[[i, "x1"]]
        x2 <- x[[i, "x2"]]
        y1 <- x[[i, "y1"]]
        y2 <- x[[i, "y2"]]
        
        vec_x <- x1:x2
        vec_y <- y1:y2
        
        combos_by_segment[[i]] <- tibble(vec_x, vec_y)
        
    }
    
    combos_all <- bind_rows(combos_by_segment)
    
    combos_all %>% 
        count(vec_x, vec_y) %>% 
        filter(n > 1) %>% 
        nrow()
    
}


segments_hv <- segments %>% 
    filter(x1 == x2 | y1 == y2)


count_gt1_overlaps(segments_hv)
    

# part 2 ------------------------------------------------------------------

count_gt1_overlaps(segments)