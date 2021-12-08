# advent of code - day 4
# https://adventofcode.com/2021/day/4

infile <- here::here("2021", "day-4", "input.txt")

# create vector with called numbers
numbers <- scan(infile, what = integer(), sep = ",", nlines = 1)

# create vector with game board matrices
x <- scan(infile, what = integer(), skip = 1)
board_values <- split(x, gl(length(x) / 25, 25))
boards <- lapply(board_values, matrix, 5, 5, byrow = TRUE)

# Create game tracking objects
markers <- lapply(rep(FALSE, length(x) / 25), matrix, 5, 5)
wins <- rep(FALSE, length(x) / 25)
scores <- rep(0, length(x) / 25)
ranks <- rep(0, length(x) / 25)

# part 1 ------------------------------------------------------------------

rank_value <- 0
for (number in numbers) {
    for (i in seq_along(boards)) {
        
        if (wins[[i]] == TRUE) {
            next
        }
        
        # check if number exists on bingo card and mark if it does
        location <- which(number == boards[[i]], arr.ind = TRUE)
        markers[[i]][location] <- TRUE 
        
        # check if player has bingo
        row_bingo <- sum(apply(markers[[i]], 1, sum) == 5)
        col_bingo <- sum(apply(markers[[i]], 2, sum) == 5)
        bingo <- row_bingo | col_bingo
        
        if (bingo) {
            rank_value <- rank_value + 1
            wins[[i]] <- TRUE
            scores[[i]] <- sum(boards[[i]][!markers[[i]]]) * number
            ranks[[i]] <- rank_value
        }
    
    }
    
}

scores[which(ranks == 1)]


# part 2 ------------------------------------------------------------------

scores[which(ranks == max(ranks))]
