require(hash)

## simulate many runs of games to check analytical results.

SetUpRandomBoard <- function(chutes, ladders){
    ## Set up a random board with a specific number
    ## of chutes and ladders

    squares <- as.character(1:100)
    board <- hash::hash()
    ## add chutes
    for(i in 1:chutes){
        ## draw two distinct locations on the board
        while(TRUE){
            endpoints <- sample(squares, size = 2, replace = FALSE)
            ## if the chute starts at square 100 the game is unwinnable
            ## we can leave the loop if we didn't draw 100
            if(!('100' %in% endpoints)){
                break
            }
        }
        ## the larger value is the start, since it's a chute
        board[[as.character(max(as.numeric(endpoints)))]] <-
            as.character(min(as.numeric(endpoints)))
        ## don't allow the same square to be in a chute or ladder
        ## more than once
        ## so, remove the drawn squares from those to sample
        squares <- squares[-which(squares %in% endpoints)]
    }
    ## add ladders
    for(i in 1:ladders){
        ## draw two distinct locations on the board
        while(TRUE){
            endpoints <- sample(squares, size = 2, replace = FALSE)
            ## don't allow the ladder to start at square 1 because it will
            ## mean players can't access part of the board
            if(!('1' %in% endpoints)){
                break
            }
        }
        ## the smaller value is the start, since it's a ladder
        board[[as.character(min(as.numeric(endpoints)))]] <-
            as.character(max(as.numeric(endpoints)))
        ## don't allow the same square to be in a chute or ladder
        ## more than once
        ## so, remove the drawn squares from those to sample
        squares <- squares[-which(squares %in% endpoints)]
    }
    return(board)
}

NumToCoord <- function(num){
    num <- as.numeric(num)
    ## square height
    y <- floor((num-1)/10) + 1
    ## whether to count right to left or vice versa
    ## depends on if the row is odd or even
    if(y %% 2 == 1){
        ## count left to right
        x <- num - ((y - 1) * 10)
    } else {
        ## count right to left
        x <- 11 - (num - ((y - 1) * 10))
    }
    return(c(x, y))
}

PlotGameBoard <- function(board){
    ## set up board with grid lines
    plot(0:10, 0:10, type="n", axes=F, xlab="", ylab="")
    for(i in 0:10){
        lines(rep(i,2), c(0, 10))
        lines(c(0, 10), rep(i,2))
    }
    for(key in hash::keys(board)){
        startpt <- NumToCoord(board[[key]]) - .5
        endpt <- NumToCoord(key) - .5
        points(c(startpt[1], endpt[1]), c(startpt[2], endpt[2]))
        if(as.numeric(board[[key]]) < as.numeric(key)){
            ## if it's a chute
            lines(c(startpt[1], endpt[1]), c(startpt[2], endpt[2]), lwd = 2)
        } else {
            ## if it's a ladder
            lines(c(startpt[1], endpt[1]), c(startpt[2], endpt[2]), lty = 2)
        }
    }
}

takeStep <- function(insquare, board){
    ## one dice roll
    step <- sample(1:6, 1)
    outsquare <- insquare + step
    ## account for chutes/ladders
    if(as.character(outsquare) %in% hash::keys(board)){
        outsquare <- as.numeric(board[[as.character(outsquare)]])
    }
    return(outsquare)
}

runGame <- function(board, outfile = NA, exact100 = TRUE){
    ## if exact100 is TRUE, you have to roll to precisely hit 100
    ## if FALSE, rolling past 100 counts as 100, and a win.
    game <- 0
    step <- 1

    while(game[step] != 100){
        step <- step + 1
        game[step] <- takeStep(game[step-1], board)
        if(exact100){
            ## stay at previous square if you go past 100
            if(game[step] > 100) game[step] <- game[step-1]
        } else {
            if(game[step] > 100) game[step] <- 100
        }
    }
    
    if(!is.na(outfile)){
        write.table(t(as.matrix(game)), outfile, col.names = FALSE,
                    row.names = FALSE, append = TRUE) 
    } else {
        return(game)
    }
}

runNGames <- function(randomBoard = TRUE, exact100 = TRUE,
                      N = 100, chutes = 9, ladders = 10,
                      sim = NULL){
    ## build file name to write simulation results to
    if(!is.null(sim)){
        outfile <- paste0('../Data/simulation-chutes-', chutes, '-ladders-',
                          ladders, '-random-', randomBoard*1, '-exact-',
                          exact100*1, '-sim-', sim, '.txt')
    } else {
        outfile <- paste0('../Data/simulation-chutes-', chutes, '-ladders-',
                          ladders, '-random-', randomBoard*1, '-exact-',
                          exact100*1, '.txt')
    }
    if(randomBoard){
        board <- SetUpRandomBoard(chutes = chutes, ladders = ladders)
        save(board, file = paste0(outfile, '.board.RData'))
    } else {
        originalBoard <- data.frame(
            insquare = c(98, 95, 93, 87, 64, 62, 56, 49, 48, 16,
                1, 4, 9, 21, 28, 36, 51, 71, 80),
            outsquare = c(78, 75, 73, 24, 60, 19, 53, 11, 26, 6,
                38, 14, 31, 42, 84, 44, 67, 91, 100))
        originalBoard$insquare <- as.character(originalBoard$insquare)
        originalBoard$outsquare <- as.character(originalBoard$outsquare)
        board <- hash::hash()
        for(i in 1:nrow(originalBoard)){
            board[[originalBoard$insquare[i]]] <- originalBoard$outsquare[i]
        }
    }
    for(i in 1:N){
        if(i %% 1000 == 0) print(i)
        runGame(board, outfile, exact100 = exact100)
    }
}

runAllSims <- function(){
    set.seed(83811)
    for(i in 1:50){
        print(i)
        runNGames(randomBoard = TRUE, exact100 = TRUE, N = 1000,
                  sim = as.character(i))
        runNGames(randomBoard = TRUE, exact100 = FALSE, N = 1000,
                  sim = as.character(i))
    }
    runNGames(randomBoard = FALSE, exact100 = TRUE, N = 1000)
    runNGames(randomBoard = FALSE, exact100 = FALSE, N = 1000)
}
