## simulate many runs of games to check analytical results.

board <- function(insquare){
    ## chutes
    if(insquare == 98) return(78)
    if(insquare == 95) return(75)
    if(insquare == 93) return(73)
    if(insquare == 87) return(24)
    if(insquare == 64) return(60)
    if(insquare == 62) return(19)
    if(insquare == 56) return(53)
    if(insquare == 49) return(11)
    if(insquare == 48) return(26)
    if(insquare == 16) return(6)

    ## ladders
    if(insquare == 1) return(38)
    if(insquare == 4) return(14)
    if(insquare == 9) return(31)
    if(insquare == 21) return(42)
    if(insquare == 28) return(84)
    if(insquare == 36) return(44)
    if(insquare == 51) return(67)
    if(insquare == 71) return(91)
    if(insquare == 80) return(100)

    ## if these conditions not met, stay where you are
    return(insquare)
}

takeStep <- function(insquare){
    ## one dice roll
    step <- sample(1:6, 1)
    outsquare <- insquare + step
    ## account for chutes/ladders
    outsquare <- board(outsquare)
    return(outsquare)
}

runGame <- function(outfile = NA){
    ## here I am assuming that if you roll to move past 100, you will
    ## reach 100 and win, rather than needing to roll exactly.
    ## This will tend to shorten game time.
    game <- 0
    step <- 1

    while(game[step] != 100){
        step <- step + 1
        game[step] <- takeStep(game[step-1])
        if(game[step] > 100) game[step] <- 100
    }
    
    if(!is.na(outfile)){
        write.table(t(as.matrix(game)), outfile, col.names = FALSE,
                    row.names = FALSE, append = TRUE) 
    } else {
        return(game)
    }
}

runNGames <- function(N = 100, outfile = '../Data/gameSimulations1.txt'){
    for(i in 1:N){
        if(i %% 1000 == 0) print(i)
        runGame(outfile)
    }
}
