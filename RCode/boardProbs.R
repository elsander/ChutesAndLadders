## Code to calculate probabilities of landing on a square at a time step, and
## analytically calculate expected game length (this part is currently broken).

require(reshape2)
require(ggplot2)
require(dplyr)
source('chutesLaddersSimulation.R')

## HARDCODED
isSource <- function(square){
    sources <- c(98,95,93,87,64,62,56,49,48,16,1,4,9,21,28,36,51,71,80)
    if(square %in% sources){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

## HARDCODED
isSink <- function(square){
    sinks <- c(78,75,73,24,60,19,53,11,26,6,38,14,31,42,84,44,67,91,100)
    if(square %in% sinks){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

nextStepProbs <- function(prevStep, accumulate = TRUE){
    nsquares <- length(prevStep)
    nextStep <- rep(0, nsquares)
    ## start at 2 because p(1) is always 0 after the first step
    for(i in 2:(nsquares-1)){
        ## prevents bad sequences for low board numbers
        lastStepSquares <- max(1, i-6):(i-1)
        nextStep[i] <- (1/6)*sum(prevStep[lastStepSquares])
    }
    ## deal with dice rolls that go past the last square
    if(accumulate){
        ## let p(100) at time t add to p(100) at t+1 to incorporate
        ## accumulated probability that game has ended
        nextStep[nsquares] <- sum(prevStep[(nsquares-6):nsquares]*
                                  c(1/6, 2/6, 3/6, 4/6, 5/6, 1, 1))
    } else {
        ## We should not do this accumulation if we are trying to
        ## calculate the expected game length
        nextStep[nsquares] <- sum(prevStep[(nsquares-6):(nsquares-1)]*
                                  c(1/6, 2/6, 3/6, 4/6, 5/6, 1))
        ## normalize: I don't think we should do this
        ##nextStep <- nextStep/sum(nextStep)
    }
    return(nextStep)
}

getBoardProbs <- function(steps = 100, accumulate = TRUE){
    boardprobs <- matrix(0, steps, 100)
    ## first step
    boardprobs[1,1:6] <- 1/6
    for(i in 2:steps){
        ## let chutes and ladders take effect from previous step
        prevStep <- boardprobs[i-1,]
        for(j in 1:length(prevStep)){
            if(isSource(j)){
                ## add probability of this square to its sink for
                ## the next step
                prevStep[board(j)] <- prevStep[board(j)] + prevStep[j]
                ## and remove the probability from the source square
                prevStep[j] <- 0
            }
        }
        boardprobs[i,] <- nextStepProbs(prevStep, accumulate = accumulate)
    }
    return(boardprobs)
}

mTrialProb <- function(oneTrialProb, m){
    return(1-(1-oneTrialProb)^m)
}

expectation <- function(probs){
    return(sum(probs*(1:length(probs))))
}

gameLength <- function(m = 1, steps = 100000){
    ## recycling code from getBoardProbs but saving memory
    ## by only keeping t=100
    prevStep <- rep(0, 100)
    probOfWin <- rep(0, steps)
    ## first step
    prevStep[1:6] <- 1/6
    for(i in 2:steps){
        ## let chutes and ladders take effect from previous step
        for(j in 1:length(prevStep)){
            if(isSource(j)){
                ## add probability of this square to its sink for
                ## the next step
                prevStep[board(j)] <- prevStep[board(j)] + prevStep[j]
                ## and remove the probability from the source square
                prevStep[j] <- 0
            }
        }
        prevStep <- nextStepProbs(prevStep, accumulate = FALSE)
        probOfWin[i] <- mTrialProb(prevStep[100], m = m)
        if(i %% 1000 == 0) print(i)
    }
    ## here is the estimate of the expectation
    print(expectation(probOfWin))
    return(probOfWin)
}

plotBoardProbsByStep <- function(boardprobs, outfile = NA){
    meltProbs <- melt(boardprobs)
    meltProbs$Var1 <- factor(meltProbs$Var1)
    g <- ggplot(meltProbs, aes(x = Var2, y = value)) +
        geom_line(aes(group = Var1, colour = Var1))

    if(is.na(outfile)){
        plot(g)
    } else {
        pdf(outfile)
        plot(g)
        dev.off()
    }
}

plotBoardProbsBySquare <- function(boardprobs, outfile = NA){
    meltProbs <- melt(boardprobs)
    meltProbs$Var2 <- factor(meltProbs$Var2)
    meltProbs <- dplyr::rename(meltProbs, Step = Var1)
    meltProbs <- dplyr::rename(meltProbs, Square = Var2)
    meltProbs <- dplyr::rename(meltProbs, Probability = value)
    g <- ggplot(meltProbs, aes(x = Step, y = Probability)) +
        geom_line(aes(group = Square, colour = Square))

    if(is.na(outfile)){
        plot(g)
    } else {
        pdf(outfile)
        plot(g)
        dev.off()
    }
}
