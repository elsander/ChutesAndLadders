require(dplyr)
require(stringr)
require(tools)

ParseName <- function(fname){
    ## remove path to file, if present
    fname <- basename(fname)
    ## strip file extension
    fname <- tools::file_path_sans_ext(fname)
    
    fdata <- stringr::str_split(fname, '-')[[1]]
    if(fdata[7] == '1'){
        btype <- 'random'
        bnum <- as.numeric(fdata[11])
    } else {
        btype <- 'original'
        ## -1 will be a flag for the original board in the board number
        bnum <- -1
    }
    fdf <- data.frame(chutes = as.numeric(fdata[3]),
                      ladders = as.numeric(fdata[5]),
                      board_type = btype,
                      exact_roll = as.numeric(fdata[9]),
                      board_num = bnum)
    return(fdf)
}

MakeGameLenDf <- function(gameLengths){
    lendf <- data.frame(game_length = rep(NA, length(unique(gameLengths))),
                        frequency = rep(NA, length(unique(gameLengths))))
    for(i in 1:length(unique(gameLengths))){
        lendf$game_length[i] <- unique(gameLengths)[i]
        lendf$frequency[i] <- length(which(gameLengths == unique(gameLengths)[i]))
    }
    return(lendf)
}

ParseOneFile <- function(fname){
    tmpdf <- ParseName(fname)
    ## read file in as a list of character vectors
    rawData <- scan(fname, what = character(), sep = '\n')
    ## convert list of strings to vector of game lengths
    rawData <- unlist(lapply(rawData, function(vec){
        vec <- stringr::str_split(vec, ' ')[[1]]
        return(length(vec))
    }))
    ## create dictionary of game lengths
    gameLenDf <- MakeGameLenDf(rawData)
    gameLenDf$board_num <- rep(tmpdf$board_num, nrow(gameLenDf))
    gameLenDf <- dplyr::full_join(tmpdf, gameLenDf, by = 'board_num')
    return(gameLenDf)
}

ParseAllFiles <- function(path, outfile = NA){
    fs <- list.files(path)
    gameLenDf <- numeric(0)
    for(f in fs){
        ## don't read in the board files
        if(length(grep('RData', f)) > 0) next
        ## if growing the df takes too long, can preallocate
        tmpdf <- ParseOneFile(file.path(path, f))
        if(length(gameLenDf) == 0){
            gameLenDf <- tmpdf
        } else {
            gameLenDf <- dplyr::bind_rows(gameLenDf, tmpdf)
        }
    }
    if(!is.na(outfile)){
        write.csv(gameLenDf, outfile, row.names = FALSE, quote = FALSE)
    }
    return(gameLenDf)
}
