require(ggplot2)

simulationDensityPlot <- function(fname){
    gamelen <- read.csv(fname, header = TRUE)
    gamelen$board_num <- as.factor(gamelen$board_num)
    g <- ggplot(gamelen, aes(x = game_length, y = frequency,
                             colour = board_num, fill = board_num)) +
                                 geom_density(alpha = .1, position = "identity",
                                          stat = 'identity') +
                                              facet_grid(board_type~.)
    plot(g)
    return(g)
}

## TODO:
## overlay original in an easy-to-see color
## plot distribution of means
## distribution of variances
