## code to read in and analyze simulated data (data not included on github because
## of large file size)

x <- scan('../Data/gameSimulations1.txt', sep = '\n', what = '')
x <- strsplit(x, split = ' ')
y <- unlist(lapply(x, length))
print(mean(y))

z <- vector(mode = 'list', length = 100000)
z <- unlist(lapply(z, function(z){
    a <- sample(1:length(y),1)
    b <- sample(1:length(y),1)
    return(min(y[a], y[b]))
}))
hist(z)
print(mean(z))

z3 <- vector(mode = 'list', length = 100000)
z3 <- unlist(lapply(z, function(z){
    a <- sample(1:length(y),1)
    b <- sample(1:length(y),1)
    d <- sample(1:length(y),1)
    return(min(y[a], y[b], y[d]))
}))
print(mean(z3))
