map.i <- function(fn, data) {
    result <- c()
    index <- 1
    for (value in data){
        result[index] <- fn(value, index)
        index <- index + 1
    }
    return(result)
}

map <- function(fn, data) {
    result <- c()
    index <- 1
    for (value in data){
        result[index] <- fn(value)
        index <- index + 1
    }
    return(result)
}