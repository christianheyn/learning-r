xs <- 0:6

# BLAU
fnOrigin <- function(x) return(x^2)

# GRAU
fnTry <- function (x) return((6 * x) - 5)

ys <- mapply(fnOrigin, xs)
ys.fn <- mapply(fnTry, xs)

err <- function(data) {
    data.mean = mean(data)
    serr = mapply(function(d) return(abs(d - data.mean) ^ 2), data)
    return(sum(serr))
}

ys.err = err(ys)
ys.err

ys.fn.pre = sum(mapply(function(d) {
    return(d)
}, ys))

ys.fn.err = err(ys.fn.pre)
ys.fn.err

plot(
    x=0,
    y=0,
    xlim=c(0,7),
    ylim=c(0,40),
    type="n"
  )

lines(x= xs, y=ys, col="#0099ff", lwd=2, type="l")
lines(x= xs, y=ys.fn, col="#cccccc", lwd=2, type="b")
