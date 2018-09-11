source("helper/map.r")

# Input
xs <- 0:6
# Origin Function
fnOrigin <- function(x) return(x^2)
# Test Function
fnTest <- function (x) { return((6 * x) - 5) }

ys <- map(fnOrigin, xs)
ys.fn <- map(fnTest, xs)

err <- function(data) {
    data.mean = mean(data)
    serr = mapply(function(d) return(abs(d - data.mean) ^ 2), data)
    return(sum(serr))
}

ys.err = err(ys)

ys.fn.pre = map.i(function(v, i) {
    return(abs(ys[i] - v) ^ 2)
}, ys.fn)

ys.fn.err = sum(ys.fn.pre)

R2 = 1 - (ys.fn.err / ys.err)

plot(
    x=0,
    y=0,
    xlim=c(0,max(xs)),
    ylim=c(0,max(ys)),
    type="n",
  )

lines(x= xs, y=ys, col="#0099ff", lwd=2, type="l")
lines(x= xs, y=ys.fn, col="#cccccc", lwd=2, type="b")

legend(
    0.5,
    max(ys),
    legend=c("Origin", "Tryed", paste("R2: ", round(R2, digits=4))),
    col=c("#0099ff", "#cccccc", "#ffffff"),
    lty=1,
    lwd=2
)
