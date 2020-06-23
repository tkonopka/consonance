# helper function to draw diagrams for the consonance vignette

RcssDefaultStyle <- Rcss("consonance.css")

anscombe.xlim <- range(unlist(anscombe[c("x1", "x2", "x3", "x4")]))
anscombe.ylim <- range(unlist(anscombe[c("y1", "y2", "y3", "y4")]))

# expects a data frame with (x, y) coordinates
plot_anscombe <- function(d, xlab="", ylab="", main="",
                          xlim=anscombe.xlim, ylim=anscombe.ylim,
                          model="linear", shade.range=FALSE, Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(Rcssclass=Rcssclass)
  parplot(anscombe.xlim, anscombe.ylim, xlim=xlim, ylim=ylim)
  if (shade.range) {
    xrange <- range(d$x)
    yaxs <- graphics::par()$usr[3:4]
    rect(xrange[1], yaxs[1], xrange[2], yaxs[2], Rcssclass="range")
  }
  box()
  axis(1, lwd=0, Rcssclass="x")
  axis(2, lwd=0, Rcssclass="y")
  if ("linear" %in% model) {
    dlm <- lm(y~x, data=d)
    lines(xlim,
          dlm$coefficients[1] + anscombe.xlim*dlm$coefficients[2],
          Rcssclass="linear")
  }
  if ("quadratic" %in% model) {
    dqm <- lm(y~x+I(x^2), data=d)
    xvals <- seq(xlim[1], xlim[2], length=128)
    yvals <- predict(dqm, data.frame(x=xvals))
    lines(xvals, yvals, Rcssclass="quadratic")
  }
  points(d$x, d$y)
  mtext(side=1, xlab, Rcssclass="x")
  mtext(side=2, ylab, Rcssclass="y")
  mtext(side=3, main, Rcssclass="main")
  ctext(d$id[1], Rcssclass="corner")
}


# expects a data frame from microbenchmark
plot_runtimes <- function(d, xlim=c(0, max(d)),
                          main="", corner="",
                          xlab="median running times (micro-sec.)",
                          Rcssclass=NULL) {
  d <- as.data.frame(d)
  d.labels <- rev(as.character(levels(d$expr)))
  # convert from nanosecond to microseconds
  n = 1000
  d.values <- split(d$time/n, d$expr)
  d.medians <- sapply(d.values, median)[d.labels]
  d.q75 <- sapply(d.values, quantile, p=0.75)[d.labels]

  RcssCompulsoryClass <- "runtimes"
  par(Rcssclass=Rcssclass)
  y <- barplot(d.medians, xlim=xlim/n, names=FALSE)
  axis(1, Rcssclass="x")
  axis(2, labels=gsub("_", " ", d.labels), at=y[,1], Rcssclass="y")
  mtext(side=1, xlab, Rcssclass="x")
  mtext(side=3, main, Rcssclass="main")
  ctext(corner, Rcssclass="corner")
}

