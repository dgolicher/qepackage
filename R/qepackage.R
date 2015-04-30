#' Extended pairs plot.
#' 
#' @param x A matrix.

#' @return A figure.
#' @examples
#' XPairs(d)


Xpairs<-function (...) {
  require(mgcv)
  panel.line<-function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                        cex = 1, ...) 
  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      md <- gam(y ~ s(x,k=3))
    xx <- seq(min(x), max(x), length = 100)
    yy <- predict(md, data.frame(x = xx), se = TRUE, type = "response")
    lines(xx, yy$fit, col = 1)
    lines(xx, yy$fit + 2 * yy$se.fit, col = 3, lty = 2)
    lines(xx, yy$fit - 2 * yy$se.fit, col = 2, lty = 2)
  }
  
  panel.hist<-function (x, ...) 
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
  }
  
  panel.cor<-function (x, y, digits = 2, prefix = "", cex.cor) 
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r * 1.2)
  }
  
  pairs(..., lower.panel = panel.line, upper.panel = panel.cor,  diag.panel = panel.hist)
}

cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

ablines<-function(mod,type="confidence",...){
  a<-names(mod$model)[2] 
  b<-paste("newdata <- data.frame(",a,"=seq(min(",a,"),max(",a,"),l=100))",sep="") 
  eval(parse(text=b))
  a<-predict(mod,newdata,interval=type)
  matlines(newdata[,1],a,...)
}

addRsq<-function(mod,pos="topleft",digits=3){
  a<-round(summary(mod)$adj.r.squared,digits) 
  a<-bquote(R[adj]^2 == .(a)) 
  legend(pos,legend=a,bty="n") 
}

ablines2<-function (mod, ...)
{
  a <- names(mod$model)[2]
  b <- paste("newdata <- data.frame(", a, "=seq(min(", a, "),max(",
             a, "),l=100))", sep = "")
  eval(parse(text = b))
  a <- predict(mod, newdata, type="response",se=TRUE)
  a$upper=a$fit+a$se.fit*2
  a$lower=a$fit-a$se.fit*2
  a<-data.frame(a$fit,a$upper,a$lower)
  matlines(newdata[, 1], a,...)
}
