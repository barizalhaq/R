#Nomor 1
x <- c(50,51,52,53,54)
y <- c(40,46,44,55,49)
linearMod <- lm(y ~ x)  # build linear regression model on full data
print(linearMod)
predict(linearMod, data.frame(x = 55))

#Nomor 2
x <- c(0,1,2,3,4)
y <- c(1,2.25,3.75,4.25,5.65)
reg <- poly(y ~ x)
print(reg)

#Nomor 11
trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- b - a
  
  fxdx <- (h / 2) * (f(a) + f(b))
  
  return(fxdx)
}
myf <- function(x, n){
  return(x^2-6)
}
trapezoid(myf, 0, 1)

#Nomor 12
myf2 <- function(x, n){
  return(x^3*4*x^2-10)
}
trapezoid(myf2, 1, 2)

#Nomor 14
h <- 0.1
x <- seq(0, 1, by=h)
f <- function(x){
  return(x^2)
}

f0 <- f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L <- h * (f0 + 2 *sum(fi) + fn) / 2
  return(L)
}
trap(f0,fi, fn, h)

#Nomor 15
h <- 0.2
x <- seq(0, 1, by=h)
f <- function(x){
  return(x^2)
}

f0 <- f(x[1])
fi <- sapply(x[2:5], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L <- h * (f0 + 2 *sum(fi) + fn) / 2
  return(L)
}
trap(f0,fi, fn, h)