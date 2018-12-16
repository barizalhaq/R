
# Metode Trapesium

myf <- function(x){
    return(4*x-x^2)
} 

trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
   
  h <- b - a
   
  fxdx <- (h / 2) * (f(a) + f(b))
   
  return(fxdx)
}
trapezoid(myf, 0, 1)

# Metode Trapesium
library(ggplot2)
f <- function(x) {
  return(4*x-x^2)
}
 
df <- data.frame(cbind(c(0, 1, 1, 0), c(0, f(1), 0, 0)))

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = f, size = 1.05, alpha = 0.75, color='blue') + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = f(1))) + 
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = f(1))) + 
  geom_polygon(data = df, aes(x=X1, y=X2), fill = 'blue', alpha = 0.2) + 
  geom_area(stat = 'function', fun = f, fill = 'black', alpha = 0.3, xlim = c(0, 1)) + 
  xlim(c(-0.5,1))

# Simpson rule
library(pracma)
myf <- function(x, n){
    return(4*x-x^2/(x+n))
} 
simpadpt(myf, 0, 1, n = 10) 

myf <- function(x){
    return(4*x-x^2)
} 

simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
   
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
   
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
   
  return(s)
}
 
simpsons.rule(myf, 0, 1)
