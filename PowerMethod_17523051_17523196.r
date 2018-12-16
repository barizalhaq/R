
powerMethod <- function(A, v = NULL, eps = 1e-6, maxiter = 100, plot=FALSE)
{
  if (!is_square_matrix(A))
    stop("'powerMethod()' requires a square numeric matrix")

  if (!is.null(v))
  {
    if (!is.vector(v) || !is.numeric(v))
      stop("'powerMethod()' requires 'v' to be a numeric vector")
    if (nrow(A) != length(v))
      stop("'A' is not conformable with 'v' in 'powerMethod()'")
  } else {
    v = rep(1, nrow(A))
  }

  if (!eps > 0)
    eps = 1e-6

  v_old = v
  steps = 1
  vectors <- list(v)
  repeat
  {
    v_new = A %*% v_old
    v_new = v_new / len(v_new)
    if (len(abs(v_new) - abs(v_old)) <= eps) break
    v_old = v_new
    steps = steps + 1
    vectors[[steps]] <- c(v_new)
    if (steps == maxiter) break
  }
  # Rayleigh quotient gives the eigenvalue
  lambda = sum((A %*% v_new) * v_new)
  # output
  res <- list(iter = steps, vector = v_new, value = lambda)
  vectors <- do.call(cbind, vectors)
  colnames(vectors) <- paste0("v", 1:ncol(vectors))
  res <- c(vector_iterations=list(vectors), res)
  if(plot){
      vecs <- t(vectors)
      pos <- c(min(c(vecs, 0))-.1, max(vecs) + .1)
      plot(pos, pos, type="n", xlab="x1", ylab="x2")
      col <- sapply(1:nrow(vecs) / nrow(vecs), 
      			  function(x) grDevices::adjustcolor('red', x))
      vectors(vecs, col=col)
      abline(h=0, v=0, col="gray")
      return(invisible(res))
  }
  res
}

C <- matrix(c(4, -5, 2, -3), 2, 2, byrow = T)
eigen(C)$vectors
powerMethod(C, v = c(1,0))

C <- matrix(c(0, 11, -5, -2, 17, -7, -4, 26, -10), 3, 3)
eigen(C)$vectors
powerMethod(C)
