# Functions for calculating ball drop probabilities
# -------------------------------------------------------------------------


#' Constant vector of layer widths
WIDTHS <- c(7:8, 11:13, rep(14:15, 5))

#' Approximate Distribution Function for a Double-Reflected Normal
#' 
#' A "double-reflected" normal is a normal distribution whose probability
#' mass is restricted to an interval [a, b], and the tails are reflected by
#' these endpoints.
#'
#' @param x quantile
#' @param a left bound
#' @param b right bound
#' @param K number of terms to include
#' @param ... parameters passed to `pnorm`
#'
pdrnorm <- function(x, a, b, K = 2, ...) {
  if (x < a) return(0)
  if (x > b) return(1)
  ks <- 0:K
  x1 <- x - 2*ks*(x - a) - 2*ks*(b - x)
  x2 <- x - 2*(ks + 1)*(x - a) - 2*ks*(b - x)
  x3 <- x + 2*(ks + 1)*(x - a) + 2*(ks + 1)*(b - x)
  x4 <- x + 2*ks*(x - a) + 2*(ks + 1)*(b - x)
  sum(pnorm(x1, ...) - pnorm(x2, ...) + pnorm(x3, ...) - pnorm(x4, ...))
}

#' Transition Matrix Between Layers
#' 
#' Calculates the transition matrix between a layer and the following layer,
#' assuming a double-reflected normal distribution around each bin.
#'
#' @param layer number of the start layer (1-14)
#' @param ... additional parameters passed to `pdrnorm`
#'
tdist_layer <- function(layer, ...) {
  m <- matrix(0, nrow = WIDTHS[layer], ncol = WIDTHS[layer + 1])
  or <- WIDTHS[layer] + 1
  oc <- WIDTHS[layer + 1]
  for (r in 1:WIDTHS[layer]) {
    for (c in 1:WIDTHS[layer + 1]) {
      m[r, c] <- pdrnorm(2*c - oc, -oc, oc, mean = 2*r - or, ...) - 
        pdrnorm(2*c - oc - 2, -oc, oc, mean = 2*r - or, ...)
    }
  }
  m
}

#' Full Transition Matrix
#' 
#' Transition matrix for a complete ball drop, starting from layer 1 and ending
#' in the bottom, layer 15.
#'
#' @param sd standard deviation of the underlying normal distribution
#'
tdist_full <- function(sd) {
  ms <- lapply(1:14, tdist_layer, sd = sd)
  mfull <- Reduce(`%*%`, ms, diag(7))
  colnames(mfull) <- 1:15
  rownames(mfull) <- 1:7
  as_tibble(mfull) %>% 
    rownames_to_column(var = "start") %>% 
    pivot_longer(-start, "end", values_to = "prob") %>% 
    mutate(start = factor(start, levels = unique(end)),
           end   = factor(end, levels = unique(end)))
}
