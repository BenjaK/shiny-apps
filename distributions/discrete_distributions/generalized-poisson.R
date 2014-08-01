
# genpoisson, p. 276 of VGAM.pdf 
dgenp <- function(k, theta, lambda) {
    a <- theta * (theta + lambda * k)^(k - 1)
    b <- exp(-(theta + lambda * k)) / factorial(k)
}