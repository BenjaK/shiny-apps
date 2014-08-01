# page 115 of gamlss.dist manual
# Univariate discrete distributions p. 485

sichel <- function(mu, sigma, nu) {
    
    text <- helpText("d")
    
    c <- DistributionUtils::besselRatio(x = 1 / sigma, 
                                        nu = nu, orderDiff = 1)
    mean <- mu
    variance <- mu^2 * (2 * sigma * (nu + 1) / c + 1 / c^2 - 1)
    
    stats <- stats_text(c("\\mu", "\\sigma", "\\nu"), 
                        c(mu, sigma, nu), 
                        mean, variance)
    
    supp <- 0:30
    dens <- gamlss.dist::dSI(supp, mu, sigma, nu)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                text = text, stats = stats))
}

# discrete_plot(sichel(0.5, 0.02, -0.5))
