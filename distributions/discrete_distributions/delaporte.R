# http://en.wikipedia.org/wiki/Delaporte_distribution

# Parametrization used in gamlss.dist
# mu > 0
# sigma > 0
# 0 < nu < 1
delaporte_msn <- function(mu, sigma, nu,
                          text = NULL, 
                          param_names = NULL,
                          param_values = NULL) {
    
    
    if (is.null(text)) {
        text <- helpText(
            p(
                "The random variable \\(X\\) has a", 
                a(href = "http://en.wikipedia.org/wiki/Delaporte_distribution", 
                  "Delaporte distribution"),
                "if its probability mass function can be written as 
                $$
                    \\text{P}(X = k)
                    = f(k; \\mu, \\sigma, \\nu)
                    = \\frac{e^{-\\mu \\nu}}{\\Gamma(1 / \\sigma)}
                      \\left[ 1 + \\mu \\sigma (1 - \\nu) \\right]^{-1 / \\sigma} S
                $$
                where
                $$
                S = \\sum_{j = 0}^{k} 
                    {k \\choose j} 
                    \\frac{\\mu^k \\nu^{k - j}}{k!}
                    \\left[ \\mu + \\frac{1}{\\sigma (1 - \\mu)} \\right]^{-j} \\!
                    \\Gamma \\left( \\frac{1}{\\sigma} + j \\right)
                $$
                with \\(k = 0, 1, 2, \\ldots \\), 
                \\(\\mu > 0, \\sigma > 0 \\), 
                and \\(0 < \\nu < 1 \\).
                The mean and variance of \\(X\\) are given by 
                $$  \\begin{align*} 
                    \\text{E}[X] &= \\mu \\\\ 
                    \\text{Var} (X) &= \\mu + \\mu^2 \\sigma (1 - \\nu)^2
                    \\end{align*}
                $$
            "
            ),
            p(
                "The parametrization by \\(\\mu, \\sigma, \\nu\\) is the one used
                 in the R package ",
                a(href = "http://cran.r-project.org/web/packages/gamlss.dist/index.html", 
                  "gamlss.dist,"),
                "and can be obtained from that with \\(\\alpha, \\beta, \\lambda\\) by
                 setting 
                $$ \\begin{align*}
                   \\mu &= \\lambda + \\alpha \\beta \\\\
                   \\sigma &= \\frac{1}{\\alpha} \\\\
                   \\nu &= \\frac{\\lambda}{\\lambda + \\alpha \\beta}
                   \\end{align*}
                $$"
            ),
            h4("Bibliography"),
            p(
                a(href = "http://en.wikipedia.org/wiki/Delaporte_distribution", 
                  "Delaporte distribution (Wikipedia)"), br(),
                a(href = "http://cran.r-project.org/web/packages/gamlss.dist/index.html",
                  "gamlss.dist (R package)")
            )
        )
    }
    
    if (is.null(param_names)) {
        param_names <- c("\\mu", "\\sigma", "\\nu")
        param_values <- c(mu, sigma, nu)
    }
    
    mean <- mu
    variance <- mu + mu^2 * sigma * (1 - nu)
    
    stats <- stats_text(param_names, param_values, mean, variance)
    
    supp <- 0:15
    dens <- gamlss.dist::dDEL(supp, mu, sigma, nu)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                text = text, stats = stats))
}

# Parametrization used in Wikipedia article
# alpha, beta, lambda > 0
delaporte_abl <- function(alpha, beta, lambda) {
    
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Delaporte_distribution", "Delaporte distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\alpha, \\beta, \\lambda)
            = \\sum_{j = 0}^{k}
              \\frac{\\Gamma(\\alpha + j)}{\\Gamma(\\alpha) j!}
              \\frac{\\beta^j}{(1 + \\beta)^{\\alpha + j}}
              \\frac{\\lambda^{k - j}}{(k - j)!}
              \\exp(-\\lambda)
            $$
            with \\(k = 0, 1, 2, \\ldots \\), and \\(\\alpha, \\beta, \\lambda > 0\\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\lambda + \\alpha \\beta \\\\ 
                \\text{Var} (X) &= \\lambda + \\alpha \\beta (1 + \\beta)
                \\end{align*}
            $$
            "
        ),
        p(
            "The Delaporte distribution can be seen as a compound distribution
            for a ",
            a(href = "http://en.wikipedia.org/wiki/Poisson_distribution", 
              "Poisson random variable,"),
             "with a mean parameter consisting of a fixed component \\(\\lambda\\)
              and a random (prior) part consisting of a ",
            a(href = "http://en.wikipedia.org/wiki/Gamma_distribution", 
              "gamma distributed"),
            "random variable with parameters \\(\\alpha\\) and \\(\\beta\\).",
            "One area of use for the Delaporte distribution is actuarial science,
            to model insurance claims."
        ),
        p( "The parametrization by \\(\\alpha, \\beta, \\lambda\\) 
            can be obtained from that with \\(\\mu, \\sigma, \\nu\\) by
            setting 
            $$ \\begin{align*}
                \\alpha &= \\frac{1}{\\sigma} \\\\
                \\beta &= \\mu \\sigma (1 - \\nu) \\\\
                \\lambda &= \\mu \\nu
               \\end{align*}
            $$"
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Delaporte_distribution", 
              "Delaporte distribution (Wikipedia)"), br(),
            a(href = "http://cran.r-project.org/web/packages/gamlss.dist/index.html",
              "gamlss.dist (R package)")
        )
    )
    
    
    param_names <- c("\\alpha", "\\beta", "\\lambda")
    param_values <- c(alpha, beta, lambda)
    
    mu <- lambda + alpha * beta
    sigma <- 1 / alpha
    nu <- lambda / (lambda + alpha * beta)
    return(delaporte_msn(mu, sigma, nu,
                         text, param_names, param_values))
}
