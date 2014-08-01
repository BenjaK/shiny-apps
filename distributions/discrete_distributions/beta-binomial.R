# http://en.wikipedia.org/wiki/Beta-binomial_model


# Uses parametrization with 
# mu = a / (a + b), 0 < mu < 1
# sigma = 1 / (a + b), sigma > 0
beta_binomial_mu_sigma <- function(mu, sigma, n, 
                                   text = NULL, param_names = NULL, param_values = NULL) {
    
    if (is.null(text)) {
        text <- helpText(
            p(
                "A random variable \\(X\\) is said to have a",
                a(href = "http://en.wikipedia.org/wiki/Beta-binomial_distribution", "Beta-Binomial distribution"),
                "if its probability mass function can be represented as 
                $$
                \\text{P}(X = k)
                = f(k; n, \\mu, \\sigma)
                = {n \\choose k} 
                  \\frac{ \\text{B} \\left(k + \\frac{\\mu}{\\sigma}, n - k + \\frac{1 - \\mu}{\\sigma} \\right) }
                        { \\text{B} \\left( \\frac{\\mu}{\\sigma}, \\frac{1 - \\mu}{\\sigma} \\right)}
                $$
                with \\(k = 0, 1, \\ldots, n \\), \\(0 < \\mu < 1 \\) and \\( \\sigma > 0\\).
                \\( \\text{B}(\\cdot ~, \\cdot) \\) is here the",
                a(href = "http://en.wikipedia.org/wiki/Beta_function", "Beta function."),
                "The mean and variance of \\(X\\) are given by 
                $$  \\begin{align*} 
                    \\text{E}[X] &= n \\mu \\\\ 
                    \\text{Var} (X) &= n \\mu (1 - \\mu) \\left[1 + (n - 1) \\frac{\\sigma}{1 + \\sigma} \\right] 
                    \\end{align*}
                $$
                From this it is clear that \\( \\mu \\) controls the mean, while \\( \\sigma \\) is a dispersion 
                parameter.
                This parametrization can be obtained from that with \\( \\alpha \\) and \\( \\beta \\) by setting
                \\( \\mu = \\frac{ \\alpha }{ \\alpha + \\beta } \\) and \\( \\sigma = \\frac{1}{ \\alpha + \\beta } \\)."
            ),
            p(
                "In a Bayesian framework, the Beta-Binomial distribution appears as the posterior 
                 predictive distribution for a binomial random variable with a Beta prior 
                 placed on the success probability."
            ),
            h4("Bibliography"),
            p(
                a(href = "http://en.wikipedia.org/wiki/Beta-binomial_model", 
                  "Beta-binomial distribution (Wikipedia)"), br(),
                a(href = "http://cran.r-project.org/web/packages/gamlss.dist/index.html",
                  "gamlss.dist (R package)")
            )
        )
    }
    
    if (is.null(param_names)) {
        param_names <- c("\\mu", "\\sigma")
    }
    
    if (is.null(param_values)) {
        param_values <- c(mu, sigma)
    }
    
    mean <- n * mu
    variance <- n * mu * (1 - mu) * (1 + n * sigma) / (1 + sigma)
    
    stats <- stats_text(param_names, param_values, mean, variance)
    
    supp <- 0:n
    dens <- gamlss.dist::dBB(x = supp,
                             mu = mu,
                             sigma = sigma,
                             bd = n)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                ymax = 1,
                text = text,
                stats = stats))
}


# Parametrization used in Wikipedia article
beta_binomial_ab <- function(alpha, beta, n) {
    
    text <- helpText(
        p(
            "A random variable \\(X\\) is said to have a",
            a(href = "http://en.wikipedia.org/wiki/Beta-binomial_distribution", "Beta-Binomial distribution"),
            "if its probability mass function can be represented as 
            $$
            \\text{P}(X = k)
            = f(k; n, \\alpha, \\beta)
            = {n \\choose k} \\frac{ \\text{B}(k + \\alpha, n - k + \\beta) }{\\text{B}(\\alpha, \\beta)}
            $$
            with \\(k = 0, 1, \\ldots, n \\), \\( \\alpha > 0\\), and \\( \\beta > 0\\). 
            
            The mean and variance of \\(X\\) are
            $$
                \\begin{align*}
                \\text{E} [X] &= n \\frac{\\alpha}{\\alpha + \\beta} \\\\
                \\text{Var} (X) &= 
                \\frac{n \\alpha \\beta (\\alpha + \\beta + n)}
                      {(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)}
                \\end{align*}
            $$
            This parametrization can be obtained from that with \\( \\mu \\) and \\( \\sigma \\) by setting
            \\( \\alpha = \\frac{ \\mu }{ \\sigma } \\) and \\( \\beta = \\frac{1 - \\mu }{ \\sigma } \\)."
        ),
        p(
            "In a Bayesian framework, the Beta-Binomial distribution appears as the predictive distribution for
                 a binomial random variable with a Beta prior placed on the success probability."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Beta-binomial_model", 
              "Beta-binomial distribution (Wikipedia)"), br(),
            a(href = "http://cran.r-project.org/web/packages/gamlss.dist/index.html",
              "gamlss.dist (R package)")
        )
    )
    
    mu <- alpha / (alpha + beta)
    sigma <- 1 / (alpha + beta)
    return(beta_binomial_mu_sigma(mu, sigma, n, text, 
                                  c("\\alpha", "\\beta"),
                                  c(alpha, beta)))
}