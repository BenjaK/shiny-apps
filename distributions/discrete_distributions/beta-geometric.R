# http://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/bgepdf.htm
# http://www.vosesoftware.com/ModelRiskHelp/index.htm#Distributions/Discrete_distributions/BetaGeometric_distribution.htm
# http://arxiv.org/pdf/1405.6392.pdf
# https://www.maths.otago.ac.nz/home/resources/stat261/2014_General/Smoking_and_pregnancy.pdf?m=1394480762

beta_geometric_mu_sigma <- function(mu, sigma) {
    
    text <- helpText(
        p(
            "A random variable \\(X\\) is said to have a Beta-Geometric distribution 
                if its probability mass function can be represented as 
                $$
                \\text{P}(X = k)
                = f(k; \\mu, \\sigma)
                =  
                \\frac{ \\text{B} \\left(\\frac{\\mu}{\\sigma} + 1, k + \\frac{1 - \\mu}{\\sigma} \\right) }
                      { \\text{B} \\left( \\frac{\\mu}{\\sigma}, \\frac{1 - \\mu}{\\sigma} \\right)}
                $$
                with \\(k = 0, 1, 2, \\ldots \\), \\(0 < \\mu < 1 \\) and \\( \\sigma > 0\\).
                \\( \\text{B}(\\cdot ~, \\cdot) \\) is here the",
            a(href = "http://en.wikipedia.org/wiki/Beta_function", "Beta function."),
            "The mean and variance of \\(X\\) are given by 
                $$  \\begin{align*} 
                    \\text{E}[X] &= \\frac{1 - \\mu}{\\mu - \\sigma}, 
                                    \\quad \\text{if } \\mu > \\sigma \\\\ 
                    \\text{Var} (X) &= \\frac{\\mu (1 - \\mu) (1 - \\sigma)}
                                             {(\\mu - \\sigma)^2 (\\mu - 2 \\sigma)},
                                       \\quad \\text{if } \\mu > 2 \\sigma
                    \\end{align*}
                $$

                This parametrization can be obtained from that with \\( \\alpha \\) and \\( \\beta \\) by setting
                \\( \\mu = \\frac{ \\alpha }{ \\alpha + \\beta } \\) and \\( \\sigma = \\frac{1}{ \\alpha + \\beta } \\)."
        ),
        p(
            "In a Bayesian framework, the Beta-Geometric distribution appears as the posterior 
                 predictive distribution for a geometric random variable with a Beta prior 
                 placed on the success probability."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/bgepdf.htm", 
              "Beta-geometric distribution (NIST)"), br(),
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)"), br(),
            a(href = "http://www.vosesoftware.com/ModelRiskHelp/index.htm#Distributions/Discrete_distributions/BetaGeometric_distribution.htm",
              "BetaGeometric equations (VOSE Software)")
        )
    )
    
    
    a <- mu / sigma
    b <- (1 - mu) / sigma
    param_names <- c("\\mu", "\\sigma")
    return(beta_geometric_ab(a, b, text, param_names, c(mu, sigma)))
}

# 
beta_geometric_ab <- function(alpha, beta,
                              text = NULL, param_names = NULL, param_values = NULL) {
    
    if (is.null(text)) {
        text <- helpText(
            p(
                "A random variable \\(X\\) is said to have a Beta-Geometric distribution 
                if its probability mass function can be represented as 
                $$
                \\text{P}(X = k)
                = f(k; \\alpha, \\beta)
                =  
                \\frac{ \\text{B} \\left(\\alpha + 1, k + \\beta \\right) }
                      { \\text{B} \\left( \\alpha, \\beta \\right)}
                $$
                with \\(k = 0, 1, 2, \\ldots \\), \\( \\alpha > 0 \\) and \\( \\beta > 0\\).
                \\( \\text{B}(\\cdot ~, \\cdot) \\) is here the",
                a(href = "http://en.wikipedia.org/wiki/Beta_function", "Beta function."),
                "The mean and variance of \\(X\\) are given by 
                $$  \\begin{align*} 
                    \\text{E}[X] &= \\frac{\\beta}{\\alpha - 1}, 
                                    \\quad \\text{if } \\alpha > 1 \\\\ 
                    \\text{Var} (X) &= \\frac{\\alpha \\beta (\\alpha + \\beta - 1)}
                                             {(\\alpha - 1)^2 (\\alpha - 2)},
                                       \\quad \\text{if } \\alpha > 2
                    \\end{align*}
                $$
                This parametrization can be obtained from that with \\( \\mu \\) and \\( \\sigma \\) by setting
                \\( \\alpha = \\frac{ \\mu }{ \\sigma } \\) and \\( \\beta = \\frac{1 - \\mu }{ \\sigma } \\)."
            ),
            p(
                "In a Bayesian framework, the Beta-Binomial distribution appears as the posterior 
                 predictive distribution for a binomial random variable with a Beta prior 
                 placed on the success probability."
            ),
            h4("Bibliography"),
            p(
                a(href = "http://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/bgepdf.htm", 
                  "Beta-geometric distribution (NIST)"), br(),
                a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
                  "VGAM (R package)"), br(),
                a(href = "http://www.vosesoftware.com/ModelRiskHelp/index.htm#Distributions/Discrete_distributions/BetaGeometric_distribution.htm",
                  "BetaGeometric equations (VOSE Software)")
            )
        )
    }
    
    if (is.null(param_names)) {
        param_names <- c("\\alpha", "\\beta")
    }
    
    if (is.null(param_values)) {
        param_values <- c(alpha, beta)
    }
    
    mean <- ifelse(alpha > 1, beta / (alpha - 1), Inf)
    variance <- ifelse(alpha > 2 , 
                       alpha * beta * (alpha + beta - 1) / ((alpha - 1)^2 * (alpha - 2)),
                       Inf)
    
    stats <- stats_text(param_names, param_values, mean, variance)
    
    
    
    supp <- 0:15
    dens <- VGAM::dbetageom(supp, alpha, beta)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                ymax = 1,
                text = text,
                stats = stats))
}
