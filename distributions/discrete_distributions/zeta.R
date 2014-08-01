# http://en.wikipedia.org/wiki/Zeta_distribution
# page 744 of VGAM manual
# page 527 of Univariate Discrete Distributions

# Parametrization used in Wikipedia article above
# s > 1
zeta_dist <- function(s) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Zeta_distribution", 
              "zeta distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; s)
            = \\frac{k^{-s}}{\\zeta(s)}
            $$
            with \\(k = 1, 2, 3, \\ldots \\), \\( s > 1 \\),
            and where \\(\\zeta(\\cdot)\\) is the ",
            a(href = "http://en.wikipedia.org/wiki/Riemann_zeta_function", 
              "Riemann zeta function."),
            "The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{\\zeta(s-1)}{\\zeta(s)}, \\quad \\text{if } s > 2 \\\\ 
                \\text{Var} (X) &= \\frac{\\zeta(s) \\zeta(s-2) - \\zeta(s-1)^2}{\\zeta(s)^2}, \\quad \\text{if } s > 3
                \\end{align*}
            $$
            "
        ),
        p(
            "The zeta distribution is also known as the discrete Pareto distribution,
             and has found use in insurance and linguistics.
             It is also sometimes referred to as the Zipf distribution,
             although in the R package ",
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM"),
            "the Zipf distribution instead refers to a Zeta distribution that
             has been right truncated at a positive integer \\(N\\)."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://www.amazon.com/Univariate-Discrete-Distributions-Norman-Johnson/dp/0471272469", 
              "Univariate Discrete Distributions (3rd edn)"), br(),
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)"), br(),
            a(href = "http://en.wikipedia.org/wiki/Zeta_distribution",
              "Zeta distribution (Wikipedia)")
        )
    )
    
    mean <- ifelse(s > 2,
                   VGAM::zeta(s - 1) / VGAM::zeta(s),
                   Inf)
    variance <- ifelse(s > 3,
                       (VGAM::zeta(s) * VGAM::zeta(s - 2) - (VGAM::zeta(s-1))^2) / (VGAM::zeta(s))^2,
                       Inf)
    
    stats <- stats_text("s", s, mean, variance)
    
    supp <- 1:15
    dens <- VGAM::dzeta(supp, s - 1)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                text = text, stats = stats))
}


# discrete_plot(zeta_dist(0.2))
