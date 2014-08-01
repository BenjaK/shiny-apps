# http://en.wikipedia.org/wiki/Yule%E2%80%93Simon_distribution

yule_simon <- function(rho) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Yule%E2%80%93Simon_distribution", 
              "Yule-Simon distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\rho)
            = \\rho \\text{B}(k, \\rho + 1)
            $$
            with \\(k = 1, 2, 3, \\ldots \\), \\( \\rho > 0 \\),
            and \\( \\text{B}(\\cdot ~, \\cdot) \\) is the ",
            a(href = "http://en.wikipedia.org/wiki/Beta_function", 
              "Beta function."),
            "The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{\\rho}{\\rho - 1}, \\quad \\text{if } \\rho > 1 \\\\ 
                \\text{Var} (X) &= \\frac{\\rho^2}{(\\rho - 1)^2 (\\rho - 2)}, \\quad \\text{if } \\rho > 2
                \\end{align*}
            $$
            "
        ),
        p(
            "The Yule-Simon distribution can be seen as a compound distribution in the following way:
             let \\(W\\) be",
            a(href = "http://en.wikipedia.org/wiki/Exponential_distribution", 
              "exponentially distributed"),
             "with mean \\(1 / \\rho\\)
             and let \\( X | W = w \\) have a shifted geometric distribution
             (i.e. supported on the positive integers) with parameter \\(e^{-w}\\). Then
            $$ \\begin{align*}
               \\text{P}(X = k)
               &= \\text{E} [\\text{P}(X = k | W)] \\\\
               &= \\int_0^\\infty e^{-w} (1 - e^{-w})^{k-1} \\rho e^{-\\rho w} \\, \\text{d}w \\\\
               &= \\rho \\int_0^\\infty (1 - e^{-w}) \\left(e^{-w} \\right)^{\\rho + 1} \\, \\text{d}w \\\\
               & \\left\\lceil t = 1 - e^{-w}, \\text{d}w = \\frac{\\text{d}t}{1-t} \\right\\rfloor \\\\
               &= \\rho \\int_0^1 t^{k-1} (1-t)^{(\\rho + 1) - 1} \\, \\text{d}t \\\\
               &= \\rho \\text{B}(k, \\rho + 1)
               \\end{align*}
            $$"
        ),
        h4("Bibliography"),
        p(
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)"), br(),
            a(href = "http://en.wikipedia.org/wiki/Yule%E2%80%93Simon_distribution",
              "Yule-Simon distribution (Wikipedia)")
        )
    )
    
    mean <- ifelse(rho > 1, 
                   rho / (rho - 1),
                   Inf)
    variance <- ifelse(rho > 2,
                       rho^2 / ((rho - 1)^2 * (rho - 2)),
                       Inf)
    
    stats <- stats_text("\\rho", rho, mean, variance)
    
    supp <- 1:15
    dens <- VGAM::dyules(supp, rho)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                ymax = 0.75,
                text = text, stats = stats))
}