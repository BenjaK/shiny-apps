# http://en.wikipedia.org/wiki/Logarithmic_distribution

# Consider using VGAM:dlog instead
logarithmic <- function(p) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Logarithmic_distribution", "logarithmic distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; p)
            = \\frac{-1}{\\log(1 - p)} \\! \\frac{p^k}{k}
            $$
            with \\(k = 1, 2, 3, \\ldots \\), and \\(0 < p < 1 \\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{-1}{\\log(1 - p)} \\frac{p}{1 - p} \\\\ 
                \\text{Var} (X) &= - p \\frac{p + \\log(1 - p)}{(1 - p)^2 \\log^2 \\! (1 - p)}
                \\end{align*}
            $$
            "
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Logarithmic_distribution", 
              "Logarithmic distribution (Wikipedia)")
        )
    )
    
    mean <- -p / ((1 - p) * log(1 - p))
    variance <- - p * (p + log(1 - p)) / ((1 - p) * log(1 - p))^2
    
    stats <- stats_text("p", p, mean, variance)
    
    supp <- 1:20
    dens <- -p^supp / (supp * log(1 - p))
    return(list(df = data.frame(supp = supp, dens = dens),
                ymax = 1,
                text = text, stats = stats))
}

# discrete_plot(logarithmic(0.99)$df)
