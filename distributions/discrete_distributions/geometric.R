# http://en.wikipedia.org/wiki/Geometric_distribution

geometric_p <- function(p) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Geometric_distribution", "geometric distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; p)
            = (1 - p)^k p
            $$
            with \\(k = 0, 1, 2, \\ldots \\), and \\(0 < p \\leq 1 \\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{1 - p}{p} \\\\ 
                \\text{Var} (X) &= \\frac{1 - p}{p^2}
                \\end{align*}
            $$
            "
        ),
        p(
            "The above representation gives the distribution
             of the number of failures until the first success in consecutive ",
            a(href = "http://en.wikipedia.org/wiki/Bernoulli_trial", "Bernoulli trials."),
            "The geometric distribution may also refer to the distribution of
             the number of trials up to and including the first success, in which case
             the support of the distribution is on the positive integers (and so excludes zero)."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Geometric_distribution", 
              "Geometric distribution (Wikipedia)")
        )
    )
    
    mean <- (1 - p) / p
    variance <- (1 - p) / p^2
    
    stats <- stats_text("p", p, mean, variance)
    
    supp <- 0:15
    dens <- dgeom(supp, p)
    return(list(df = data.frame(supp = supp, 
                                dens = dens),
                ymax = 1, 
                text = text, stats = stats))
}

# discrete_plot(geometric_p(0.05))
