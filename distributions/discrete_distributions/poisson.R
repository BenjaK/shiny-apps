
# Mean parametrization of Poisson distribution
poisson_dist <- function(lambda) {
    
#     text <- remove_newlines("
    text <- helpText(
        p(
            "A random variable \\(X\\) is said to have a",
            a(href = "en.wikipedia.org/wiki/Poisson_distribution", "Poisson distribution"),
            "if its probability mass function can be represented as 
            $$
            \\text{P}(X = k)
            = f(k; \\lambda)
            = \\frac{\\lambda^k}{k!} e^{-\\lambda},
            \\qquad k = 0, 1, 2, \\ldots
            $$
            with \\(\\lambda > 0\\). The mean of \\(X\\) is \\( \\text{E}[X] = \\lambda \\) 
            and the variance is \\( \\text{Var} (X) = \\lambda \\)."
        ),
        p(
            "The Poisson distribution provides a good model for the number of events
            that occur in a fixed interval of time (or some other dimension),
            when these events happen with the known average rate \\( \\lambda \\)
            and happen independently of the time since the previous event.
            Common examples of such situations include the number of radioactive 
            particles that decay in an interval of time, 
            the number of spelling errors per page in a book,
            and the number of customers arriving at a queue."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Poisson_distribution", 
              "Poisson distribution (Wikipedia)")
        )
    )
    
    
    mean <- lambda
    variance <- lambda
    
    stats <- stats_text("\\lambda", lambda, mean, variance)
    
    supp <- 0:30
    dens <- dpois(supp, lambda)
    return(list(df = data.frame(supp = supp, 
                                dens = dens), 
                ymax = 0.607,
                text = text,
                stats = stats))
}
