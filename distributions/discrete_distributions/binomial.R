# http://en.wikipedia.org/wiki/Binomial_distribution

binomial_dist <- function(p, n) {
    
    
    text <- helpText(
        p(
            "A random variable \\(X\\) is said to have a", 
            a(href = "http://en.wikipedia.org/wiki/Binomial_distribution", "Binomial distribution"),
            "if its probability mass function can be represented as 
            $$
            \\text{P}(X = k)
            = f(k; n, p)
            = {n \\choose k} p^n (1 - p)^{n - k}
            $$
            with \\(k = 0, 1, \\ldots, n \\), and \\(0 < p < 1 \\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= n p \\\\ 
                \\text{Var} (X) &= n p (1 - p)
                \\end{align*}
            $$
            "
        ),
        p(
            "The binomial distribution can be thought of as the distribution of the number
             of heads one gets in \\( n \\) coin flips, when the (known) probability of 
             obtaining a head in any one flip is \\( p \\)."
        )
    )
    
    mean <- n * p
    variance <- n * p * (1 - p)
    
    stats <- stats_text(c("p", "n"), c(p, n), mean, variance)
    
    
    
    supp <- 0:n
    dens <- dbinom(x = supp, 
                   size = n,
                   prob = p)
#     ymax <- dbinom(floor(0.01 * (n + 1)), n, 0.01) + 0.01
    return(list(df = data.frame(supp = supp, dens = dens),
                ymax = 1,
                text = text, stats = stats))
}
