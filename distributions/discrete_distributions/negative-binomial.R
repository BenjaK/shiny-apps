# http://en.wikipedia.org/wiki/Negative_binomial_distribution


# See also http://cran.r-project.org/web/packages/VGAM/VGAM.pdf
# page 473-475

# Using the definition of the NBinom distribution found 
# at the Wikipedia article above
# p = probability of success
# r = number of failures after which we stop
# k = number of successes (seen in plot)
negative_binomial_pr <- function(p, r) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Negative_binomial_distribution", "negative binomial distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; p, r)
            = {k + r - 1 \\choose k} (1 - p)^r p^k
            $$
            with \\(k = 0, 1, 2, \\ldots \\), \\(r > 0 \\), and \\(0 < p < 1 \\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{p r}{1 - p} \\\\ 
                \\text{Var} (X) &= \\frac{pr}{(1 - p)^2}
                \\end{align*}
            $$
            "
        ),
        p(
            "The negative binomial pmf above gives the probability of seeing \\(k\\) successes 
             before reaching exactly \\(r\\) failures in a sequence of ",
            a(href = "http://en.wikipedia.org/wiki/Bernoulli_trial",
              "Bernoulli trials,"),
            "each with success probability \\(p\\)."
            
        ),
        
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Negative_binomial_distribution", 
              "Negative binomial distribution (Wikipedia)")
        )
    )
    
    mean <- p * r / (1 - p)
    variance <- p * r / (1 - p)^2
    
    stats <- stats_text(c("p", "r"), c(p, r), mean, variance)
    
    supp <- 0:20  # number of successes until r failures
    dens <- dnbinom(supp, r, 1-p)
    return(list(df = data.frame(supp = supp, 
                                dens = dens),
                text = text, stats = stats))
}
