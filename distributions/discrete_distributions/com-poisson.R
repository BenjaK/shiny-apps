# http://en.wikipedia.org/wiki/Conway%E2%80%93Maxwell%E2%80%93Poisson_distribution

com_poisson <- function(lambda, nu) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Conway%E2%80%93Maxwell%E2%80%93Poisson_distribution", 
              "Conway–Maxwell–Poisson distribution"),
            "(or COM-Poisson distribution) if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\lambda, \\nu)
            = \\frac{\\lambda^k}{(k!)^\\nu} \\! \\frac{1}{Z(\\lambda, \\nu)}
            $$
            with \\(k = 0, 1, 2, \\ldots \\), \\(\\lambda > 0, \\nu \\geq 0 \\), and
            $$
                Z(\\lambda, \\nu) = \\sum_{j = 0}^{\\infty} \\frac{\\lambda^j}{(j!)^\\nu}
            $$

            No closed-form expressions exist for the mean and variance of \\(X\\);
            they may instead be written in terms of their respective definitions as
            $$  \\begin{align*} 
                \\text{E}[X] &=  \\sum_{j = 0}^{\\infty} 
                                 \\frac{j \\lambda^j}
                                       {(j!)^\\nu Z(\\lambda, \\nu)} \\\\ 
                \\text{Var} (X) &= \\sum_{j = 0}^{\\infty} 
                                   \\frac{j^2 \\lambda^j}
                                         {(j!)^\\nu Z(\\lambda, \\nu)}
                                   - (\\text{E}[X])^2
                \\end{align*}
            $$
            "
        ),
        p(
            "The COM-Poisson distribution is a generalization of the Poisson distribution,
             with the parameter \\(\\nu\\) providing a means to model under- and overdispersion.
             When \\(\\nu = 1\\), the COM-Poisson distribution reduces to the standard Poisson
             distribution."
        ),
        h4("Bibliography"),
        p(  a(href = "http://cran.r-project.org/web/packages/compoisson/index.html",
              "compoisson (R package)"), br(),
            a(href = "http://en.wikipedia.org/wiki/Conway%E2%80%93Maxwell%E2%80%93Poisson_distribution", 
              "Conway–Maxwell–Poisson distribution (Wikipedia)")
        )
    )
    
    mean <- compoisson::com.mean(lambda, nu)
    variance <- compoisson::com.var(lambda, nu)
    
    stats <- stats_text(c("\\lambda", "\\nu"), c(lambda, nu), mean, variance)
    
    supp <- 0:25
    dens <- compoisson::dcom(supp, lambda, nu)
    return(list(df = data.frame(supp = supp, dens = dens),
                text = text, stats = stats))
}

# discrete_plot(com_poisson(15, 0.5))
