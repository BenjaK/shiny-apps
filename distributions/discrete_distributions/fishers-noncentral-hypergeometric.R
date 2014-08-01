# http://en.wikipedia.org/wiki/Fisher's_noncentral_hypergeometric_distribution
# http://en.wikipedia.org/wiki/Noncentral_hypergeometric_distributions


# Urn contains N = n_red + n_white red and white balls
# log_odds is the log oddds of red over white balls
#
fishers_noncentral_hypergeometric <- function(odds, n_red, n_white, n_sampled) {
    
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
                                   \\frac{j \\lambda^j}
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
        )
    )
    
    mean <- BiasedUrn::meanFNCHypergeo(m1 = n_red, 
                                       m2 = n_white, 
                                       n = n_sampled, 
                                       odds = odds, 
                                       precision = 1E-3)
    variance <- BiasedUrn::varFNCHypergeo(m1 = n_red, 
                                          m2 = n_white, 
                                          n = n_sampled, 
                                          odds = odds, 
                                          precision = 1E-3)
    
    supp <- seq(max(0, n_sampled - n_white), min(n_sampled, n_red))
    dens <- BiasedUrn::dFNCHypergeo(x = supp,
                                    m1 = n_red,
                                    m2 = n_white,
                                    n = n_sampled,
                                    odds = odds)
    return(list(df = data.frame(supp = supp,
                                dens = dens)))
}

# discrete_plot(fishers_noncentral_hypergeometric(0.5, 10, 10, 4))
