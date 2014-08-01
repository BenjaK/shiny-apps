# http://en.wikipedia.org/wiki/Skellam_distribution

skellam_dist <- function(mu1, mu2) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Skellam_distribution", "Skellam distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\mu_1, \\mu_2)
            = e^{  -(\\mu_1 + \\mu_2)  }
              \\left( \\frac{\\mu_1}{\\mu_2} \\right)^{k / 2}
              I_{k} \\left( 2 \\sqrt{\\mu_1 \\mu_2} \\right)
            $$
            with \\(k = 0, 1, 2, \\ldots \\), \\( \\mu_1 > 0\\), \\( \\mu_2 > 0\\),
            and \\( I_k(x) \\) is the",
            a(href = "http://en.wikipedia.org/wiki/Bessel_function#Modified_Bessel_functions_:_I.CE.B1.2C_K.CE.B1", 
              "modified Bessel function"),
            "of the first kind.",
            "The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\mu_1 - \\mu_2 \\\\ 
                \\text{Var} (X) &= \\mu_1 + \\mu_2
                \\end{align*}
            $$
            "
        ),
        p(
            "The Skellam distribution is the distribution of the difference of two
             independent Poisson random variables, the first with mean \\( \\mu_1 \\)
             and the other with mean \\( \\mu_2 \\)."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Skellam_distribution",
              "Skellam distribution (Wikipedia)"), br(),
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)")
        )
    )
    
    mean <- mu1 - mu2
    variance <- mu1 + mu2
    
    stats <- stats_text(c("\\mu_1", "\\mu_2"), c(mu1, mu2), mean, variance)
    
    supp <- -10:10
    dens <- VGAM::dskellam(supp, mu1, mu2)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                text = text, stats = stats))
}