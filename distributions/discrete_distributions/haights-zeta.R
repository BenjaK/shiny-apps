# http://artax.karlin.mff.cuni.cz/r-help/library/VGAM/html/hzeta.html

haights_zeta <- function(alpha) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://books.google.se/books?id=JchiadWLnykC&pg=PA533&lpg=PA533&dq=Haight%27s+Zeta+distribution&source=bl&ots=pXyH5gAtsv&sig=lsSsFKxfn92zp4SeFGr9KIrjWOc&hl=en&sa=X&ei=-JfWU-rWNaf-ygPBu4GQAg&ved=0CCEQ6AEwAA#v=onepage&q=Haight's%20Zeta%20distribution&f=false", 
              "Haight's zeta distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\alpha)
            = (2k - 1)^{-\\alpha} - (2k + 1)^{-\\alpha}
            $$
            with \\(k = 1, 2, \\ldots \\), and \\(\\alpha > 0 \\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= (1 - 2^{-\\alpha}) \\zeta(\\alpha), \\quad \\alpha > 1 \\\\ 
                \\text{Var} (X) &= (1 - 2^{1 - \\alpha}) \\zeta(\\alpha - 1), \\quad \\alpha > 2
                \\end{align*}
            $$
            where \\( \\zeta( \\cdot ) \\) is the ",
            a(href = "http://en.wikipedia.org/wiki/Riemann_zeta_function", 
              "Riemann zeta function.")
        ),
        h4("Bibliography"),
        p(  a(href = "http://www.amazon.com/Univariate-Discrete-Distributions-Norman-Johnson/dp/0471272469/ref=sr_1_1?s=books&ie=UTF8&qid=1406572659&sr=1-1&keywords=univariate+discrete+distributions", 
              "Univariate Discrete Distributions"), br(),
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)")
        )
    )
    
    mean <- ifelse(alpha > 1,
                   (1 - 2^(-alpha)) * VGAM::zeta(alpha),
                   Inf)
    variance <- ifelse(alpha > 2,
                       (1 - 2^(1-alpha)) * VGAM::zeta(alpha - 1) - mean^2,
                       Inf)
    
    stats <- stats_text("\\alpha", alpha, mean, variance)
    
    supp <- 1:10
    dens <- VGAM::dhzeta(supp, alpha)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
#                 ymax = 0.75,
                text = text, stats = stats))
}