# VGAM manual page 531
# http://books.google.se/books?id=B8kNa1khS4QC&pg=PA195&lpg=PA195&dq=poisson+lognormal+distribution+moments&source=bl&ots=Bjr0HZn8Rd&sig=kejAbaivzkcAB1FSyoB908sHumc&hl=en&sa=X&ei=sGzTU9PKDaqAywOXgIKQDw&ved=0CCEQ6AEwAA#v=onepage&q=poisson%20lognormal%20distribution%20moments&f=false


poisson_lognormal <- function(mu, sigma) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://books.google.se/books?id=B8kNa1khS4QC&pg=PA195&lpg=PA195&dq=poisson+lognormal+distribution+moments&source=bl&ots=Bjr0HZn8Rd&sig=kejAbaivzkcAB1FSyoB908sHumc&hl=en&sa=X&ei=sGzTU9PKDaqAywOXgIKQDw&ved=0CCEQ6AEwAA#v=onepage&q=poisson%20lognormal%20distribution%20moments&f=false", 
              "Poisson-Lognormal distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\mu, \\sigma)
            = \\frac{1}{ \\sigma \\sqrt{2 \\pi} } \\cdot \\frac{1}{k!}
              \\int_{0}^{\\infty} \\! \\!
              e^{-y} y^{k-1} \\exp \\left( -\\frac{\\left( \\log(y) - \\mu \\right)^2}{2 \\sigma^2} \\right) 
              \\text{d}y
            $$
            with \\(k = 0, 1, 2, \\ldots \\), \\( \\sigma > 0 \\), and \\( \\mu \\in \\mathbb{R} \\).
            The integral above cannot be expressed in closed-form.
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\exp( \\mu + \\sigma^2 / 2 ) \\\\ 
                \\text{Var} (X) &= \\exp( \\mu + \\sigma^2 / 2 )
                                   \\left[ \\exp( \\mu + \\sigma^2 / 2 ) ( e^{\\sigma^2} - 1) + 1 \\right]
                \\end{align*}
            $$
            "
        ),
        p(
            "If \\(X | Y = y \\) has a \\(\\text{Poisson}(y) \\) distribution,
             and \\( \\log Y \\) has a normal distribution with parameters \\(\\mu\\) and \\(\\sigma\\)
             (so that \\(Y\\) is log-normally distributed),
             the unconditional distribution of \\(X\\) will be Poisson-Lognormal.",
            "The Poisson-Lognormal distribution has found use in biology, in the context of species abundance."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://www.amazon.com/Lognormal-Distributions-Applications-Statistics-Monographs/dp/0824778030/ref=sr_1_1?s=books&ie=UTF8&qid=1406562590&sr=1-1&keywords=9780824778033&dpPl=1", 
              "Lognormal Distributions: Theory and Applications"), br(),
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)")
        )
    )
    
    mean <- exp(mu + sigma^2 / 2)
    variance <- mean * (mean * (exp(sigma^2) - 1) + 1)
    
    stats <- stats_text(c("\\mu", "\\sigma"), c(mu, sigma), mean, variance)
    
    supp <- 0:20
    dens <- VGAM::dpolono(supp, mu, sigma)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                text = text,
                stats = stats))
}

# discrete_plot(poisson_lognormal(0, 1))
