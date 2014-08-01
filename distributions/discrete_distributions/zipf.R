# http://en.wikipedia.org/wiki/Zipf%27s_law
# http://en.wikipedia.org/wiki/Zipf's_law#Theoretical_review
# 

zipf_dist <- function(s, N) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://books.google.se/books?id=OkDy0RJOwiAC&pg=PA127&lpg=PA127&dq=truncated+zeta+distribution&source=bl&ots=NXcacLhC0f&sig=nmJk-NU_UPS0p6KVQaLNnIhDwZw&hl=en&sa=X&ei=rmXVU62RC6uAywPtrIHIBQ&ved=0CEgQ6AEwBQ#v=onepage&q=truncated%20zeta%20distribution&f=false", 
              "Zipf distribution, "),
            "or (right) truncated Zeta distribution, 
             if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; s, N)
            = \\frac{k^{-s}}{\\sum_{n=1}^N n^{-s}} 
            $$
            with \\(k = 1, 2, \\ldots, N \\), and \\( s > 0 \\).
            Letting \\( H_{n,m} = \\sum_{k=1}^n k^{-m} \\) be the ",
            a(href = "http://en.wikipedia.org/wiki/Harmonic_number#Generalized_harmonic_numbers",
              "generalized harmonic number "),
            "of order n of m,
            we have that the mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{H_{N, s - 1}}{H_{N, s}} \\\\ 
                \\text{Var} (X) &= \\frac{H_{N, s - 2}}{H_{N, s}} - \\left( \\frac{H_{N, s - 1}}{H_{N, s}} \\right)^2
                \\end{align*}
            $$
            "
        ),
        p(
            "The distribution is motivated by ",
            a(href = "http://en.wikipedia.org/wiki/Zipf%27s_law", "Zipf's law:"),
            "in a population of N elements, such as the words in the English language,
             the relative frequency of the elements of rank \\(k\\) in a text corpus
             is predicted to be \\(f(k; s, N)\\), where the parameter \\(s\\) is 
             an exponent characterizing the distribution (and equal to one in the 
             classical version of the law, according to the Wikipedia article)."
        ),
        h4("Bibliography"),
        p(
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)"), br(),
            a(href = "http://www.degruyter.com/view/product/41158",
              "Word Frequency Studies by Ioan-Iovitz Popescu"), br(),   
            a(href = "http://en.wikipedia.org/wiki/Zeta_distribution",
              "Zeta distribution (Wikipedia)"), br(),
            a(href = "http://en.wikipedia.org/wiki/Zipf%27s_law",
              "Zipf's law (Wikipedia)")
        )
    )
    
    
    mean <- VGAM:::gharmonic(N, s - 1) / VGAM:::gharmonic(N, s)
    variance <- VGAM:::gharmonic(N, s - 2) / VGAM:::gharmonic(N, s) - mean^2
    
    stats <- stats_text(c("s", "N"), c(s, N), mean, variance)
    
    supp <- 1:N
    dens <- VGAM::dzipf(supp, N, s)
    return(list(df = data.frame(supp = supp,
                                dens = dens),
                ymax = 1,
                text = text,
                stats = stats))
}
