# http://en.wikipedia.org/wiki/Borel_distribution

# Error in VGAM: density should contain
# Qsize instea of factorial(Qsize) in numerator
borel_tanner <- function(mu, n, 
                         text = NULL, 
                         param_names = NULL, 
                         param_values = NULL) {
    
    if (is.null(text)) {
        text <- helpText(
            p(
                "The random variable \\(X\\) has a", 
                a(href = "http://en.wikipedia.org/wiki/Borel_distribution#Borel.E2.80.93Tanner_distribution", 
                  "Borel-Tanner distribution"),
                "if its probability mass function can be written as 
                $$
                \\text{P}(X = k)
                = f(k; n, \\mu)
                = \\frac{n}{k} \\cdot \\frac{(\\mu k)^{k - n}}{(k - n)!}\\exp(-\\mu k)
                $$
                with \\(k = n, n+1, n+2, \\ldots \\), and \\(0 < \\mu < 1 \\).
                The mean and variance of \\(X\\) are given by 
                $$  \\begin{align*} 
                    \\text{E}[X] &= n \\frac{1}{1 - \\mu} \\\\ 
                    \\text{Var} (X) &= n \\frac{\\mu}{(1 - \\mu)^3}
                    \\end{align*}
                $$
            "
            ),
            p(
                "The Borel-Tanner distribution with parameters \\(\\mu\\) and \\(n\\)
                 is the distribution of the sum of \\(n\\) independent Borel-distributed
                 random variables, each with parameter \\(\\mu\\).
                "
            ),
            h4("Bibliography"),
            p(
                a(href = "http://en.wikipedia.org/wiki/Borel_distribution#Borel.E2.80.93Tanner_distribution", 
                  "Borel-Tanner distribution (Wikipedia)"), br(),
                a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
                  "VGAM (R package)")
            )
        )
    }
    
    if (is.null(param_names)) {
        param_names <- c("\\mu", "n")
        param_values <- c(mu, n)
    }
    
    mean <- n * mu
    variance <- n * mu / (1 - mu)^3
    
    stats <- stats_text(param_names, param_values, mean, variance)
    
    supp <- n:30
    dens <- VGAM::dbort(supp, n, mu) / factorial(n - 1) # found error in dbort implementation; have notified author
    return(list(df = data.frame(supp = supp, dens = dens),
                text = text, stats = stats))
}

borel <- function(mu) {
    
    text <- helpText(
        p(
            "The random variable \\(X\\) has a", 
            a(href = "http://en.wikipedia.org/wiki/Borel_distribution", "Borel distribution"),
            "if its probability mass function can be written as 
            $$
            \\text{P}(X = k)
            = f(k; \\mu)
            = \\frac{(\\mu k)^{k - 1}}{k!} \\exp(-\\mu k)
            $$
            with \\(k = 1, 2, \\ldots \\), and \\(0 < \\mu < 1 \\).
            The mean and variance of \\(X\\) are given by 
            $$  \\begin{align*} 
                \\text{E}[X] &= \\frac{1}{1 - \\mu} \\\\ 
                \\text{Var} (X) &= \\frac{\\mu}{(1 - \\mu)^3}
                \\end{align*}
            $$
            "
        ),
        p(
            "The Borel distribution finds use in",
            a(href = "http://en.wikipedia.org/wiki/Branching_processes", "branching processes"),
            "as the distribution of the total number of descendants of a single individual,
             if the individual and its descendants all have offspring according to a
             Poisson distribution with mean \\(\\mu\\), \\(0 < \\mu < 1\\).",
            "This distribution also appears in",
            a(href = "http://en.wikipedia.org/wiki/Queueing_theory", "queuing theory"),
            "as the number of customers served in the busy period of an",
            a(href = "http://en.wikipedia.org/wiki/M/D/1_queue", "M/D/1-queue.")
        ),
        h4("Bibliography"),
        p(
            a(href = "http://en.wikipedia.org/wiki/Borel_distribution", 
              "Borel distribution (Wikipedia)"), br(),
            a(href = "http://cran.r-project.org/web/packages/VGAM/index.html", 
              "VGAM (R package)")
        )
    )
    
    param_names <- "\\mu"
    param_values <- mu
    
    
    return(borel_tanner(mu, 1, text, param_names, param_values))
}


# discrete_plot(borel_tanner(5, 0.3))
# discrete_plot(borel(0.7))
