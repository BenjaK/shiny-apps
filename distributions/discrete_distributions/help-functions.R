
remove_newlines <- function(string) {
    gsub(pattern = "\\n", x = string, replacement = "")
}

paragraphs <- function(strings) {
    cat(strings, sep = '\\n')
}

paster <- function(s, v) {
    res <- ""
    
}

stats_text <- function(param_names, param_values, mean, variance) {
    dec <- 2  # number of decimals to round to
    
    if (mean == Inf) {
        mean <- "\\infty"
    } else {
        mean <- round(mean, dec)
    }
    
    if (variance == Inf) {
        variance <- "\\infty"
        sdev <- variance
    } else {
        sdev <- round(sqrt(variance), dec)
        variance <- round(variance, dec)
    }
    
    a <- "$$ \\begin{align*}"
    b <- paste(param_names, " &= ", round(param_values, dec), " \\\\", sep = "")
    c <- paste("\\text{E}[X] &= ", mean, "\\\\")
    d <- paste("\\text{SD}(X) &= ", sdev, "\\\\")
    e <- paste("\\text{Var} (X) &= ", variance)
    f <- "\\end{align*} $$"
    s <- do.call(helpText, list(a,b,c,d,e,f))
    return(s)
}

# strs <- c(
#     "atat
#      qfefqef",
#     "qefqef
#      afafqe")