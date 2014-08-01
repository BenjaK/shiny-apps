# http://en.wikipedia.org/wiki/Hypergeometric_distribution


# Probability of getting k successes
# in n draws without replacement 
# from urn containing N possibilities,
# K of which are successes
hypergeometric <- function(draws, total, successes) {
    supp <- seq(max(0, draws + successes - total), 
                min(successes, draws))
    print(supp)
    dens <- dhyper(x = supp,
                   m = successes,
                   n = total - successes,
                   k = draws)
    return(list(df = data.frame(supp = supp, dens = dens)))
}