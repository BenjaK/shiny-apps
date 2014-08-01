
# Urn contains N = n_red + n_white red and white balls
# log_odds is the log oddds of red over white balls
#
wallenius_noncentral_hypergeometric <- function(odds, n_red, n_white, n_sampled) {
    supp <- seq(max(0, n_sampled - n_white), min(n_sampled, n_red))
    dens <- BiasedUrn::dWNCHypergeo(supp,
                                    n_red,
                                    n_white,
                                    n_sampled,
                                    odds)
    return(list(df = data.frame(supp = supp,
                                dens = dens)))
}

# discrete_plot(wallenius_noncentral_hypergeometric(-1, 10, 10, 4))
