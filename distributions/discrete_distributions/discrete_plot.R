
# df should be a data frame containing variables
# supp and dens
discrete_plot <- function(lst, ymax = FALSE) {
    if (!("df" %in% names(lst) && length(intersect(c("supp", "dens"), 
                                                   names(lst$df))) > 1)) {
        stop("'lst' must be a list containing at least a data.frame 'df' with variables 'supp' and 'dens'")
    }
    p <- ggplot(data = lst$df, 
                aes(x = as.factor(supp), 
                    y = dens)) +
        geom_histogram(binwidth=1, 
                       stat="identity", 
                       colour = "black", 
                       fill = "deepskyblue4",
                       alpha = 0.6) +
        xlab(expression(italic(k))) + ylab(expression(paste("P(", italic("X = k"), ")"))) +
        theme_bw() +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14))
    
    if ("ymax" %in% names(lst)) {
     p <- p + ylim(0, lst$ymax)
    }
    
    return(p)
}