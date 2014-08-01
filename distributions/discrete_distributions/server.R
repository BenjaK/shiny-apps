library(ggplot2)
library(gamlss.dist)
library(VGAM)
library(compoisson)
library(BiasedUrn)

source("help-functions.R")
source("discrete_plot.R")
source("beta-binomial.R")
source("beta-geometric.R")
source("binomial.R")
source("borel.R")
source("com-poisson.R")
source("delaporte.R")
source("fishers-noncentral-hypergeometric.R")
source("geometric.R")
source("haights-zeta.R")
source("hypergeometric.R")
source("logarithmic.R")
source("negative-binomial.R")
source("poisson.R")
source("poisson-lognormal.R")
source("sichel.R")
source("skellam.R")
source("wallenius-noncentral-hypergeometric.R")
source("yule-simon.R")
source("zeta.R")
source("zipf.R")




shinyServer(function(input, output) {
    
################   TITLE   #####################################################
    output$title <- renderText({
        
        switch(input$distribution,
               "Beta-Binomial" = "The Beta-Binomial Distribution",
               "Beta-Geometric" = "The Beta-Geometric Distribution",
               "Binomial" = "The Binomial Distribution",
               "Borel" = "The Borel Distribution",
               "Borel-Tanner" = "The Borel-Tanner Distribution",
               "COM-Poisson" = "The Conway-Maxwell-Poisson Distribution",
               "Delaporte" = "The Delaporte Distribution",
#                "FNCHypergeometric" = "Fisher's Noncentral Hypergeometric Distribution",
#                "Generalized Poisson",
               "Geometric" = "The Geometric Distribution",
               "Haight's Zeta" = "Haight's Zeta Distribution",
#                "Hypergeometric" = "The Hypergeometric Distribution",
#                "Inverse Binomial",
               "Logarithmic" = "The Logarithmic Distribution",
               "Negative-Binomial" = "The Negative Binomial Distribution",
               "Poisson" = "The Poisson Distribution",
               "Poisson-Lognormal" = "The Poisson-Lognormal Distribution",
#                "Poisson-Tweedie", # http://cran.r-project.org/web/packages/poistweedie/poistweedie.pdf
               "Sichel" = "The Sichel Distribution",
               "Skellam" = "The Skellam Distribution",
#                "WNCHypergeometric" = "Wallenius' Noncentral Hypergeometric Distribution",
               "Yule-Simon" = "The Yule-Simon Distribution",
               "Zeta" = "The Zeta Distribution",
               "Zipf" = "The Zipf Distribution")
    })
    
################   PARAMETRIZATION   ###########################################
    output$parametrization <- renderUI({
        if ( is.null(input$distribution) || 
            !(input$distribution %in% c("Beta-Binomial",
                                        "Beta-Geometric",
                                        "Delaporte")
              )
            ) {
            return()
        }
        
        switch(input$distribution,
               "Beta-Binomial" = ,
               "Beta-Geometric" = selectInput("betaParam", 
                                             label = "Choose a parametrization",
                                             choices = c("μ-σ", 
                                                         "α-β"),
                                             selected = "α-β"),
               "Delaporte" = selectInput("delaporteParam", 
                                         label = "Choose a parametrization",
                                         choices = c("μ-σ-ν", 
                                                     "α-β-λ"),
                                         selected = "α-β-λ")
               )
    })
    
################   FIRST PARAMETER   ###########################################
    output$param1 <- renderUI({
        if (is.null(input$distribution)) {
            return()
        }
        
        switch(input$distribution, 
               "Beta-Binomial" = ,
               
               "Beta-Geometric" = switch(input$betaParam,
                                        "μ-σ" = sliderInput(
                                            "beta_mu", 
                                            "Value of μ:", 
                                            min = 0.05,  
                                            max = 0.95, 
                                            value = 0.5,
                                            step = 0.05),
                                        "α-β" = sliderInput(
                                            "beta_logalpha", 
                                            "Value of log(α):", 
                                            min = -2.3,  
                                            max = 7, 
                                            value = 0,
                                            step = 0.1)
                                        ),
               
               "Negative-Binomial" = ,
               
               "Logarithmic" = ,
               
               "Geometric" = ,
               
               "Binomial" = sliderInput("p", 
                                        "Value of p:", 
                                        min = 0.01,  
                                        max = 0.99, 
                                        value = 0.5,
                                        step = 0.01
                                        ),
               
               "Borel" = ,
               
               "Borel-Tanner" = sliderInput("borel_mu", 
                                     "Value of μ:", 
                                     min = 0.01,  
                                     max = 0.99, 
                                     value = 0.5,
                                     step = 0.01
                                     ),
               
               "COM-Poisson" = sliderInput("com_poisson_lambda", 
                                           "Value of λ:", 
                                           min = 0.1,  
                                           max = 5, 
                                           value = 1,
                                           step = 0.1
                                           ),
               
               "Delaporte" = switch(input$delaporteParam,
                                    "μ-σ-ν" = sliderInput(
                                        "delaporte_mu", 
                                        "Value of μ:", 
                                        min = 0.1,  
                                        max = 10, 
                                        value = 0.5,
                                        step = 0.1),
                                    "α-β-λ" = sliderInput(
                                        "delaporte_alpha", 
                                        "Value of α:", 
                                        min = 0.05,  
                                        max = 5, 
                                        value = 0.5,
                                        step = 0.05)
                                    ),
               
               "WNCHypergeometric" = ,
               
               "FNCHypergeometric" = sliderInput("nch_logodds", 
                                                 "Value of log(odds):", 
                                                 min = -3,  
                                                 max = 3, 
                                                 value = 0,
                                                 step = 0.1
                                                 ),
               
               "Haight's Zeta" = sliderInput("hzeta_a", 
                                             "Value of α:", 
                                             min = 0.05,  
                                             max = 5, 
                                             value = 1,
                                             step = 0.05
               ),
               
               "Hypergeometric" = sliderInput("hyper_N",
                                              "Population size:",
                                              min = 3,
                                              max = 30,
                                              value = 15,
                                              step = 1
                                              ), 
               
               "Poisson" = sliderInput("poisson_lambda", 
                                       "Value of λ:", 
                                       min = 0.5,  
                                       max = 15, 
                                       value = 1,
                                       step = 0.1
                                       ),
               
               "Poisson-Lognormal" = sliderInput("poislog_mu", 
                                                 "Value of μ:", 
                                                 min = -3,  
                                                 max = 3, 
                                                 value = 0,
                                                 step = 0.1
                                                ),
               
               "Sichel" = sliderInput("sichel_mu", 
                                      "Value of μ:", 
                                      min = 0.1,  
                                      max = 10, 
                                      value = 1,
                                      step = 0.1
                                      ),
               
               "Skellam" = sliderInput("skellam_mu1", 
                                       "Value of μ₁:", 
                                       min = 0.5,  
                                       max = 5, 
                                       value = 1,
                                       step = 0.1
                                       ),
               
               "Yule-Simon" = sliderInput("yulesimon_rho", 
                                          "Value of ρ:", 
                                          min = 0.05,  
                                          max = 3, 
                                          value = 1,
                                          step = 0.05
                                          ),
               
               "Zeta" = sliderInput("zeta_s", 
                                    "Value of s:", 
                                    min = 1.01,  
                                    max = 5, 
                                    value = 2,
                                    step = 0.01
                                    ),
               
               "Zipf" = sliderInput("zipf_s", 
                                    "Value of s:", 
                                    min = 0.01,  
                                    max = 4, 
                                    value = 1,
                                    step = 0.01
                                    )
               
               )
    })
    
################   SECOND PARAMETER   ##########################################
    output$param2 <- renderUI({
        if (is.null(input$distribution) || 
            !(input$distribution %in% c("Beta-Binomial", 
                                        "Beta-Geometric",
                                        "Binomial",
                                        "Borel-Tanner",
                                        "COM-Poisson",
                                        "Delaporte",
                                        "FNCHypergeometric",
                                        "Hypergeometric",
                                        "Negative-Binomial",
                                        "Poisson-Lognormal",
                                        "Sichel",
                                        "Skellam",
                                        "WNCHypergeometric",
                                        "Zipf")
              )
            ) {
            return()
        }

        switch(input$distribution, 
               "Beta-Binomial" = ,
               
               "Beta-Geometric" = switch(input$betaParam,
                                        "μ-σ" = sliderInput(
                                            "beta_logsigma", 
                                            "Value of log(σ):", 
                                            min = -7,  
                                            max = 3, 
                                            value = 0,
                                            step = 0.1),
                                        "α-β" = sliderInput(
                                            "beta_logbeta", 
                                            "Value of log(β):", 
                                            min = -2.3,  
                                            max = 7, 
                                            value = 0,
                                            step = 0.1)
                                        ),
               
               "Binomial" = sliderInput("binomial_n",
                                        "Number of tosses:",
                                        min = 1,
                                        max = 30,
                                        value = 10,
                                        step = 1
                                        ),
               
               "Borel-Tanner" = sliderInput("borel_n",
                                            "Initial number of individuals:",
                                            min = 1,
                                            max = 10,
                                            value = 5,
                                            step = 1
                                            ),
               
               "COM-Poisson" = sliderInput("com_poisson_nu",
                                           "Value of ν:",
                                           min = 0.5,
                                           max = 10,
                                           value = 1,
                                           step = 0.1
                                           ),
               
               "Delaporte" = switch(input$delaporteParam,
                                    "μ-σ-ν" = sliderInput(
                                        "delaporte_sigma", 
                                        "Value of σ:", 
                                        min = 0.05,  
                                        max = 5, 
                                        value = 0.5,
                                        step = 0.05),
                                    "α-β-λ" = sliderInput(
                                        "delaporte_beta", 
                                        "Value of β:", 
                                        min = 0.05,  
                                        max = 5, 
                                        value = 0.5,
                                        step = 0.05)
                                    ),
               
               "WNCHypergeometric" = ,
               
               "FNCHypergeometric" = sliderInput("nch_nred",
                                                 "Number of red balls:",
                                                 min = 1,
                                                 max = 10,
                                                 value = 5,
                                                 step = 1),
               
               "Hypergeometric" = sliderInput("hyper_K",
                                              "Population # of successes:",
                                              min = 2,
                                              max = input$hyper_N,
                                              value = 2,
                                              step = 1),
               
               "Negative-Binomial" = sliderInput("nBinom_r",
                                                 "Number of failures r:",
                                                 min = 1,
                                                 max = 20,
                                                 value = 5,
                                                 step = 1),
               "Poisson-Lognormal" = sliderInput("poislog_sigma", 
                                                 "Value of σ:", 
                                                 min = 0.1,  
                                                 max = 3, 
                                                 value = 1,
                                                 step = 0.1
                                                 ),
               
               "Sichel" = sliderInput("sichel_sigma", 
                                      "Value of σ:", 
                                      min = 0.1,  
                                      max = 3, 
                                      value = 1,
                                      step = 0.1
                                      ),
               
               "Skellam" = sliderInput("skellam_mu2", 
                                       "Value of μ₂:", 
                                       min = 0.5,  
                                       max = 5, 
                                       value = 1,
                                       step = 0.1
                                       ),
               
               "Zipf" = sliderInput("zipf_N", 
                                    "Value of N:", 
                                    min = 2,  
                                    max = 15, 
                                    value = 5,
                                    step = 1
                                    )
        )
    })
    
################   THIRD PARAMETER   ###########################################
    output$param3 <- renderUI({
        if ( is.null(input$distribution) || 
            !(input$distribution %in% c("Beta-Binomial",
                                        "Delaporte",
                                        "FNCHypergeometric",
                                        "Hypergeometric",
                                        "Sichel",
                                        "WNCHypergeometric")
              )
            ) {
            return()
        }
        switch(input$distribution, 
               
               "Beta-Binomial" = sliderInput("betaBinom_n",
                                             "Number of tosses:",
                                             min = 1,
                                             max = 30,
                                             value = 10,
                                             step = 1),
               
               "Delaporte" = switch(input$delaporteParam,
                                    "μ-σ-ν" = sliderInput(
                                        "delaporte_nu", 
                                        "Value of ν:", 
                                        min = 0.05,  
                                        max = 0.95, 
                                        value = 0.5,
                                        step = 0.05),
                                    "α-β-λ" = sliderInput(
                                        "delaporte_lambda", 
                                        "Value of λ:", 
                                        min = 0.1,  
                                        max = 5, 
                                        value = 0.5,
                                        step = 0.1)
                                    ),
               
               "WNCHypergeometric" = ,
               
               "FNCHypergeometric" = sliderInput("nch_nwhite",
                                                 "Number of white balls:",
                                                 min = 2,
                                                 max = 10,
                                                 value = 5,
                                                 step = 1),
               
               "Hypergeometric" =  sliderInput("hyper_n",
                                               "Number of draws:",
                                                min = 1,
                                                max = input$hyper_K,
                                                value = 1,
                                                step = 1),
               
               "Sichel" = sliderInput("sichel_nu",
                                      "Value of ν:",
                                      min = -3,
                                      max = 3,
                                      value = 0,
                                      step = 0.1)
               
               )
    })
    
################   FOURTH PARAMETER   ##########################################
    output$param4 <- renderUI({
        if ( is.null(input$distribution) || 
                 !(input$distribution %in% c("FNCHypergeometric",
                                             "WNCHypergeometric")
                 )
        ) {
            return()
        }
        switch(input$distribution, 
               "WNCHypergeometric" = ,
               
               "FNCHypergeometric" = sliderInput("nch_nsampled",
                                                 "Number of balls sampled:",
                                                 min = 1,
                                                 max = input$nch_nred + input$nch_nwhite - 1,
                                                 value = 1,
                                                 step = 1),
               )
    })

################   CALCULATION   ###############################################

    distribution_output <- reactive({
        if (is.null(input$distribution)){
            return()
        }
        
        switch(input$distribution,
               
               "Beta-Binomial" = switch(input$betaParam,
                                        "μ-σ" = beta_binomial_mu_sigma(input$beta_mu,
                                                                                   exp(input$beta_logsigma),
                                                                                   input$betaBinom_n),
                                        "α-β" = beta_binomial_ab(exp(input$beta_logalpha),
                                                                        exp(input$beta_logbeta),
                                                                        input$betaBinom_n)
                                        ),
               
               "Beta-Geometric" = switch(input$betaParam,
                                         "μ-σ" = beta_geometric_mu_sigma(input$beta_mu,
                                                                         exp(input$beta_logsigma)),
                                         "α-β" = beta_geometric_ab(exp(input$beta_logalpha),
                                                                          exp(input$beta_logbeta)),
                                         ),
               
               "Binomial" = binomial_dist(input$p, input$binomial_n),
               
               "Borel" = borel(input$borel_mu),
               
               "Borel-Tanner" = borel_tanner(input$borel_mu, input$borel_n),
               
               "COM-Poisson" = com_poisson(input$com_poisson_lambda, input$com_poisson_nu),
               
               "Delaporte" = switch(input$delaporteParam,
                                    "μ-σ-ν" = delaporte_msn(input$delaporte_mu,
                                                                  input$delaporte_sigma,
                                                                  input$delaporte_nu),
                                    "α-β-λ" = delaporte_abl(input$delaporte_alpha,
                                                                        input$delaporte_beta,
                                                                        input$delaporte_lambda)
                                    ),
               
               "FNCHypergeometric" = fishers_noncentral_hypergeometric(exp(input$nch_logodds),
                                                                       input$nch_nred,
                                                                       input$nch_nwhite,
                                                                       input$nch_nsampled),
               
               "Geometric" = geometric_p(input$p),
               
               "Haight's Zeta" = haights_zeta(input$hzeta_a), 
               
               "Hypergeometric" = hypergeometric(input$hyper_n,
                                                 input$hyper_N,
                                                 input$hyper_K),
               
               "Logarithmic" = logarithmic(input$p),
               
               "Negative-Binomial" = negative_binomial_pr(input$p, input$nBinom_r),
               
               "Poisson" = poisson_dist(input$poisson_lambda),
               
               "Poisson-Lognormal" = poisson_lognormal(input$poislog_mu, input$poislog_sigma),
               
               "Sichel" = sichel(input$sichel_mu, input$sichel_sigma, input$sichel_nu),
               
               "Skellam" = skellam_dist(input$skellam_mu1, input$skellam_mu2),
               
               "WNCHypergeometric" = wallenius_noncentral_hypergeometric(exp(input$nch_logodds),
                                                                         input$nch_nred,
                                                                         input$nch_nwhite,
                                                                         input$nch_nsampled),
               
               "Yule-Simon" = yule_simon(input$yulesimon_rho),
               
               "Zeta" = zeta_dist(input$zeta_s),
               
               "Zipf" = zipf_dist(input$zipf_s, input$zipf_N)
               
        )
    })


        

################   PLOT   ######################################################
    output$plot <- renderPlot({
        if (is.null(input$distribution)){
            return()
        }
        
        discrete_plot(distribution_output())
    })

################   DISTRIBUTION DESCRIPTION   ##################################
    output$description <- renderUI({
        if (is.null(input$distribution)){
            return()
        }
        withMathJax(
            distribution_output()$text
#         helpText(distribution_output()$text)
        )
    })
    
################   DISTRIBUTION PROPERTIES   ###################################
#     output$stats <- renderTable({
#         stats <- data.frame(Value = round(c(distribution_output()$mean,
#                                             distribution_output()$variance),
#                                           2),
#                    row.names = c("Mean", "Variance"))
#         names(stats) <- NULL
#         stats
#     })

    output$stats <- renderUI({
        if (is.null(input$distribution)){
            return()
        }
        withMathJax(
            distribution_output()$stats
        )
    })

})