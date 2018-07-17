require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)
require(htmlTable) || install.packages(htmlTable)

shinyServer(function(input, output, session) {

    ## Validate(Need()) 
    massValidateNeed <- function(...) {
        arguments <- list(...)
        for (a in arguments) {
            validate(need(a, ""))
        }
    }

    ## Determine whether several inputs are integers
    areIntegers <- function(...) {
        arguments <- list(...)
        for (a in arguments) {
            if (grepl("\\D", a) || grepl("\\D", a)) {
                stop("Invalid input.")
            }
        }
    }

    ## Determine whether several inputs are numeric
    areNumeric <- function(...) {
        arguments <- list(...)
        for (a in arguments) {
            a <- as.numeric(a)
            if (is.na(a)) {
                stop("Invalid input.")
            }
        }
    }


    ## Determine whether several numbers are valid correlations
    areSquaredMCs <- function(...) {
        arguments <- list(...)
        for (a in arguments) {
            a <- as.numeric(a)
            if (a < 0 || a > 1) {
                stop("Invalid input.")
            }
        }
    }


    ## Determine whether a confidence level is valid
    areValidCLs <- function(...) {
        arguments <- list(...)
        for (a in arguments) {
            a <- as.numeric(a)
            if (a <= .6 || a >= .999) {
                stop("Invalid input.")
            }
        }
    }


    areProbabilities <- function(...) {
        arguments <- list(...)
        for (a in arguments) {
            a <- as.numeric(a)
            if (a <= 0 || a >= 1) {
                stop("Invalid input.")
            }
        }
    }

    ## Global values
    values <- reactiveValues()
    values$n <- ""
    values$k <- ""
    values$r <- ""
    values$rho <- ""
    values$confidence <- ""
    values$alpha <- ""
    values$power <- ""
    values$percentage <- ""
    values$calculation <- ""
    values$predictors <- ""
    values$criterion <- ""
    values$old.output <-c("","","","","","","","","","")



    output$selector <- renderUI({ ## This is necessary to get subscripts and superscripts

        ## You can't use HTML() in the choices argument, only in the two punch combo choiceNames/choiceValues
        ## The drop-down menu input, selectInput, doesn't have those, so we're using radioButtons instead
        html_ui <- paste0(radioButtons("calculation", "Calculation to run:",
                                       choiceNames = list(HTML("Confidence interval on R<sup>2</sup> (fixed regressor)"),
                                                          HTML("Confidence interval on R<sup>2</sup> (random regressor)"),
                                                          HTML("Power to reject H<sub>0</sub>: R<sup>2</sup> = 0"),
                                                          HTML("Sample size to reject H<sub>0</sub>: R<sup>2</sup> = 0"),
                                                          "Estimate standardized regression coefficients",
                                                          HTML("Calculate R<sup>2</sup> from raw data")),
                                       choiceValues = list("fixedci", "randomci", "power", "samplesize", "beta", "r2")))
        HTML(html_ui)

    })

    output$valueInput <- renderUI({

        html_ui <- ""

        validate(need(input$calculation, ""))

        ## Create input widgets, pulling default values from global
      	if (values$calculation == "fixedci") {
            html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                              textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("r", "R squared:", values$r),
                              textInput("confidence", "Confidence level:", values$confidence, placeholder = 0.95))
        } else if (values$calculation == "randomci") {
            html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                              textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("r", "R squared:", values$r),
                              textInput("confidence", "Confidence level:", values$confidence, placeholder = 0.95))
        } else if (values$calculation == "power") {
            html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                              textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("rho", "Rho squared:", values$rho),
                              textInput("alpha", "Alpha:", values$alpha, placeholder = 0.05))
        } else if (values$calculation == "samplesize") {
            html_ui <- paste0(textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("rho", "Rho squared:", values$rho),
                              textInput("alpha", "Alpha:", values$alpha, placeholder = 0.05),
                              textInput("power", "Power desired:", values$power, placeholder = 0.8))
        } else if (values$calculation == "beta") {
            
            validate(need(input$datafile, "")) ## Check that data file has been uploaded

            ## Check that R can read the data file as a .csv
            tryCatch({
                read.csv(file=input$datafile[[4]], head=FALSE, sep=",")
            }, warning = function(w) {
                stop("There was a problem reading your .csv file.")
            }, error = function(e) {
                stop("There was a problem reading your .csv file.")
            })

            ## Load data and count the number of variables
            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            variables <- ncol(data)

            if (variables > 16) {
	      	stop("You cannot have more than 16 variables.")
            }

            options <- 1:variables
            names(options) <- options
            html_ui <- ''


            ## If the data is a correlation matrix, create sample size input
            if (nrow(data) == ncol(data)) {
                html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
            }


            ## Create confidence coefficient and method input
            html_ui <- paste0(html_ui,
                              textInput("confidence",
                                        "Confidence level:",
                                        values$confidence,
                                        placeholder = 0.95),
                              radioButtons("familywise",
                                           "Familywise error control:",
                                           choices = c("None"="uncorrected",
                                                       "Bonferroni"="bonferroni",
                                                       "Dunn-Sidak"="sidak",
                                                       "Stepdown Bonferroni"="stepdown_bonferroni",
                                                       "Stepdown Dunn-Sidak"="stepdown_sidak")),
                              div(style="display: inline-block;vertical-align:top; width: 100px;",
                                  checkboxGroupInput("predictors", "Predictors:",
                                                     choices=options)),
                              div(style="display: inline-block;vertical-align:top; width: 50px;",
                                  radioButtons("criterion", "Criterion:", choices=options))
                              )

        } else if (values$calculation == "r2") {

            validate(need(input$datafile, "")) ## Check that the data file has been uploaded

            ## Check that R can read the data file as a .csv
            tryCatch({
                read.csv(file=input$datafile[[4]], head=FALSE, sep=",")
            }, warning = function(w) {
                stop("There was a problem reading your .csv file.")
            }, error = function(e) {
                stop("There was a problem reading your .csv file.")
            })

            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            variables <- ncol(data)
            
            if (variables > 16) {
	      	stop("You cannot have more than 16 variables.")
            }

            options <- 1:variables
            names(options) <- options
            html_ui <- ""

            ## Create radio buttons for choosing the predictors
            html_ui <- paste0(html_ui,
                              div(style="display: inline-block;vertical-align:top; width: 100px;",
                                  checkboxGroupInput("predictors",
                                                     "Predictors:",
                                                     options,
                                                     values$predictors)))

            ## Create radio buttons for choosing the criterion
            html_ui <- paste0(html_ui,
                              div(style="display: inline-block;vertical-align:top; width: 50px;",
                                  radioButtons("criterion",
                                               "Criterion:",
                                               options,
                                               values$criterion)))

        }
        HTML(html_ui)
    })
    
    ## If calculation is changed, send current widget values to global, then update global calculation value to whatever calculator it's changing to
    observeEvent(input$calculation, {
        values$n <- input$n
        values$k <- input$k
        values$r <- input$r
        values$confidence <- input$confidence
        values$rho <- input$rho
        values$alpha <- input$alpha
        values$power <- input$power
        values$percentage <- input$percentage
        values$calculation <- input$calculation
        values$predictors <- input$predictors
        values$criterion <- input$criterion
    })

    R2 <- eventReactive(input$runButton, {
        
        if (input$calculation == "fixedci") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$r, input$confidence)

            ## Error checking
            areIntegers(input$n, input$k)
            areNumeric(input$r, input$confidence)
            areSquaredMCs(input$r)
            areValidCLs(input$confidence)

            ## Convert to numeric
            n <- as.integer(input$n)
            k <- as.integer(input$k)
            r <- as.numeric(input$r)
            confidence.level <- as.numeric(input$confidence)

            ## Run the tests
            fixedCI <- dget("fixedCI.R")
            foot <- paste0("N=", n, ", k=", k, ", R<sup>2</sup>=", r, ", 1-&alpha;=", confidence.level)
            new.output <- fixedCI(n, k, r, confidence.level)
            new.output <- htmlTable(new.output,
                                    caption = "Confidence Interval (Fixed Regressor)",
                                    css.cell = "padding-left: 2em; padding-right: 2em;",
                                    tfoot = foot)

        } else if (input$calculation == "randomci") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$r, input$confidence)

            ## Error checking
            areIntegers(input$n, input$k)
            areNumeric(input$r, input$confidence)
            areSquaredMCs(input$r)
            areValidCLs(input$confidence)

            ## Convert to numeric
            n <- as.integer(input$n)
            k <- as.integer(input$k)
            r <- as.numeric(input$r)
            confidence.level <- as.numeric(input$confidence)
    
            ## Run the test
            randomCI <- dget("randomCI.R")
            new.output <- randomCI(n, k, r, confidence.level)
            foot <- paste0("N=", n, ", k=", k, ", R<sup>2</sup>=", r, ", 1-&alpha;=", confidence.level)
            new.output <- randomCI(n, k, r, confidence.level)
            new.output <- htmlTable(new.output,
                                    caption = "Confidence Interval (Random Regressor)",
                                    css.cell = "padding-left: 2em; padding-right: 2em;",
                                    tfoot = foot)
            
        } else if (input$calculation == "power") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$rho, input$alpha)

            ## Error checking
            areIntegers(input$n, input$k)
            areNumeric(input$rho, input$alpha)
            areSquaredMCs(input$rho)
            areProbabilities(input$alpha)

            ## Convert to numeric
            n <- as.integer(input$n)
            k <- as.integer(input$k)
            rho <- as.numeric(input$rho)
            alpha <- as.numeric(input$alpha)

            Power <- dget("Power.R")
            new.output <- Power(n, k, rho, alpha)
            foot <- paste0("N=", n, ", k=", k, ", &rho;=", rho, ", &alpha;=", alpha)
            new.output <- htmlTable(new.output,
                                    caption = "Power Calculation",
                                    tfoot = foot)

        } else if (input$calculation == "samplesize") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$k, input$rho, input$alpha, input$power)

            ## Error checking
            areIntegers(input$n)
            areNumeric(input$rho, input$alpha, input$power)
            areSquaredMCs(input$rho)
            areProbabilities(input$alpha, input$power)

            ## Convert to numeric
            k <- as.integer(input$k)
            rho <- as.numeric(input$rho)
            alpha <- as.numeric(input$alpha)
            power <- as.numeric(input$power)

            SampleSize <- dget("SampleSize.R")
            new.output <- SampleSize(k, rho, alpha, power)
            foot <- paste0("k=", k, ", &rho;=", rho, ", &alpha;=", alpha, ", 1-&beta;=", power)
            new.output <- htmlTable(new.output,
                                    caption = "Sample Size Calculation",
                                    css.cell = "padding-left: .5em; padding-right: .2em;",
                                    tfoot = foot)


        } else if (input$calculation == "beta") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$datafile, input$predictors, input$criterion, input$confidence, input$familywise)

            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            
            ## Error checking
            if (as.character(input$criterion) %in% input$predictors) {
	      	stop("A variable cannot be both a predictor and the criterion.")
            }
            if (length(input$predictors) < 2) {
	      	stop("You must have at least two predictors.")
            }
            if (NA %in% as.numeric(data)) {
	      	stop("Your data has missing or non-numeric elements.")
            }

            ## Define arguments
            if (ncol(data) != nrow(data)) {
                N <- nrow(data)
                data <- cov(data)
            } else {
                areIntegers(input$n)
                N <- as.numeric(input$n)
            }
            predictors <- as.numeric(input$predictors)
            criterion <- as.numeric(input$criterion)
            familywise <- input$familywise
            cx <- data[predictors, predictors]
            cxy <- data[predictors, criterion]
            vy <- data[criterion, criterion]
            alpha <- 1 - as.numeric(input$confidence)

            Beta <- dget("Beta.R")
            new.output <- Beta(cx, cxy, vy, N, alpha, familywise)
            foot <-paste0("Y=",criterion,", X=",paste(predictors, collapse=","))
            new.output <- htmlTable(new.output,
                                    css.cell = "padding-left: .5em; padding-right: .5em;",
                                    caption = "Standardized Beta Coefficient Estimates",
                                    cgroup = c("Estimates", ""),
                                    n.cgroup = c(3,4),
                                    tfoot = foot)

        } else if (input$calculation == "r2") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$datafile, input$predictors, input$criterion)

            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))

            ## Error checking
            if (as.character(input$criterion) %in% input$predictors) {
	      	stop("A variable cannot be both a predictor and the criterion.")
            }
            if (length(input$predictors) < 2) {
	      	stop("You must have at least two predictors.")
            }
            if (NA %in% as.numeric(data)) {
	      	stop("Your data has missing or non-numeric elements.")
            }
            
            ## Define arguments
            predictors <- as.numeric(input$predictors)
            criterion <- as.numeric(input$criterion)

            ## Run the test
            R2 <- dget("R2.R")
            new.output <- R2(data, predictors, criterion)
            new.output <- htmlTable(new.output,
                                    caption = "Squared Multiple Correlation Calculation",
                                    tfoot = paste0("Y=", criterion, ", X=", paste(predictors, collapse=",")))

        }

        print(new.output)

        ## Print the current output plus the last 9
        ## But only if the new output is different from the previous one
        if (new.output != values$old.output[1]) {
            values$old.output <- c(new.output, values$old.output)[1:10]
        }
        html_output <- paste(values$old.output, collapse="")
        HTML(html_output)

    })

    output$finaloutput <- renderUI({ 
        R2()
    })

})
