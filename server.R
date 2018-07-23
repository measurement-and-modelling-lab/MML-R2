require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)
require(htmlTable) || install.packages(htmlTable)



shinyServer(function(input, output, session) {

    observe({
        options(shiny.sanitize.errors = FALSE)
    })

    ## Import a bunch of mass error checking functions
    source("errorcheck.R")

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
        ## The drop-down menu input, selectInput, doesn't have those, so we're using radioButtons even though the drop-down is slimmer/prettier
        html_ui <- paste0(radioButtons("calculation", "Calculation to run:",
                                       choiceNames = list(HTML("Confidence interval on R<sup>2</sup> (fixed regressor)"),
                                                          HTML("Confidence interval on R<sup>2</sup> (random regressor)"),
                                                          HTML("Power to reject R<sup>2</sup> = 0"),
                                                          HTML("Sample size to reject R<sup>2</sup> = 0"),
                                                          "Standardized regression coefficients",
                                                          HTML("Calculate R<sup>2</sup> from raw data")),
                                       choiceValues = list("fixedci", "randomci", "power", "samplesize", "beta", "r2")))
        HTML(html_ui)

    })

    output$valueInput <- renderUI({

        html_ui <- ""

        validate(need(input$calculation, ""))

        ## Create input widgets, pulling default values from global
        if (values$calculation == "fixedci") {
            html_ui <- paste0(numericInput("n", "Number of observations:", values$n, 3),
                              numericInput("k", "Number of variables, including criterion:", values$k, 2),
                              numericInput("r", "R squared:", values$r, 0, 1, 0.01),
                              numericInput("confidence", "Confidence level:", values$confidence, 0, 1, 0.01))
        } else if (values$calculation == "randomci") {
            html_ui <- paste0(numericInput("n", "Number of observations:", values$n, 3),
                              numericInput("k", "Number of variables, including criterion:", values$k, 2),
                              numericInput("r", "R squared:", values$r, 0, 1, 0.01),
                              numericInput("confidence", "Confidence level:", values$confidence, 0, 1, 0.01))
        } else if (values$calculation == "power") {
            html_ui <- paste0(numericInput("n", "Number of observations:", values$n, 3),
                              numericInput("k", "Number of variables, including criterion:", values$k, 2),
                              numericInput("rho", "Rho squared:", values$rho, 0, 1, 0.01),
                              numericInput("alpha", "Alpha:", values$alpha, 0, 1, 0.01))
        } else if (values$calculation == "samplesize") {
            html_ui <- paste0(numericInput("k", "Number of variables, including criterion:", values$k, 2),
                              numericInput("rho", "Rho squared:", values$rho, 0, 1, 0.01),
                              numericInput("alpha", "Alpha:", values$alpha, 0, 1, 0.01),
                              numericInput("power", "Power desired:", values$power, 0, 1, 0.01))
        } else if (values$calculation == "beta") {

            ## Check that data file has been uploaded
            validate(need(input$datafile, ""))


            ## Error checking and data import
            tryCatch({
                data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            }, warning = function(w) {
                stop("There was a problem reading one of your .csv files. You may need to add a blank line to the end of the file.")
            }, error = function(e) {
                stop("There was a problem reading one of your .csv files. You may need to add a blank line to the end of the file.")
            })
            if (ncol(data) > 16) {
                stop("You cannot have more than 16 variables.")
            }


            ## If the data is a correlation matrix, create sample size input
            if (nrow(data) == ncol(data)) {
                html_ui <- paste0(numericInput("n", "Number of observations:", values$n))
            } else {
                html_ui <- ""
            }


            ## Create confidence coefficient and FW error control inputs
            FW.choices <- c("uncorrected", "bonferroni", "sidak", "stepdown_bonferroni", "stepdown_sidak")
            names(FW.choices) <- c("None", "Bonferroni", "Dunn-Sidak", "Stepdown Bonferroni", "Stepdown Dunn-Sidak")
            options <- 1:ncol(data)
            names(options) <- options
            html_ui <- paste0(html_ui,
                              numericInput("confidence", "Confidence level:", values$confidence, 0, 1, 0.01),
                              radioButtons("familywise","Familywise error control:", FW.choices),
                              div(style="display: inline-block;vertical-align:top; width: 100px;",
                                  checkboxGroupInput("predictors", "Predictors:", choices=options)),
                              div(style="display: inline-block;vertical-align:top; width: 50px;",
                                  radioButtons("criterion", "Criterion:", choices=options)))

        } else if (values$calculation == "r2") {

            ## Check that the data file has been uploaded
            validate(need(input$datafile, ""))


            ## Error checking / data import
            tryCatch({
                data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            }, warning = function(w) {
                stop("There was a problem reading your .csv file.")
            }, error = function(e) {
                stop("There was a problem reading your .csv file.")
            })
            if (ncol(data) > 16) {
                stop("You cannot have more than 16 variables.")
            }


            ## Create radio buttons for choosing the criterion and predictors
            html_ui <- ""
            options <- 1:ncol(data)
            names(options) <- options
            html_ui <- paste0(html_ui,
                              div(style="display: inline-block;vertical-align:top; width: 100px;",
                                  checkboxGroupInput("predictors", "Predictors:", options, values$predictors)),
                              div(style="display: inline-block;vertical-align:top; width: 50px;",
                                  radioButtons("criterion", "Criterion:", options, values$criterion)))
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
            areShort(input$n, input$k, input$r, input$confidence)
            areIntegers(input$n, input$k)
            areBetween0And1(input$r, input$confidence)
            if (input$k < 2) {
                stop("There must be at least two variables.")
            }
            if (input$n <= input$k) {
                stop("There must be more observations than variables.")
            }

            ## Run the test; if errors are encountered, return a generic error message
            tryCatch({
                fixedCI <- dget("fixedCI.R")
                new.output <- fixedCI(input$n, input$k, input$r, input$confidence)
            }, warning = function(w) {
                stop("Confidence interval calculation failed.")
            }, error = function(e) {
                stop("Confidence interval calculation failed.")
            })


            ## Round output
            new.output <- round(new.output, 5)
            new.output[new.output == 1] <- "> 0.99999"
            new.output[new.output == 0] <- "< 0.00001"


            ## Format the output table
            new.output <- htmlTable(new.output,
                                    caption = "<b>Confidence Interval (Fixed Regressor)</b>",
                                    foot = paste0("N=", input$n, ", k=", input$k, ", R<sup>2</sup>=", input$r, ", 1-&alpha;=", input$confidence.level),
                                    css.cell = "padding-left: 2em; padding-right: 2em;")

        } else if (input$calculation == "randomci") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$r, input$confidence)


            ## Error checking
            areShort(input$n, input$k, input$r, input$confidence)
            areIntegers(input$n, input$k)
            areBetween0And1(input$r, input$confidence)
            if (input$k < 2) {
                stop("There must be at least two variables!")
            }
            if (input$n <= input$k) {
                stop("There must be more observations than variables.")
            }

            
            ## Run the test; if errors are encountered, return a generic error message
            tryCatch({
                randomCI <- dget("randomCI.R")
                new.output <- randomCI(input$n, input$k, input$r, input$confidence)
            }, warning = function(w) {
                stop("Confidence interval calculation failed.")
            }, error = function(e) {
                stop("Confidence interval calculation failed.")
            })


            ## Round output
            new.output <- round(new.output, 5)
            new.output[new.output == 1] <- "> 0.99999"
            new.output[new.output == 0] <- "< 0.00001"
            

            ## Format output table
            new.output <- htmlTable(new.output,
                                    caption = "<b>Confidence Interval (Random Regressor)</b>",
                                    tfoot = paste0("N=", input$n, ", k=", input$k, ", R<sup>2</sup>=", input$r, ", 1-&alpha;=", input$confidence.level),
                                    css.cell = "padding-left: 2em; padding-right: 2em;")

        } else if (input$calculation == "power") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$rho, input$alpha)


            ## Error checking
            areShort(input$n, input$k, input$rho, input$alpha)
            areIntegers(input$n, input$k)
            areBetween0And1(input$rho, input$alpha)
            if (input$k < 2) {
                stop("There must be at least two variables!")
            }
            if (input$n <= input$k) {
                stop("There must be more observations than variables.")
            }


            ## Run the test; if errors are encountered, return a generic error message
            Power <- dget("Power.R")
            tryCatch({
                new.output <- Power(input$n, input$k, input$rho, input$alpha)
            }, warning = function(w) {
                stop("Power calculation failed.")
            }, error = function(e) {
                stop("Power calculation failed.")
            })


            ## Round output
            new.output <- round(new.output, 5)
            if (new.output == 1) {
                new.output <- "> 0.99999"
            } else if (new.output == 0) {
                new.output <- "< 0.00001"
            } else {
                new.output <- paste0("= ", new.output)
            }


            ## Format output in html
            foot <- paste0("N=", input$n, ", k=", input$k, ", &rho;=", input$rho, ", &alpha;=", input$alpha)
            new.output <- paste0("<p><b>Power</b><br>", "Power ", new.output, "<br>", foot, "<p>")

        } else if (input$calculation == "samplesize") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$k, input$rho, input$alpha, input$power)


            ## Error checking
            areShort(input$k, input$rho, input$alpha, input$power)
            areIntegers(input$k)
            areBetween0And1(input$rho, input$alpha, input$power)
            if (input$k < 2) {
                stop("There must be at least two variables.")
            }


            ## Run the test; if errors are encountered, return a generic error message
            SampleSize <- dget("SampleSize.R")
            tryCatch({
                new.output <- SampleSize(input$k, input$rho, input$alpha, input$power)
            }, warning = function(w) {
                stop("Sample size calculation failed.")
            }, error = function(e) {
                stop("Sample size calculation failed.")
            })


            ## Round output
            new.output <- round(new.output, 5)
            new.output[new.output == 1] <- "> 0.99999"
            new.output[new.output == 0] <- "< 0.00001"


            ## Assemble output table
            new.output <- htmlTable(new.output,
                                    caption = "<b>Sample Size</b>",
                                    tfoot = paste0("k=", input$k, ", &rho;=", input$rho, ", &alpha;=", input$alpha, ", 1-&beta;=", input$power),
                                    css.cell = "padding-left: .5em; padding-right: .2em;")


        } else if (input$calculation == "beta") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$datafile, input$predictors, input$criterion, input$confidence, input$familywise)
            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            if (ncol(data) == nrow(data)) {
                validate(need(input$n, ""))
            }

            ## Run the test
            Beta <- dget("Beta.R")
            new.output <- Beta(data, input$n, input$criterion, input$predictors, input$familywise, input$confidence)

            ## Format output table
            FW <- c("None", "Bonferroni", "Dunn-Sidak", "Stepdown Bonferroni", "Stepdown Dunn-Sidak")
            names(FW) <- c("uncorrected", "bonferroni", "sidak", "stepdown_bonferroni", "stepdown_sidak")
            FW <- FW[[input$familywise]]
            new.output <- htmlTable(new.output,
                                    css.cell = "padding-left: .5em; padding-right: .5em;",
                                    caption = "<b>Standardized Beta Coefficients</b>",
                                    cgroup = c("Estimates", ""),
                                    tfoot = paste0("Y=",input$criterion,", X=",paste(input$predictors, collapse=","), ", 1-&alpha;=", 1-input$alpha, ", FW=", FW),
                                    n.cgroup = c(3,4))

        } else if (input$calculation == "r2") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$datafile, input$predictors, input$criterion)

            ## Import arguments
            predictors <- as.numeric(input$predictors)
            criterion <- as.numeric(input$criterion)
            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))

            ## Run the test
            R2 <- dget("R2.R")
            new.output <- R2(data, predictors, criterion)

            ## Assemble output table
            foot <- paste0("Y=", criterion, ", X=", paste(predictors, collapse=","))
            new.output <- paste0("<p><b>Squared Multiple Correlation</b><br>", "R<sup>2</sup> ", new.output, "<br>", foot, "<p>")

        }

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
