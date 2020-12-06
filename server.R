require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)
require(htmlTable) || install.packages(htmlTable)


shinyServer(function(input, output, session) {

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
    values$output.old <-c("","","","","","","","","","")

    ## Show actual error messages
    observe({
        options(shiny.sanitize.errors = FALSE)
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

    ## Create input widgets, pulling default values from global
    output$R2Input <- renderUI({

        validate(need(input$calculation, ""))

        html_ui <- ""
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

            ## Check that the data file is readable
            cat("\n", file=input$datafile[[4]], append = TRUE) ## append the necessary line break to the end
            tryCatch(data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=",")),
                     warning = function(w) stop("There was a problem reading your .csv file."),
                     error = function(e) stop("There was a problem reading your .csv file.."))

            ## Limit the number of variables their data can have
            if (ncol(data) > 16) stop("You cannot have more than 16 variables.")

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

            ## Error checking and data import
            cat("\n", file=input$datafile[[4]], append = TRUE) ## append the necessary line break to the end
            tryCatch(data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=",")),
                     warning = function(w) stop("There was a problem reading your .csv file."),
                     error = function(e) stop("There was a problem reading your .csv file."))

            ## Limit the number of variables their data can have
            if (ncol(data) > 16) stop("You cannot have more than 16 variables.")

            ## Create radio buttons for choosing the criterion and predictors
            options <- 1:ncol(data)
            names(options) <- options
            html_ui <- paste0(div(style="display: inline-block;vertical-align:top; width: 100px;",
                                  checkboxGroupInput("predictors", "Predictors:", options, values$predictors)),
                              div(style="display: inline-block;vertical-align:top; width: 50px;",
                                  radioButtons("criterion", "Criterion:", options, values$criterion)))
        }

        HTML(html_ui)
    })


    R2_main <- eventReactive(input$runButton, { ## Run tests when run button is pressed

        ## Import functions used by 2 or more tests
        datacheck <- dget("datacheck.R")
        source("errorcheck.R")

        if (input$calculation == "fixedci") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$r, input$confidence)

            ## Error checking
            areShort(input$n, input$k, input$r, input$confidence)
            areIntegers(input$n, input$k)
            areBetween0And1(input$r, input$confidence)
            if (input$k < 2) stop("There must be at least two variables.")
            if (input$n <= input$k) stop("Sample size must be greater than the number of variables.")

            ## Run the calculation
            fixedCI <- dget("fixedCI.R")
            fixedCI.output <- fixedCI(input$n, input$k, input$r, input$confidence)

            ## Format the output table
            output.new <- htmlTable(fixedCI.output,
                                    caption = "<b>Confidence Interval (Fixed Regressor)</b>",
                                    tfoot = paste0("N=", input$n, ", k=", input$k, ", R<sup>2</sup>=", input$r, ", 1-&alpha;=", input$confidence),
                                    css.cell = "padding-left: 2em; padding-right: 2em;")

        } else if (input$calculation == "randomci") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$r, input$confidence)

            ## Error checking
            areShort(input$n, input$k, input$r, input$confidence)
            areIntegers(input$n, input$k)
            areBetween0And1(input$r, input$confidence)
            if (input$k < 2) stop("There must be at least two variables.")
            if (input$n <= input$k) stop("Sample size must be greater than the number of variables.")

            ## Run calculation
            randomCI <- dget("randomCI.R")
            randomCI.output <- randomCI(input$n, input$k, input$r, input$confidence)

            ## Format output table
            output.new <- htmlTable(randomCI.output,
                                    caption = "<b>Confidence Interval (Random Regressor)</b>",
                                    tfoot = paste0("N=", input$n, ", k=", input$k, ", R<sup>2</sup>=", input$r, ", 1-&alpha;=", input$confidence),
                                    css.cell = "padding-left: 2em; padding-right: 2em;")

        } else if (input$calculation == "power") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$n, input$k, input$rho, input$alpha)

            ## Error checking
            areShort(input$k, input$rho, input$alpha)
            areIntegers(input$n, input$k)
            areBetween0And1(input$rho, input$alpha)
            if (input$k < 2) stop("There must be at least two variables.")
            if (input$n <= input$k) stop("Sample size must be greater than the number of variables.")

            ## Run the calculation
            Power <- dget("Power.R")
            power.output <- Power(input$n, input$k, input$rho, input$alpha, TRUE)

            ## Format output in html
            foot <- paste0("N=", input$n, ", k=", input$k, ", &rho;<sup>2</sup>=", input$rho, ", &alpha;=", input$alpha)
            output.new <- paste0("<p><b>Power</b><br>", "Power ", power.output, "<br>", foot, "<p>")

        } else if (input$calculation == "samplesize") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$k, input$rho, input$alpha, input$power)

            ## Check for errors
            areShort(input$k, input$rho, input$alpha, input$power)
            areIntegers(input$k)
            areBetween0And1(input$rho, input$alpha, input$power)
            if (input$k < 2) stop("There must be at least two variables.")

            ## Run the calculation
            SampleSize <- dget("SampleSize.R")
            samplesize.output <- SampleSize(input$k, input$rho, input$alpha, input$power)

            ## Assemble output table
            output.new <- htmlTable(samplesize.output,
                                    caption = "<b>Sample Size</b>",
                                    tfoot = paste0("k=", input$k, ", &rho;<sup>2</sup>=", input$rho, ", &alpha;=", input$alpha, ", 1-&beta;=", input$power),
                                    css.cell = "padding-left: .5em; padding-right: .2em;")

        } else if (input$calculation == "beta") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$datafile, input$predictors, input$criterion, input$confidence, input$familywise)
            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
            if (ncol(data) == nrow(data)) {
                validate(need(input$n, ""))
                n <- input$n
                areIntegers(input$n)
            } else {
                n <- nrow(data)
            }

            ## Error checking
            data <- datacheck(data)
            if (input$criterion %in% input$predictors) stop("A variable cannot be both a predictor and the criterion.")
            if (length(input$predictors) < 2) stop("You must have at least two predictors.")
            areBetween0And1(input$confidence)

            ## Run the calculation
            Beta <- dget("Beta.R")
            beta.output <- Beta(data, n, input$criterion, input$predictors, input$familywise, input$confidence)

            ## Format output table
            FW <- c("None", "Bonferroni", "Dunn-Sidak", "Stepdown Bonferroni", "Stepdown Dunn-Sidak")
            names(FW) <- c("uncorrected", "bonferroni", "sidak", "stepdown_bonferroni", "stepdown_sidak")
            FW <- FW[[input$familywise]]
            output.new <- htmlTable(beta.output,
                                    css.cell = "padding-left: .5em; padding-right: .5em;",
                                    caption = "<b>  Beta Coefficients</b>",
                                    cgroup = c("Estimates", ""),
                                    tfoot = paste0("Y=",input$criterion,", X=",paste(input$predictors, collapse=","), ", 1-&alpha;=", input$confidence, ", FW=", FW),
                                    n.cgroup = c(3,4))

        } else if (input$calculation == "r2") {

            ## Ensure that the necessary values have been entered
            massValidateNeed(input$datafile, input$predictors, input$criterion)

            ## Import arguments
            predictors <- as.numeric(input$predictors)
            criterion <- as.numeric(input$criterion)
            data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))

            ## Error checking
            data <- datacheck(data)
            print(data)
            print(predictors)
            print(criterion)
            if (input$criterion %in% input$predictors) stop("A variable cannot be both a predictor and the criterion.")

            ## Run the calculation
            R2 <- dget("R2.R")
            r2.output <- R2(data, predictors, criterion)

            ## Assemble output table
            foot <- paste0("Y=", criterion, ", X=", paste(predictors, collapse=","))
            output.new <- paste0("<p><b>Squared Multiple Correlation</b><br>", "R<sup>2</sup> = ", r2.output, "<br>", foot, "<p>")

        }

        ## Print the current output plus the last 9
        ## But only if the output.new is different from the previous one
        if (output.new != values$output.old[1]) {
            values$output.old <- c(output.new, values$output.old)[1:10]
        }
        html_output <- paste(values$output.old, collapse="")
        HTML(html_output)

    })


    ## Get values from R2_main() then send output to ui.R
    output$R2Output <- renderUI({
        R2_main()
    })

})
