shinyServer(function(input, output, session) {

	# Global values
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
    values$output <-list('')

    output$valueInput <- renderUI({

	  # Create input widgets for global calculation value, with default values from global
      html_ui <- ""
      if (values$calculation == "ci1") {
        html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
        html_ui <- paste0(html_ui, textInput("k", "Number of variables, including criterion:", values$k))
        html_ui <- paste0(html_ui, textInput("r", "R squared:", values$r))
        html_ui <- paste0(html_ui, textInput("confidence", "Confidence level:", values$confidence))
      } else if (values$calculation == "ci2") {
        html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
        html_ui <- paste0(html_ui, textInput("k", "Number of variables, including criterion:", values$k))
        html_ui <- paste0(html_ui, textInput("r", "R squared:", values$r))
        html_ui <- paste0(html_ui, textInput("confidence", "Confidence level:", values$confidence))
      } else if (values$calculation == "pa") {
        html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
        html_ui <- paste0(html_ui, textInput("k", "Number of variables, including criterion:", values$k))
        html_ui <- paste0(html_ui, textInput("rho", "Rho squared:", values$rho))
        html_ui <- paste0(html_ui, textInput("alpha", "Alpha:", values$alpha))
      } else if (values$calculation == "ssc") {
        html_ui <- paste0(html_ui, textInput("k", "Number of variables, including criterion:", values$k))
        html_ui <- paste0(html_ui, textInput("rho", "Rho squared:", values$rho))
        html_ui <- paste0(html_ui, textInput("alpha", "Alpha:", values$alpha))
        html_ui <- paste0(html_ui, textInput("power", "Power desired:", values$power))
      } else if (values$calculation == "ppc") {
        html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
        html_ui <- paste0(html_ui, textInput("k", "Number of variables, including criterion:", values$k))
        html_ui <- paste0(html_ui, textInput("rho", "Rho squared:", values$rho))
        html_ui <- paste0(html_ui, textInput("percentage", "Percentage point desired:", values$percentage))
      } else if (values$calculation == "pic") {
        html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
        html_ui <- paste0(html_ui, textInput("k", "Number of variables, including criterion:", values$k))
        html_ui <- paste0(html_ui, textInput("r", "R squared:", values$r))
        html_ui <- paste0(html_ui, textInput("rho", "Rho squared:", values$rho))
      }
      HTML(html_ui)
    })

	# If calculation is changed, send current widget values to global, then update global calculation value to change the input widgets
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
    })
    
    observeEvent(input$runButton, {
      updateTabsetPanel(session, "inTabset", 'out')
    })

    output$r2Output <- eventReactive(input$runButton, {
      
      if (input$calculation == "ci1") {
          validate(need(input$n, ""))
          validate(need(input$k, ""))
          validate(need(input$r, ""))
          validate(need(input$confidence, ""))
          ErrorCheck <- dget("ErrorCheck.R")
          test <- ErrorCheck(input$calculation, input$n, input$k, input$r, input$confidence)
          if (is.null(test)) {
              temp1 <- '<center><b><font color="red">Error: Invalid input.</font></b></center>'
          } else if (as.numeric(input$confidence) <= .6 || as.numeric(input$confidence) >= .999) {
              temp1 <- '<center><b><font color="red">Error: Confidence Level must be between .60 and .999, inclusive.</font></b></center>'
          } else {
              ConfidenceInterval <- dget("ConfidenceInterval.R")
              temp1 <- capture.output(ConfidenceInterval(input$n, input$k, input$r, input$confidence))
          }

      } else if (input$calculation == "ci2") {
        validate(need(input$n, ""))
        validate(need(input$k, ""))
        validate(need(input$r, ""))
        validate(need(input$confidence, ""))
        ErrorCheck <- dget("ErrorCheck.R")
        test <- ErrorCheck(input$calculation, input$n, input$k, input$r, input$confidence)
        if (is.null(test)) {
          temp1 <- '<center><b><font color="red">Error: Invalid input.</font></b></center>'
        }else if (as.numeric(input$confidence) <= .6 || as.numeric(input$confidence) >= .999) {
          temp1 <- '<center><b><font color="red">Error: Confidence Level must be between .60 and .999, inclusive.</font></b></center>'
        } else {
          ConfidenceInterval2 <- dget("ConfidenceInterval2.R")
          temp1 <- capture.output(ConfidenceInterval2(input$n, input$k, input$r, input$confidence))
        }
        
      } else if (input$calculation == "pa") {
          validate(need(input$n, ""))
          validate(need(input$k, ""))
          validate(need(input$rho, ""))
          validate(need(input$alpha, ""))
          ErrorCheck <- dget("ErrorCheck.R")
          test <- ErrorCheck(input$calculation, input$n, input$k, input$rho, input$alpha)
          if (is.null(test)) {
              temp1 <- '<center><b><font color="red">Error: Invalid input.</font></b></center>'
          } else {
              Power <- dget("Power.R")
              temp1 <- capture.output(Power(input$n, input$k, input$rho, input$alpha))
          }

      } else if (input$calculation == "ssc") {
          validate(need(input$k, ""))
          validate(need(input$rho, ""))
          validate(need(input$alpha, ""))
          validate(need(input$power, ""))
          ErrorCheck <- dget("ErrorCheck.R")
          test <- ErrorCheck(input$calculation, input$k, input$rho, input$alpha, input$power)
          if (is.null(test)) {
              temp1 <- '<center><b><font color="red">Error: Invalid input.</font></b></center>'
          } else {
              SampleSize <- dget("SampleSize.R")
              temp1 <- capture.output(SampleSize(input$k, input$rho, input$alpha, input$power))
          }
      } else if (input$calculation == "ppc") {
        validate(need(input$n, ""))
        validate(need(input$k, ""))
        validate(need(input$rho, ""))
        validate(need(input$percentage, ""))
        #PercentagePoint <- dget("PercentagePoint.R")
        #capture.output(PercentagePoint(input$n, input$k, input$rho, input$percentage))
      } else {
        validate(need(input$n, ""))
        validate(need(input$k, ""))
        validate(need(input$r, ""))
        validate(need(input$rho, ""))
        #ProbabilityIntegral <- dget("ProbabilityIntegral.R")
        #capture.output(ProbabilityIntegral(input$n, input$k, input$r, input$rho))
      }
      if (temp1 != values$output[[length(values$output)]]) {
        values$output[[length(values$output)+1]] <- temp1
        if (length(values$output) > 19) {
          first <- length(values$output)-19
        } else {
          first <- 1
        }
        last <- length(values$output)
        capture.output(cat(paste(unlist(rev(values$output[first:last])), collapse='<br>')))
      } else {        
        if (length(values$output) > 19) {
          first <- length(values$output)-19
        } else {
          first <- 1
        }
        last <- length(values$output)
        capture.output(cat(paste(unlist(rev(values$output[first:last])), collapse='<br>')))
      }

  })

})
