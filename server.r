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
	values$predictors <- ""
	values$criterion <- ""
	values$output <-list('')

    output$valueInput <- renderUI({

		# Create input widgets for global calculation value, with default values from global
      	html_ui <- ""
      	if (values$calculation == "ci1") {
        	html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                          	  textInput("k", "Number of variables, including criterion:", values$k),
                          	  textInput("r", "R squared:", values$r),
                          	  textInput("confidence", "Confidence level:", values$confidence, placeholder = 0.95))
        } else if (values$calculation == "ci2") {
        	html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                          	  textInput("k", "Number of variables, including criterion:", values$k),
                          	  textInput("r", "R squared:", values$r),
                          	  textInput("confidence", "Confidence level:", values$confidence, placeholder = 0.95))
		} else if (values$calculation == "pa") {
			html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                          	  textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("rho", "Rho squared:", values$rho),
                              textInput("alpha", "Alpha:", values$alpha, placeholder = 0.05))
		} else if (values$calculation == "ssc") {
        	html_ui <- paste0(textInput("k", "Number of variables, including criterion:", values$k),
                          	  textInput("rho", "Rho squared:", values$rho),
                              textInput("alpha", "Alpha:", values$alpha, placeholder = 0.05),
                              textInput("power", "Power desired:", values$power, placeholder = 0.8))
      } else if (values$calculation == "ppc") {
        	html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                              textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("rho", "Rho squared:", values$rho),
                              textInput("percentage", "Percentage point desired:", values$percentage))
      } else if (values$calculation == "pic") {
        	html_ui <- paste0(textInput("n", "Number of observations:", values$n),
                              textInput("k", "Number of variables, including criterion:", values$k),
                              textInput("r", "R squared:", values$r),
                              textInput("rho", "Rho squared:", values$rho))
      } else if (values$calculation == "b") {
        
			validate(need(input$datafile, ""))

			# Check that R can read the data file as a .csv
			result = tryCatch({
				read.csv(file=input$datafile[[4]], head=FALSE, sep=",")
			}, warning = function(w) {
				'problem'
			}, error = function(e) {
				'problem'
			})
			if ('problem' %in% result) {
				return()
			} else { # If so import it as a matrix
				data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
			}

			variables <- ncol(data)

			string_vector <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9", "10" = "10", "11" = "11", "12" = "12", "13" = "13", "14" = "14", "15" = "15", "16" = "16")
			html_ui <- ''
			if (nrow(data) == ncol(data)) {
				html_ui <- paste0(html_ui, textInput("n", "Number of observations:", values$n))
			}
            html_ui <- paste0(html_ui,
							  textInput("confidence", "Confidence level:", values$confidence, placeholder = 0.95),
							  radioButtons("familywise", "Familywise error control:", choices = c("None"="uncorrected", "Bonferroni"="bonferroni", "Dunn-Sidak"="sidak", "Stepdown Bonferroni"="stepdown_bonferroni", "Stepdown Dunn-Sidak"="stepdown_sidak")),
							  div(style="display: inline-block;vertical-align:top; width: 100px;", checkboxGroupInput("predictors", "Predictors:", string_vector[1:variables], values$predictors)),
							  div(style="display: inline-block;vertical-align:top; width: 50px;", radioButtons("criterion", "Criterion:", string_vector[1:variables], values$criterion)))

      } else if (values$calculation == "r2") {
        
			validate(need(input$datafile, ""))

			# Check that R can read the data file as a .csv
			result = tryCatch({
				read.csv(file=input$datafile[[4]], head=FALSE, sep=",")
			}, warning = function(w) {
				'problem'
			}, error = function(e) {
				'problem'
			})
			if ('problem' %in% result) {
				return()
			} else { # If so import it as a matrix
				data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
			}

			variables <- ncol(data)

			string_vector <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9", "10" = "10", "11" = "11", "12" = "12", "13" = "13", "14" = "14", "15" = "15", "16" = "16")


			html_ui <- paste0(html_ui, div(style="display: inline-block;vertical-align:top; width: 100px;", checkboxGroupInput("predictors", "Predictors:", string_vector[1:variables], values$predictors)))
			html_ui <- paste0(html_ui, div(style="display: inline-block;vertical-align:top; width: 50px;", radioButtons("criterion", "Criterion:", string_vector[1:variables], values$criterion)))

      }
      HTML(html_ui)
    })
    
	# If calculation is changed, send current widget values to global, then update global calculation value to whatever calculator it's changing to
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

    output$r2Output <- eventReactive(input$runButton, {
      
      if (input$calculation == "ci1") {
          validate(need(input$n, ""))
          validate(need(input$k, ""))
          validate(need(input$r, ""))
          validate(need(input$confidence, ""))
          ErrorCheck <- dget("ErrorCheck.R")
          test <- ErrorCheck(input$calculation, input$n, input$k, input$r, input$confidence)
          if (is.null(test)) {
              return(capture.output(cat('<center><b><font color="red">Error: Invalid input.</font></b></center>')))
          } else if (as.numeric(input$confidence) <= .6 || as.numeric(input$confidence) >= .999) {
              return(capture.output(cat('<center><b><font color="red">Error: Confidence Level must be between .60 and .999, inclusive.</font></b></center>')))
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
            return(capture.output(cat('<center><b><font color="red">Error: Invalid input.</font></b></center>')))
        }else if (as.numeric(input$confidence) <= .6 || as.numeric(input$confidence) >= .999) {
            return(capture.output(cat('<center><b><font color="red">Error: Confidence Level must be between .60 and .999, inclusive.</font></b></center>')))
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
          	  return(capture.output(cat('<center><b><font color="red">Error: Invalid input.</font></b></center>')))
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
          	  return(capture.output(cat('<center><b><font color="red">Error: Invalid input.</font></b></center>')))
          } else {
              SampleSize <- dget("SampleSize.R")
              temp1 <- capture.output(SampleSize(input$k, input$rho, input$alpha, input$power))
          }
      } else if (input$calculation == "b") {
		  validate(need(input$datafile, ""))
		  validate(need(input$predictors, ""))
		  validate(need(input$criterion, ""))
		  validate(need(input$confidence, ""))
		  validate(need(input$familywise, ""))
		  
		  # Check that R can read the data file as a .csv
		  result = tryCatch({
			read.csv(file=input$datafile[[4]], head=FALSE, sep=",")
		  }, warning = function(w) {
			'problem'
		  }, error = function(e) {
			'problem'
		  }, finally = {
		  })
		  if ('problem' %in% result) {
	      	return(capture.output(cat('<center><b><font color="red">Error: Invalid input.</font></b></center>')))
		  } else { # If so import it as a matrix
			data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
		  }
		  
		  if (ncol(data) > 16) {
	      	return(capture.output(cat('<center><b><font color="red">Error: You cannot have more than 16 variables.</font></b></center>')))
		  }

		  if (as.character(input$criterion) %in% input$predictors) {
	      	return(capture.output(cat('<center><b><font color="red">Error: A variable cannot be both a predictor and the criterion.</font></b></center>')))
		  }
		  
		  if (length(input$predictors) < 2) {
	      	return(capture.output(cat('<center><b><font color="red">Error: You must have at least two predictors.</font></b></center>')))
		  }

		  if (NA %in% as.numeric(data)) {
	      	return(capture.output(cat('<center><b><font color="red">Error: Your data has missing or non-numeric elements.</font></b></center>')))
		  }

		  if (ncol(data) != nrow(data)) {
		    Nobs <- nrow(data)
		  	data <- cov(data)
		  } else {
			Nobs <- as.numeric(input$n)
		  }
		  
		  predictors <- as.numeric(input$predictors)
		  criterion <- as.numeric(input$criterion)

		  familywise <- input$familywise

		  cov.x <- data[predictors,predictors]
		  cov.xy <- data[predictors,criterion]
		  var.y <- data[criterion,criterion]

		  alpha <- 1 - as.numeric(input$confidence)

		  stdb <- dget("stdb.R")
		  temp1 <- capture.output(stdb(X=NULL, y=NULL, cov.x=cov.x, cov.xy=cov.xy, var.y=var.y, criterion=criterion, predictors=predictors, alpha=alpha, Nobs=Nobs, familywise=familywise))


      } else if (input$calculation == "r2") {
		  validate(need(input$datafile, ""))
		  validate(need(input$predictors, ""))
		  validate(need(input$criterion, ""))

		  # Check that R can read the data file as a .csv
		  result = tryCatch({
			read.csv(file=input$datafile[[4]], head=FALSE, sep=",")
		  }, warning = function(w) {
			'problem'
		  }, error = function(e) {
			'problem'
		  }, finally = {
		  })
		  if ('problem' %in% result) {
	      	return(capture.output(cat('<center><b><font color="red">Error: You must have at least two predictors.</font></b></center>')))
		  } else { # If so import it as a matrix
			data <- as.matrix(read.csv(file=input$datafile[[4]], head=FALSE, sep=","))
		  }
		  
		  if (ncol(data) > 16) {
	      	return(capture.output(cat('<center><b><font color="red">Error: You cannot have more than 16 variables.</font></b></center>')))
		  }

		  if (NA %in% as.numeric(data)) {
	      	return(capture.output(cat('<center><b><font color="red">Error: Your data has missing or non-numeric elements.</font></b></center>')))
		  }
		  
		  if (as.character(input$criterion) %in% input$predictors) {
	      	return(capture.output(cat('<center><b><font color="red">Error: A variable cannot be both a predictor and the criterion.</font></b></center>')))
		  }
		  
		  if (length(input$predictors) < 2) {
	      	return(capture.output(cat('<center><b><font color="red">Error: You must have at least two predictors.</font></b></center>')))
		  }
		  
		  rxx <- dget("rxx.R")

		  predictors <- as.numeric(input$predictors)
		  criterion <- as.numeric(input$criterion)

		  temp1 <- capture.output(rxx(data, predictors, criterion))

      }




	  # only show last 20 output
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
