shinyUI(fluidPage(theme = "simplex.css",

    ## Header bar with links to other apps
    HTML('<br>
          <link rel="stylesheet" type="text/css" href="index.css">
          <style>
              html { overflow-y: scroll; }
          </style>
          <div class="bar">
              <b class="title">Measurement and Modelling Lab &nbsp; - &nbsp; Tools</b><br class="rwd-break">
              <b class="link">
                  <a href="https://shiny.rcg.sfu.ca/u/pserafin/rsquared/"><font color="#00ca8a">MML-R2</font></a>
                  &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/wbcorr/"><font color="white">MML-WBCORR</font></a>
                  &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/csvgenerator/"><font color="white">CSV Generator</font></a>
              </b>
          </div>
          <br>'
        ),

    ## CSS style stuff
    tags$head(
        tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Patua+One');"))
    ),
    
  ## No title since it's in the header bar
  headerPanel('', windowTitle = 'MML-R2'),

  sidebarLayout(
      sidebarPanel(
          selectInput("calculation", "Calculation to run:",
                      c("Confidence interval on R2 (fixed regressor)" = "fixedci",
                        "Confidence interval on R2 (random regressor)" = "randomci",
                        "Power to reject H0: R2 = 0" = "power",
                        "Sample size to reject HO: R2 = 0" = "samplesize",
                        "Estimate standardized regression coefficients" = "beta",
                        "Calculate R2 from raw data" = "r2")),
          HTML("<hr>"),
          conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'beta'", fileInput("datafile", "Data:")),
          uiOutput("valueInput"),
          HTML("<hr>"),
          actionButton("runButton", "Run"),
          HTML("<br><br>"),
          conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'beta'",
                           helpText("Note: Your data can either be raw data (row = observation, column = variable),
                                     a correlation matrix, or a covariance matrix. It should be in headerless, .csv format."))),
  
  mainPanel(
      htmlOutput("finaloutput"))
  ),

  ## Footer bar
  HTML('
    <br>
    <link rel="stylesheet" type="text/css" href="index.css">
    <div class="bar2">
     <b class="bottom">
     <font color="#717171">Provided by the</font>
     <a href="http://members.psyc.sfu.ca/labs/mml"><font color=white>Measurement and Modelling Lab</font></a>
     <font color="#717171"> at</font>
     <a href="https://www.sfu.ca/"><font color=white> SFU</font></a>
     </b>
     </div><br>')
)
)
