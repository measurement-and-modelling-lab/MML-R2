require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",
                  
                  
                  HTML('<br>
  
    <link rel="stylesheet" type="text/css" href="index.css">
  <style>
    html {
       overflow-y: scroll;
       }
       </style>
    <title>Analytics^2 - About</title>
         <div class="bar">
    <b class="title">Measurement and Modelling Lab &nbsp; - &nbsp; Tools</b><br class="rwd-break"><b class="link">
    <a href="https://shiny.rcg.sfu.ca/u/pserafin/rsquared/"><font color="#00ca8a">MML-R2</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/wbcorr/"><font color="white">MML-WBCORR</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/csvgenerator/"><font color="white">CSV Generator</font></a>





        </b>
        </div>
         
         
         
         '),
                  
                  HTML("<br>"),

  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Patua+One');
                    h1 {
                    font-family: 'Patua One';
                    font-weight: bold;
                    line-height: 1.1;
                    color: #333;
                    }
                    body { min-width: 450px; }
                    td {
                      white-space: nowrap;
                      width: 1px;
                      padding-left: 8px;
                      padding-right: 8px;
                      padding-top: 0px;
                      padding-bottom: 0px;
                      color: #717171;
                    }
                    "))
    ),
    
  headerPanel('', windowTitle = 'MML-R2'),

  sidebarLayout(
  sidebarPanel(
  	selectInput("calculation", "Calculation to run:",
                 c("Confidence interval on R2 (fixed regressor)" = "ci1",
                   "Confidence interval on R2 (random regressor)" = "ci2",
                   "Power to reject H0: R2 = 0" = "pa",
                   "Sample size to reject HO: R2 = 0" = "ssc",
                   "Estimate standardized regression coefficients" = "b",
                   "Calculate R2 from raw data" = "r2"
                 )
    ),
    HTML("<hr>"),
    conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'b'", fileInput("datafile", "Data:")),
    uiOutput("valueInput"),
    HTML("<hr>"),
	actionButton("runButton", "Run"),
    HTML("<br><br>"),
    conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'b'", helpText("Note: Your data can either be raw data (row = observation, column = variable), a correlation matrix, or a covariance matrix. It should be in headerless, .csv format."))
  ),
  
  mainPanel(
	htmlOutput("r2Output")
  )),

  HTML('<br>'),
  HTML('
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
