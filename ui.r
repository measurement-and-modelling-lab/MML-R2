library(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",

  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Patua+One');
                    h1 {
                    font-family: 'Patua One';
                    font-weight: bold;
                    line-height: 1.1;
                    color: #333;
                    }
                    td {
                      white-space: nowrap;
                      width: 1px;
                      padding-left: 8px;
                      padding-right: 8px;
                      padding-top: 0px;
                      padding-bottom: 0px;
                      color: #717171;
                    }
					sup {
						vertical-align: 75%;
						font-size: smaller;
					}
                    "))
    ),

  headerPanel('MML-R2', windowTitle = 'MML - R-Squared'),

  sidebarLayout(
  sidebarPanel(
  	selectInput("calculation", "Calculation to run:",
                 c("Fixed Regressor Confidence Interval" = "ci1",
                   "Random Regressor Confidence Interval" = "ci2",
                   "Power Analysis" = "pa",
                   "Necessary Sample Size" = "ssc",
                   "Standardized Coefficient Confidence Interval" = "b",
                   "Squared Multiple Correlation" = "r2"
                 )
    ),
    HTML("<hr>"),
    conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'b'", fileInput("datafile", "Correlation file:")),
    conditionalPanel(condition = "input.calculation == 'rxx'", textInput("Nobs", "Sample size:")),
    uiOutput("valueInput"),
    HTML("<hr>"),
	actionButton("runButton", "Run")
  ),
  
  mainPanel(
	htmlOutput("r2Output")
  )),

  HTML('<br>'),
  HTML('<link rel="stylesheet" type="text/css" href="index.css">
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
