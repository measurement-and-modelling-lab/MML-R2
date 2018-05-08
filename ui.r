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
                    "))
    ),

  headerPanel('MML-R2', windowTitle = 'MML - R-Squared'),

  sidebarLayout(
  sidebarPanel(
  	selectInput("calculation", "Calculation to run:",
                 c("Confidence Interval (Fixed Regressor)" = "ci1",
                   "Confidence Interval (Random Regressor)" = "ci2",
                   "Power" = "pa",
                   "Necessary Sample Size" = "ssc",
                   "Standardized Coefficients" = "b",
                   "Squared Multiple Correlation" = "r2"
                 )
    ),
    HTML("<hr>"),
    conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'b'", fileInput("datafile", "Data:")),
    uiOutput("valueInput"),
    HTML("<hr>"),
	actionButton("runButton", "Run"),
    HTML("<br><br>"),
    conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'b'", helpText("Note: data should raw (row = observation, column = variable) and in headerless, .csv format."))
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
