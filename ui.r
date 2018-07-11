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
    
  headerPanel('', windowTitle = 'MML - R-Squared'),

  sidebarLayout(
  sidebarPanel(
  	selectInput("calculation", "Calculation to run:",
                 c("Confidence interval on R^2 (Fixed Regressor)" = "ci1",
                   "Confidence interval on R^2 (Random Regressor)" = "ci2",
                   "Power to reject R^2 = 0" = "pa",
                   "Sample size to reject R^2 = 0" = "ssc",
                   "Standardized regression coefficients" = "b",
                   "Squared multiple correlation" = "r2"
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

  HTML('<br>')
)
)
