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
                 c("Fixed Regressor CI" = "ci1",
                   "Random Regressor CI" = "ci2",
                   "Power Analysis" = "pa",
                   "Necessary Sample Size" = "ssc",
                   "Squared Multiple Correlation" = "rxx"
                 )
    ),
    HTML("<hr>"),
    conditionalPanel(condition = "input.calculation == 'rxx'", fileInput("datafile", "Correlation file:")),
    uiOutput("valueInput"),
    conditionalPanel(condition = "input.calculation != 'rxx'", HTML("<hr>")),
    conditionalPanel(condition = "input.calculation != 'rxx'", actionButton("runButton", "Run"))
  ),
  
  mainPanel(
    conditionalPanel(condition = "input.calculation != 'rxx'", htmlOutput("r2Output")),
    conditionalPanel(condition = "input.calculation == 'rxx'", htmlOutput("rxxoutput"))
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
