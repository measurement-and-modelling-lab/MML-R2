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
                    "))
    ),
                  
                  
                  
  headerPanel('MML-R2', windowTitle = 'MML - R-Squared'),

  HTML("<br><br>"),
 
  sidebarLayout(
  sidebarPanel(
    radioButtons("calculation", "Calculation to run:",
                 c("Confidence Interval (Fixed Regressor)" = "ci1",
                   "Confidence Interval (Random Regressor)" = "ci2",
                   "Power Analysis" = "pa",
                   "Necessary Sample Size" = "ssc",
                   "Rxx Matrix" = "rxx"
                   #"Percentage Point" = "ppc",
                   #"Probability Integral" = "pic"
                 )
    ),
    uiOutput("valueInput"),
    actionButton("runButton", "Run")
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
