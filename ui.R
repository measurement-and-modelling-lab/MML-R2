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
                  <a href="https://shiny.rcg.sfu.ca/u/pserafin/MML-R2/"><font color="#00ca8a">MML-R2</font></a>
                  &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/MML-Multicorr/"><font color="white">MML-Multicorr</font></a>
                  &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/MML-WBCORR/"><font color="white">MML-WBCORR</font></a>
                  &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/csv-generator/"><font color="white">CSV Generator</font></a>
              </b>
          </div>
          <br>'
        ),

    ## CSS style stuff
    tags$head(
        tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Patua+One');
        
th {
   color: #808080;
}

                        "))
    ),
    
  ## No title since it's in the header bar
  headerPanel('', windowTitle = 'MML-R2'),

  sidebarLayout(
      sidebarPanel(
          uiOutput("selector"), ## Radio buttons for choosing a calculation to run
          HTML("<hr>"),
          conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'beta'",
                           fileInput("datafile", "Data:")),
          uiOutput("valueInput"), ## Inputs for the selected calculation
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
