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
                  &emsp;&nbsp;<a href="http://members.psyc.sfu.ca/labs/mml/research"><font color="white">Distribution Tests</font></a>
              </b>
          </div>
          <br>'),

    ## CSS styling
    tags$head(
             tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Patua+One');
                              body { min-width: 450px };
                              th { color: #808080 }"))),
    
    ## No header panel title since it's in the header bar
    headerPanel('', windowTitle = 'MML-R2'),

    sidebarLayout(
        sidebarPanel(

            ## Calculation selector
            radioButtons("calculation",
                         "Calculation to run:",
                         choiceNames = list(HTML("Confidence interval on population R<sup>2</sup> (fixed regressor)"),
                                            HTML("Confidence interval on population R<sup>2</sup> (random regressor)"),
                                            HTML("Power to reject population R<sup>2</sup> = 0"),
                                            HTML("Sample size to reject population R<sup>2</sup> = 0"),
                                            "Standardized regression coefficients from sample data",
                                            HTML("R<sup>2</sup> from sample data")),
                         choiceValues = list("fixedci", "randomci", "power", "samplesize", "beta", "r2")),
            HTML("<hr>"),

            ## File upload
            conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'beta'",
                             fileInput("datafile", "Data:")),

            ## Calculation-specific input boxes
            uiOutput("R2Input"), ## Inputs for the selected calculation
            HTML("<hr>"),

            ## Run button
            actionButton("runButton", "Run"),
            HTML("<br><br>"),

            ## Help text
            conditionalPanel(condition = "input.calculation == 'r2' | input.calculation == 'beta'",
                             helpText("Note: Your data can either be raw data (row = observation, column = variable),
                                       a correlation matrix, or a covariance matrix. It should be in headerless, .csv format."))),

        mainPanel(

            tabsetPanel(

                id = "inTabset",
                                        #tabPanel(value = "about", "About", includeHTML("./documentation/about.html")),
                tabPanel(value = "output", "Output", htmlOutput("R2Output")),
                tabPanel(value = "about", "About", includeHTML("./documentation/about.html"))
            )
        )),



  ## Footer bar with links to lab webpage and sfu.ca
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
