#UI development for Hyperspectral Image Processing
#naufalrauf@yahoo.co.id
#Developed by Rauf 13 June 2017
#Updated by Rauf 13 June 2018

library (shiny)

shinyUI(fluidPage(
  titlePanel("Hyperspectral Image Processing"),
  
  sidebarLayout(
      sidebarPanel(
        h4("Control Panel"),
        h5("Create Hyperspectral Image Database"),
        actionButton("dbcreate", "Create"),
        h5("Select a Hyperspectral Image Database (.RData)"),
        actionButton("dbload", "Browse"),
        h6(textOutput("text3")),
        selectInput("var", 
          label = " Choose a program function to execute",
          choices = list("Extracting Images", "Plot Spectral Signature"),
          selected = "Extracting Images"),
        
        conditionalPanel(
          condition = "input.var == 'Extracting Images'",
          numericInput("wvl1", "Wavelength:", value = 550, min = 400, max = 1000),
          h5("Generate Image"),
          actionButton("browse", "Click"),
          br(),
          br(),
          actionButton("ndvi.button", "NDVI"),
          br(),
          #actionButton("ccim", "Image Correction"),
          h6(textOutput("text4"))
          
        ),  
        conditionalPanel(
          condition = "input.var == 'Plot Spectral Signature'",
          h5("Plot Spectral Signature"),
          h6("Manual Input Coordinates"),
          numericInput("plotx1", "X1: ", value = 0),
          numericInput("plotx2", "X2: ", value = 0),
          numericInput("ploty1", "Y1: ", value = 0),
          numericInput("ploty2", "Y2: ", value = 0),
          actionButton("plotss", "Plot"))
          
        
       
        ),
    mainPanel(
    tabsetPanel(id = "inTabset", type = "tabs",
      tabPanel(title = "Image", value = "panel1", 
        br(),
        #textOutput("text1"), 
        textOutput("text2"),
        br(),
        imageOutput("image1",
          click = "plot_click",
          brush = "plot_brush"
                   ),
        verbatimTextOutput("info"),
        br(),
        imageOutput("image2",
           click = "plot_click2",
           brush = "plot_brush2"),
        verbatimTextOutput("infoc")
      ),
      
      tabPanel(title = "Plot", value = "panel2", plotOutput("ssplot"),
          verbatimTextOutput("hsindex"),
          verbatimTextOutput("info2")
            
            ),
      tabPanel("About", 
        br(),
        "Laboratory of Agricultural Machinery and Production System Design", 
        br(),
        "Kyushu University 2017")
      )
   )
  )
))