
library(shiny)
home <- c("Away", "Home")

#fluidPage(
  navbarPage("Analysis",
         tabPanel("Regression",
  # Application title
  h1("NBA Shots Made/Missed", align = "center"),
  

  sidebarPanel(
h5("Percentage"),
verbatimTextOutput('out1'),
selectInput('in1', 'Home or Away', c(home), selectize=FALSE),
sliderInput("defenderdistance", "Closest Defender Distance", min = 0, max = 10, value = 5, step = .5),
sliderInput("distancefromhoop", "The distance from the Hoop", min = 0, max = 30, value = 15, step = 1),
sliderInput("touchtime", "Touch Time", min = 0, max = 24, value = 12, step = 1),
sliderInput("shotclock", "Shot Clock", min = 0, max = 24, value = 12, step = 1)


#hr(),
#fluidRow(column(3, verbatimTextOutput("value")))
),
  mainPanel(
    tabsetPanel(
  tabPanel("Regression",
  h3("Logistic Regression", align = "center"),
  verbatimTextOutput("log_fxn")),
  tabPanel("Chart",
  h3("Shots Made", align = "center"),
  plotOutput('barPlot')
  )
    )
    )
),
tabPanel("Plots",
         mainPanel(
           tabsetPanel(
             tabPanel("Shot Clock",
           plotOutput('plotshotclk')),
             tabPanel("Touch Time",
           plotOutput('plottoucht')),
              tabPanel("Home or Away",
           plotOutput('plotloc')),
              tabPanel("Distance from Defender",
           plotOutput('plotclosestdefdist')),
              tabPanel("Distance from Hoop",
           plotOutput('plothoopdist')),
              tabPanel("Best Shooters",
           plotOutput('plotoplayer'))
           )
         )
),
tabPanel("Data",
         h1("NBA Dataset", align = "center"),
         mainPanel(
        dataTableOutput("table")
         )
        )
)
