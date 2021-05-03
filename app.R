# Minimal Difference - RShiny Web-App

## Packages
library(shiny)
library(shinythemes)

## User Interface

ui <- fluidPage(
  h2("Adler Medical Data Science - Minimal Difference (MD)"),
  # theme for style of the web app
  theme = shinytheme("cosmo"),
  # navigation bar layout
  sidebarLayout(
    sidebarPanel(width = 3,
                 h3("Data input:"),
                 tags$br(),
                 textInput("Name", "Name of parameter:", "Fasting plasma glucose"),
                 textInput("Unit", "Unit of parameter:", "mmol/l"),
                 numericInput("MeanLow", "Mean of low quality control:", 6.6),
                 numericInput("VKLow", "Coefficient of variation of low quality control:", 1.6),
                 numericInput("MeanHigh", "Mean of high quality control:", 15.3),
                 numericInput("VKHigh", "Coefficient of variation of high quality control:", 1.7),
                 tags$hr(),
                 tags$br(),
                 tags$strong("Copyright by Jakob Adler")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          h4("Introduction"),
          tags$br(),
          "This is a little web app for estimation of the Minimal Difference (MD) from quality control measurements. The
          theoretical basis of these estimation is published in this article:",
          tags$br(),
          tags$br(),
          uiOutput("Link1"),
          tags$br(),
          "On the right site you can enter the name and the measurement unit of the parameter.",
          tags$br(),
          tags$br(),
          "To calculate the Minimal Difference at a given cut-off, you must enter the mean and the
          coefficient of variation of a low and a high quality control.",
          tags$br(),
          tags$br(),
          "Using the", tags$strong("Estimation of MD"), "tab, this app will show you the estimated MDs at the quality control level
          and will perform a", tags$strong("linear regression model."),
          tags$br(),
          tags$br(),
          "This model ist used to estimate the MD at a given cut-off.", 
          tags$hr(),
          "For critics and questions, please contact", tags$strong("Jakob Adler:"),
          tags$br(),
          tags$br(),
          uiOutput("Link2"),
          tags$br(),
          tags$strong("Copyright by Jakob Adler")
        ),
        tabPanel(
          h4("Estimation of MD"),
          tags$br(),
          fluidRow(
            h4("Data set:"),
            column(4, verbatimTextOutput("Dataset"))
          ),
          tags$hr(),
          h4("Estimation of Minimal Difference at the Cut-Off:"),
          inputPanel(
            uiOutput("xaxismin1"),
            uiOutput("xaxismax1"),
            uiOutput("yaxismax1"),
            numericInput("Cut", "Cut-Off:", 7.0)
          ),
          fluidRow(
            column(8, verbatimTextOutput("MinDiff")),
            column(8, verbatimTextOutput("LinReg"))
          ),
          fluidRow(
            column(6, plotOutput("RegPlot"))
          ),
          tags$hr()
        )
      )
    )
  )
)

## Server

server <- function(input, output){
  # Link1
  output$Link1 <- renderUI({
    url <- a("Measurement Uncertainty Impacts Diagnosis of Diabetes Mellitus: Reliable Minimal Difference of Plasma Glucose Results", href = "https://link.springer.com/article/10.1007/s13300-019-00740-w")
    tagList(url)
  })
  # Link2
  output$Link2 <- renderUI({
    url <- a("Adler Medical Data Science on GitHub", href = "https://github.com/Bussard91")
    tagList(url)
  })
  # Data set preparation
  Data <- reactive({
    a <- input$MeanLow
    b <- input$MeanHigh
    c <- input$VKLow
    d <- input$VKHigh
    e <- round(((a*c)/100)*2, 4)
    f <- round(((b*d)/100)*2, 4)
    MW <- c(a, b)
    VK <- c(c, d)
    MD <- c(e, f)
    df <- cbind(MW, VK, MD)
    df <- as.data.frame(df)
    colnames(df) <- c("Mean","VK","MD")
    rownames(df) <- c("Low quality control", "High quality control")
    df
  })
  # Data set output
  output$Dataset <- renderPrint({
    df <- Data()
    df
  })
  # Inputs for plot x-axis
  output$xaxismin1 <- renderUI({
    df <- Data()
    numericInput("xaxismin2", "Minimum x-axis:", min(df$Mean))
  })
  output$xaxismax1 <- renderUI({
    df <- Data()
    numericInput("xaxismax2", "Maximum x-axis:", max(df$Mean))
  })
  output$yaxismax1 <- renderUI({
    df <- Data()
    numericInput("yaxismax2", "Maximum y-axis:", max(df$MD))
  })
  # MD estimation
  output$MinDiff <- renderPrint({
    df <- Data()
    Model <- lm(df$MD ~ df$Mean)
    MiniDiff <- ((Model$coefficients[2])*input$Cut) + (Model$coefficients[1])
    MinDiffEst <- paste("The Minimal Difference at the Cut-Off of", input$Cut, input$Unit, "is", round(MiniDiff,2), input$Unit, ".")
    MinDiffEst
  })
  # Linear regression formula
  output$LinReg <- renderPrint({
    df <- Data()
    Model <- lm(df$MD ~ df$Mean)
    if(Model$coefficients[1] < 0){
      LinReg <- paste("The formula for the linear regression model is: MD = ", round(Model$coefficients[2],2),
                    "x Cut-Off", round(Model$coefficients[1],4), ".")
      return(LinReg)
    } else {
      LinReg <- paste("The formula for the linear regression model is: y = ", round(Model$coefficients[2],2),
                      "x +", round(Model$coefficients[1],4), ".")
      return(LinReg)
    }
  })
  # Linear regression plot
  output$RegPlot <- renderPlot({
    df <- Data()
    a <- input$Cut
    Model <- lm(df$MD ~ df$Mean)
    plot(df$Mean, df$MD, xlab = paste(input$Name, "in", input$Unit), ylab = paste("Minimal Difference in", input$Unit),
         col = "blue", cex = 2, pch = 16, xlim = c(input$xaxismin2, input$xaxismax2), ylim = c(0, input$yaxismax2),
         main = paste("Regression plot of", input$Name, "MD based on quality controls"))
    abline(Model, lwd = 2)
    points(x = input$Cut, y = Model$coefficients[2]*input$Cut+Model$coefficients[1], col = "red", pch = 16, cex = 2)
    legend(x = (input$xaxismin2 + input$xaxismax2)/1.7, y = input$yaxismax2/5, legend = c("Quality controls", "Cut-Off"),
           pch = c(16, 16), col = c("blue", "red"))
  })
}

shinyApp(ui = ui, server = server)
