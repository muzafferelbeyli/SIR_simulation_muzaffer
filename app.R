
library(shiny)
library(deSolve)
library(tidyverse)

# equations
sir_model <- function(time,state,parameter){
  with(
    as.list(c(state, parameter)), {
      dS = b - d*S - a*c*S*I + (sigma*R)
      dI = a*c*S*I - (delta*I) - (rho*I)
      dR = (rho*I) - (sigma*R) - d*R
      
      return(list(c(dS, dI, dR)))
    })
}

initial <- c(S = 999, I = 1, R = 0)
time <- seq(0, 50, by = 1)

# ui
ui <- fluidPage(
    withMathJax(),
    titlePanel("SIR Model Simulation"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("a", "Tranmission rate (a):",
                    min = 0.01 ,max = 1,value = 0.05, step = 0.005),
        sliderInput("rho", "Recovery rate (ρ):",
                    min = 0.01 ,max = 1,value = 0.1, step = 0.005),
        sliderInput("delta", "Death rate by the disease: (δ)",
                    min = 0 ,max = 0.2,value = 0.02, step = 0.005),
        
        hr(),
        helpText("Play with parameters to observe dynamics of disease spread"),
        hr(),
        p("Simulation made by Muzaffer A. Elbeyli"),
        hr(),
        p("Model used in the simulation is from the book:", br(), 
          "A Biologist’s Guide to Mathematical Modeling in Ecology and Evolution", br(),
          "by Otto & Day")
      ),
      
      
      
      mainPanel(
        plotOutput("sirplot"),
        hr(),
        
        p(strong("SIR Model Equations:")),
        helpText("$$\\frac{dS}{dt} = b - dS - acSI + \\sigma R$$"),
        helpText("$$\\frac{dI}{dt} = acSI - \\delta I - \\rho I$$"),
        helpText("$$\\frac{dR}{dt} = \\rho I - \\sigma R - dR$$")
                )
    )
)


# server
server <- function(input, output) {
  
  output$sirplot <- renderPlot({
    pars <- c(b = 10, d = 0.01, a = input$a, 
              c = 0.1, sigma = 0.05, 
              delta = input$delta, rho = input$rho)
    
    # differential equations
    modelode <- ode(y = initial,times = time,func = sir_model, parms = pars)
    model_asdf <- as.data.frame(modelode)
    
    # plotting
    forplot <- model_asdf %>%
      pivot_longer( cols = c(S, I, R), 
                    names_to = "Population", 
                    values_to = "Number"
      )
    
    ggplot(data = forplot,
           aes(x = time, y = Number,
               color = Population)) +
      
      geom_line(linewidth = 1.2) +
      
      scale_color_manual(values = c("S" = "blue", "I" = "red3", "R" = "green4")) +
      labs(
        title = "SIR Model Dynamics",
        x = "Time (in Days)",
        y = "Number of Individuals",
        color = "Category") +
      
      theme_minimal()
  })

}

# run
shinyApp(ui = ui, server = server)
