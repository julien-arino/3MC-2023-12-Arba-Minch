# Install required packages if not installed
# install.packages(c("shiny", "deSolve"))

# Load required libraries
library(shiny)
library(deSolve)

# Define the SIR model function
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

# Define the shiny app
ui <- fluidPage(
  titlePanel("SIR Epidemic Model Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("beta", "Transmission Rate (beta)", min = 0, max = 1, value = 0.3),
      sliderInput("gamma", "Recovery Rate (gamma)", min = 0, max = 1, value = 0.1),
      sliderInput("initial_infected", "Initial Infected", min = 1, max = 100, value = 10),
      sliderInput("population", "Total Population", min = 100, max = 10000, value = 1000)
    ),
    mainPanel(
      plotOutput("sir_plot"),
      plotOutput("ode_plot")
    )
  )
)

server <- function(input, output) {
  # Reactive function to simulate the SIR model
  sir_simulation <- reactive({
    initial_state <- c(S = input$population - input$initial_infected,
                       I = input$initial_infected,
                       R = 0)
    
    parameters <- c(beta = input$beta, gamma = input$gamma)
    
    ode_result <- ode(y = initial_state, times = seq(0, 200, by = 1), func = sir_model, parms = parameters)
    
    return(ode_result)
  })
  
  # Plot the SIR simulation
  output$sir_plot <- renderPlot({
    plot(sir_simulation()$time, sir_simulation()$y[, "I"], type = "l", col = "red",
         xlab = "Time", ylab = "Infected", main = "SIR Model Simulation")
  })
  
  # Plot the corresponding ODE
  output$ode_plot <- renderPlot({
    ode_result <- sir_simulation()
    lines(ode_result$time, ode_result$y[, "I"], col = "red", lty = 2)
  })
}

shinyApp(ui, server)
