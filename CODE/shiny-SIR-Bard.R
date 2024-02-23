library(shiny)
library(ggplot2)
library(deSolve)

ui <- fluidPage(
  titlePanel("SIR Epidemic Model"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("population", "Population Size:", min = 100, max = 10000, value = 1000),
      sliderInput("beta", "Transmission Rate (beta):", min = 0, max = 1, value = 0.2),
      sliderInput("gamma", "Recovery Rate (gamma):", min = 0, max = 1, value = 0.1),
      sliderInput("initial_infected", "Initial Infected:", min = 1, max = 50, value = 10),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("CTMC Simulation", plotOutput("ctmc_plot")),
        tabPanel("ODE Plot", plotOutput("ode_plot"))
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$simulate, {
    simulate_ctmc()
    plot_ode()
  })
  
  simulate_ctmc <- reactive({
    # Define transition rates
    rates <- c(input$beta * (input$population - X) * X / input$population,
               input$gamma * X)
    
    # Simulate CTMC using Gillespie algorithm
    ctmc_sim <- gillespie(init = input$initial_infected, rates = rates, tmax = 100)
    
    ctmc_data <- as.data.frame(ctmc_sim)
    ctmc_data$time <- ctmc_sim$time
    
    ctmc_data
  })
  
  output$ctmc_plot <- renderPlot({
    ctmc_data <- simulate_ctmc()
    
    ggplot(ctmc_data, aes(x = time, y = X)) +
      geom_line() +
      labs(title = "CTMC Simulation of SIR Model", x = "Time", y = "Number of Infected")
  })
  
  plot_ode <- reactive({
    # Define ODE model
    ode_model <- function(time, state, params) {
      with(as.list(c(state, params)), {
        dS <- -beta * S * I / population
        dI <- beta * S * I / population - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
      })
    }
    
    # Solve ODE
    ode_solution <- ode(y = c(S = input$population - input$initial_infected, I = input$initial_infected, R = 0),
                        times = seq(0, 100),
                        func = ode_model,
                        parms = c(beta = input$beta, gamma = input$gamma, population = input$population))
    
    ode_data <- as.data.frame(ode_solution)
    
    ode_data
  })
  
  output$ode_plot <- renderPlot({
    ode_data <- plot_ode()
    
    ggplot(ode_data, aes(x = time)) +
      geom_line(aes(y = S, color = "Susceptible")) +
      geom_line(aes(y = I, color = "Infected")) +
      geom_line(aes(y = R, color = "Recovered")) +
      labs(title = "ODE Plot of SIR Model", x = "Time", y = "Number of Individuals")
  })
}

shinyApp(ui, server)
