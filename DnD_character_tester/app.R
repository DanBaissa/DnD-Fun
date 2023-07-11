library(shiny)
library(tidyverse)
library(shinythemes)

# Function to simulate a single attack
simulate_attack <- function(to_hit, target_ac, disadvantage = FALSE) {
  # Draw two random numbers from 1 to 20 if disadvantage is TRUE, one otherwise
  rolls <- sample(1:20, ifelse(disadvantage, 2, 1))
  
  # If disadvantage is TRUE, take the lower roll, otherwise take the roll
  roll <- ifelse(disadvantage, min(rolls), rolls)
  
  # Check for natural 20 (if not at disadvantage)
  if (roll == 20 & !disadvantage) {
    return(list('result' = TRUE, 'message' = "Critical hit!"))
  }
  
  # Add the "to hit" bonus
  total <- roll + to_hit
  
  # Return total if the total is greater than the target's AC, FALSE otherwise
  return(total)
}

to_hit <- 5
target_ac <- 15
disadvantage <- TRUE
damage <- 6
target_hp <- 50
attacks_per_round <- 2
shield_spells <- 0


attack_result <- 15+6

if (attack_result > target_ac & attack_result < 6 + target_ac){
  print("test")
}

# Function to simulate a battle
simulate_battle <- function(to_hit, target_ac, disadvantage, damage, target_hp, attacks_per_round, shield_spells) {
  # Initialize round counter
  rounds_survived <- 0
  
  # Loop until target is defeated
  while(target_hp > 0) {
    # Initialize shield status for the round
    shield_active <- FALSE
    
    # Loop for each attack per round
    for(i in 1:attacks_per_round) {
      
      # Simulate an attack
      attack_result <- simulate_attack(to_hit, ifelse(shield_active, target_ac + 5, target_ac), disadvantage)
      
      # Did the attack hit?
      if (attack_result > target_ac & attack_result < 6 + target_ac){
        
        # Check if shield spell can be used
        if(!shield_active && shield_spells > 0 ) {
          shield_active <- TRUE
          shield_spells <- shield_spells - 1
        }
      }
      
      # If the attack hit, subtract damage from target's HP
      if (attack_result > ifelse(shield_active, target_ac + 5, target_ac)) {
        target_hp <- target_hp - damage
      }  # This was the missing closing brace
      
      # If target is defeated, break out of the loop
      if(target_hp <= 0) {
        break
      }
    }
    
    # Increment round counter
    rounds_survived <- rounds_survived + 1
  }
  
  # Return number of rounds survived
  return(rounds_survived)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #5c1623; border-color: #5c1623; }
      .navbar-default .navbar-brand { color: #FFFFFF; }
      .navbar-default .navbar-brand:hover { color: #FFFFFF; }
      .navbar-default .navbar-nav > li > a { color: #FFFFFF; }
      .navbar-default .navbar-nav > li > a:hover { color: #FFFFFF; }
      .well { background-color: #808080; border-color: #5c1623; color: #FFFFFF; }
    "))
  ),
  navbarPage(  # Using "cerulean" as base theme
    title = "D&D Character Tester",
    
    # The home tab
    tabPanel("Home", 
             h1("D&D Character Tester"),
             p("In this shiny app, you can test the survivability and damage output of your Dungeons & Dragons (D&D) characters.")
    ),
    
    # The survivability tab
    tabPanel("Survivability",
             fluidRow(
               column(3, numericInput("target_ac", "Target AC", 15)),
               column(3, numericInput("target_hp", "Target HP", 50)),
               column(3, numericInput("shield_spells", "Shield Spells", 2)),
               column(3, sliderInput("n_sims", "Number of Simulations", min = 100, max = 5000, value = 1000))  
             ),
             fluidRow(
               column(4, 
                      wellPanel(
                        h3("Monster 1 Attributes"),
                        numericInput("to_hit1", "To Hit Bonus", 5),
                        checkboxInput("disadvantage1", "Disadvantage", TRUE),
                        numericInput("damage1", "Damage", 6),
                        numericInput("attacks_per_round1", "Attacks per Round", 2),
                        actionButton("sim1", "Simulate")  # Add simulate button
                      )
               ),
               column(8, plotOutput("hist_rounds1"))
             ),
             fluidRow(
               column(4, 
                      wellPanel(
                        h3("Monster 2 Attributes"),
                        numericInput("to_hit2", "To Hit Bonus", 5),
                        checkboxInput("disadvantage2", "Disadvantage", TRUE),
                        numericInput("damage2", "Damage", 6),
                        numericInput("attacks_per_round2", "Attacks per Round", 2),
                        actionButton("sim2", "Simulate")  # Add simulate button
                      )
               ),
               column(8, plotOutput("hist_rounds2"))
             ),
             fluidRow(
               column(4, 
                      wellPanel(
                        h3("Monster 3 Attributes"),
                        numericInput("to_hit3", "To Hit Bonus", 5),
                        checkboxInput("disadvantage3", "Disadvantage", TRUE),
                        numericInput("damage3", "Damage", 6),
                        numericInput("attacks_per_round3", "Attacks per Round", 2),
                        actionButton("sim3", "Simulate")  # Add simulate button
                      )
               ),
               column(8, plotOutput("hist_rounds3"))
             )
    ),
    
    # The damage output tab
    tabPanel("Damage Output",
             # You can add a similar layout to the Survivability tab, but with inputs and outputs for the damage output simulation
    ),
    
    # The math behind tab
    tabPanel("Math Behind",
             # Add more detailed explanations and examples of the math involved in your calculations
    )
  )
)
# Define your Shiny server
server <- function(input, output) {
  
  observeEvent(input$sim1, {  # Replaced observe() with observeEvent() so it only runs when the button is pressed
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit1, input$target_ac, input$disadvantage1, input$damage1, input$target_hp, input$attacks_per_round1, input$shield_spells)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    output$hist_rounds1 <- renderPlot(create_plot(results_df, median_rounds))
  })
  
  observeEvent(input$sim2, {  # Replaced observe() with observeEvent() so it only runs when the button is pressed
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit2, input$target_ac, input$disadvantage2, input$damage2, input$target_hp, input$attacks_per_round2, input$shield_spells)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    output$hist_rounds2 <- renderPlot(create_plot(results_df, median_rounds))
  })
  
  observeEvent(input$sim3, {  # Replaced observe() with observeEvent() so it only runs when the button is pressed
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit3, input$target_ac, input$disadvantage3, input$damage3, input$target_hp, input$attacks_per_round3, input$shield_spells)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    output$hist_rounds3 <- renderPlot(create_plot(results_df, median_rounds))
  })
  
  create_plot <- function(results_df, median_rounds) {
    ggplot(results_df, aes(x=RoundsSurvived)) +
      geom_histogram(binwidth=1, color="black", fill="lightblue") +
      theme_minimal() +
      labs(
        title="Distribution of Rounds Survived",
        subtitle=paste("Median number of rounds the character survived:", round(median_rounds , 2)),
        x="Rounds Survived", 
        y="Count"
      )
  }
}

# Run the Shiny app
shinyApp(ui = ui, server = server)