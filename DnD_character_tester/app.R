library(shiny)
library(tidyverse)

# Function to simulate a single attack
simulate_attack <- function(to_hit, target_ac, disadvantage = FALSE) {
  rolls <- sample(1:20, ifelse(disadvantage, 2, 1))
  roll <- ifelse(disadvantage, min(rolls), rolls)
  
  if (roll == 20 & !disadvantage) {
    return(list('result' = TRUE, 'message' = "Critical hit!"))
  }
  
  total <- roll + to_hit
  
  if(total >= target_ac) {
    return(list('result' = TRUE, 'message' = "Hit!"))
  } else {
    return(list('result' = FALSE, 'message' = "Miss!"))
  }
}

# Function to simulate a battle
simulate_battle <- function(to_hit, target_ac, disadvantage, damage, target_hp, attacks_per_round) {
  rounds_survived <- 0
  
  while(target_hp > 0) {
    for(i in 1:attacks_per_round) {
      attack_result <- simulate_attack(to_hit, target_ac, disadvantage)
      
      if (attack_result$result) {
        target_hp <- target_hp - damage
      }
      
      if(target_hp <= 0) {
        break
      }
    }
    
    rounds_survived <- rounds_survived + 1
  }
  
  return(rounds_survived)
}

# Define your Shiny UI
ui <- navbarPage("D&D Character Tester",
                 
                 # The home tab
                 tabPanel("Home", 
                          h1("D&D Character Tester"),
                          p("In this shiny app, you can test the survivability and damage output of your Dungeons & Dragons (D&D) characters.")
                 ),
                 
                 # The survivability tab
                 tabPanel("Survivability", 
                          sidebarLayout(
                            sidebarPanel(
                              wellPanel(
                                h3("Character Attributes"),
                                numericInput("target_ac", "Target AC", 15),
                                numericInput("target_hp", "Target HP", 50)
                              ),
                              wellPanel(
                                h3("Monster Attributes"),
                                numericInput("to_hit", "To Hit Bonus", 5),
                                checkboxInput("disadvantage", "Disadvantage", TRUE),
                                numericInput("damage", "Damage", 6),
                                numericInput("attacks_per_round", "Attacks per Round", 2),
                                numericInput("n_sims", "Number of Simulations", 1000),
                                actionButton("go", "Simulate")
                              )
                            ),
                            mainPanel(
                              textOutput("avg_rounds"),
                              plotOutput("hist_rounds")
                            )
                          )
                 )
                 ,
                 
                 # The damage output tab
                 tabPanel("Damage Output",
                          # You can add a similar layout to the Survivability tab, but with inputs and outputs for the damage output simulation
                 ),
                 
                 # The math behind tab
                 tabPanel("Math Behind",
                          # Add more detailed explanations and examples of the math involved in your calculations
                 )
)

# Define your Shiny server
server <- function(input, output) {
  
  observeEvent(input$go, {
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit, input$target_ac, input$disadvantage, input$damage, input$target_hp, input$attacks_per_round)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    
    output$avg_rounds <- renderText(paste("The tank survived an average of", round(mean(results_df$RoundsSurvived), 2), "rounds."))
    
    output$hist_rounds <- renderPlot({
      ggplot(results_df, aes(x=RoundsSurvived)) +
        geom_histogram(binwidth=1, color="black", fill="lightblue") +
        theme_minimal() +
        labs(
          title="Distribution of Rounds Survived",
          subtitle=paste("Median number of rounds the character survived:", round(median_rounds , 2)),
          x="Rounds Survived", 
          y="Count"
        )
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
