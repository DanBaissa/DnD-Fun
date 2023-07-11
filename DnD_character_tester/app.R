library(shiny)
library(ggplot2)

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

simulate_damage <- function(to_hit, target_ac, disadvantage, damage, attacks_per_round, target_hp) {
  # Initialize total damage dealt
  total_damage <- 0
  rounds_survived <- 0
  
  # Loop until target is defeated
  while(target_hp > 0) {
    # Loop for each attack per round
    for(j in 1:attacks_per_round) {
      # Simulate an attack
      attack_result <- simulate_attack(to_hit, target_ac, disadvantage)
      
      # If the attack hit, add damage to total damage
      if (attack_result > target_ac) {
        total_damage <- total_damage + damage
        target_hp <- target_hp - damage  # Decrease target_hp with each successful hit
      }
      
      # If target is defeated, break out of the loop
      if(target_hp <= 0) {
        break
      }
    }
    # Increment round counter
    rounds_survived <- rounds_survived + 1
  }
  
  # Return Target Rounds Survived
  return(rounds_survived)
}



# Function to generate survivability plot
create_survivability_plot <- function(results_df, median_rounds) {
  ggplot(results_df, aes(x=RoundsSurvived)) +
    geom_histogram(binwidth=1, color= "#5c1623", fill="#808080") +
    theme_minimal() +
    labs(
      title="Distribution of Rounds Survived",
      subtitle=paste("Median number of rounds the character survived:", round(median_rounds , 2)),
      x="Rounds Survived", 
      y="Count"
    )
}

# Function to generate damage output plot
create_damage_plot <- function(results_df, median_damage) {
  ggplot(results_df, aes(x=TotalDamage)) +
    geom_histogram(binwidth=1, color="#5c1623", fill="#808080") +
    theme_minimal() +
    labs(
      title="Distribution of Rounds to defeat Target",
      subtitle=paste("Median number of rounds the target survived:", round(median_damage , 2)),
      x="Rounds", 
      y="Count"
    )
}

# Define your Shiny UI
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
  navbarPage(title = "D&D Character Tester",
             
             # The home tab
             tabPanel("Home", 
                      h1("Welcome to the D&D Character Tester App"),
                      p("This shiny app is designed to help Dungeons & Dragons (D&D) players test the survivability and damage output of their characters. Through a series of simulations based on the combat mechanics of D&D, the app provides a statistical analysis of how different characters might perform in combat scenarios."),
                      p("In the Survivability tab, you can specify your character's attributes (such as Armor Class and Hit Points), as well as the attributes of up to three different monsters. The app will then simulate numerous rounds of combat and visualize the distribution of rounds your character survives."),
                      p("In the Damage Output tab, you can specify your character's attack attributes (such as to hit bonus and damage per hit), as well as the Armor Class and Hit Points of a target. The app will then simulate a set number of rounds and visualize the distribution of damage dealt by your character."),
                      p("The app also includes an Explanation tab, which provides a detailed explanation of the math behind the simulations.")
             ),
             
             # The survivability tab
             tabPanel("Survivability",
                      fluidRow(
                        column(3, numericInput("target_ac", "Character AC", 15)),
                        column(3, numericInput("target_hp", "Character HP", 50)),
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
                      fluidRow(
                        column(3, numericInput("dmg_to_hit", "To Hit Bonus", 5)),
                        column(3, numericInput("dmg_damage", "Damage", 6)),
                        column(3, numericInput("dmg_attacks_per_round", "Attacks per Round", 2)),
                        column(3, actionButton("dmg_sim", "Simulate"))  # Add simulate button
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("dmg_target_ac", "Target AC", 15),
                          numericInput("dmg_target_hp", "Target HP", 50)
                        ),
                        mainPanel(
                          plotOutput("dmg_output")
                        )
                      )
             ),
             
             # The explanation tab
             tabPanel("Explanation",
                      h1("Explanation"),
                      p("The key mechanics of this app are attack and damage simulation, both of which are based on the combat mechanics in D&D."),
                      h3("Attack Simulation"),
                      p("Each simulated attack roll generates a random number between 1 and 20, representing a roll of a 20-sided die. If an attack is at disadvantage, the simulation generates two numbers and takes the lower one. The attack roll, plus the attacker's to hit bonus, is compared to the target's AC to determine if the attack hits."),
                      h3("Disadvantage"),
                      p("Disadvantage, a core mechanic in D&D, is represented in this app by taking the lower of two randomly generated numbers. This makes the attacker less likely to hit."),
                      h3("Battle Simulation"),
                      p("The battle simulation calculates how long the character can survive against a monster's attacks. The monster attacks a certain number of times per round, dealing a certain amount of damage with each hit. The simulation runs until the character's HP reach 0."),
                      h3("Shield Spell"),
                      p("The shield spell, when activated, adds 5 to the character's AC. The shield activates if an attack would hit the character but miss if their AC were 5 higher. The shield stays active for the remainder of the round."),
                      h3("Damage Simulation"),
                      p("The damage simulation calculates the total damage the character can deal in a set number of rounds. If an attack hits, it deals a set amount of damage to the target."),
                      h3("Histograms"),
                      p("The app presents the results of the survivability and damage simulations as histograms, showing the distribution of outcomes across multiple simulations. This allows you to see not just the average result, but the spread of possible outcomes.")
             )
             
  )
)

# Define your Shiny server
server <- function(input, output) {
  
  # Observing simulation input for Monster 1
  observeEvent(input$sim1, { 
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit1, input$target_ac, input$disadvantage1, input$damage1, input$target_hp, input$attacks_per_round1, input$shield_spells)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    output$hist_rounds1 <- renderPlot(create_survivability_plot(results_df, median_rounds))
  })
  
  # Observing simulation input for Monster 2
  observeEvent(input$sim2, { 
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit2, input$target_ac, input$disadvantage2, input$damage2, input$target_hp, input$attacks_per_round2, input$shield_spells)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    output$hist_rounds2 <- renderPlot(create_survivability_plot(results_df, median_rounds))
  })
  
  # Observing simulation input for Monster 3
  observeEvent(input$sim3, { 
    rounds_survived <- replicate(input$n_sims, isolate(simulate_battle(input$to_hit3, input$target_ac, input$disadvantage3, input$damage3, input$target_hp, input$attacks_per_round3, input$shield_spells)))
    results_df <- data.frame(RoundsSurvived = rounds_survived)
    median_rounds <- median(results_df$RoundsSurvived)
    output$hist_rounds3 <- renderPlot(create_survivability_plot(results_df, median_rounds))
  })
  
  # Observing damage simulation input
  observeEvent(input$dmg_sim, { 
    total_damage <- replicate(input$n_sims, isolate(simulate_damage(input$dmg_to_hit, input$dmg_target_ac, input$disadvantage1, input$dmg_damage, input$dmg_attacks_per_round, input$dmg_target_hp)))
    results_df <- data.frame(TotalDamage = total_damage)
    median_damage <- median(results_df$TotalDamage)
    output$dmg_output <- renderPlot(create_damage_plot(results_df, median_damage))
  })
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)