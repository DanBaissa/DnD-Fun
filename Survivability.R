# Code for survivability tab
survivabilityTab <- tabPanel("Survivability", 
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
