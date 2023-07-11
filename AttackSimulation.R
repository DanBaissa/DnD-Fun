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
