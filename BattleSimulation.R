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
