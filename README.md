# Ranking Methods
Bayesian ranking methods implementation  

## 1. Rank by Posterior Mean   
file: posteriormeans.r
pros:  
  - simple, typically used across fields
cons:  
  - ignores varying levels on uncertainty in the point estimates of various ranked items

# Positional Loss  
## 2. Loss on Ranks  
pros:  
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
  - assumes ranks are evenly spaced (TODO check)
###a. Square Error Loss on Ranks
file:  
###b. Absolute Error Loss on Ranks
file:  
###c. Zero One Loss on Ranks
file:  

##3. Loss on Probability Scale  
pros:
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
###a. Square Error Loss on Probability Scale
file:  
###b. Absolute Error Loss on Probability Scale
file:  
###c. Zero One Loss on Probability Scale
file:  

##4. Loss on Point Estimate Scale (TODO can we be this flexible?)
pros:  
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
  - ranking will depend on scale (TODO check this)
###a. Square Error Loss on Point Estimate Scale
file:  
###b. Absolute Error Loss on Probability Scale
file:  
###c. Zero One Loss on Probability Scale
file:  

##5. Weighted Loss on Rank Scale
pros:  
  - allows users to weight ranked items or positions (or both? TODO add as 2nd step)
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
###a. Square Error Loss on Point Estimate Scale
file:   
###b. Absolute Error Loss on Probability Scale
file:  
###c. Zero One Loss on Probability Scale
file:  

#Comparison Loss
