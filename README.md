# Ranking Methods
Bayesian ranking methods implementation  

# Positional Loss  
## 1. Loss on Ranks  
pros:  
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
  - assumes ranks are evenly spaced (TODO check)
### a. Square Error Loss on Ranks
file:  
### b. Absolute Error Loss on Ranks
file:  
### c. Zero One Loss on Ranks
file:  

## 2. Loss on Probability Scale  
pros:
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
### a. Square Error Loss on Probability Scale
file:  
### b. Absolute Error Loss on Probability Scale
file:  
### c. Zero One Loss on Probability Scale
file:  

## 3. Loss on Point Estimate Scale (TODO can we be this flexible?)
pros:  
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
  - ranking will depend on scale (TODO check this)
### a. Square Error Loss on Point Estimate Scale
file:  
### b. Absolute Error Loss on Probability Scale
file:  
### c. Zero One Loss on Probability Scale
file:  

## 4. Weighted Loss on Rank Scale
pros:  
  - allows users to weight ranked items or positions (or both? TODO add as 2nd step)
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
### a. Square Error Loss on Point Estimate Scale
file:   
### b. Absolute Error Loss on Probability Scale
file:  
### c. Zero One Loss on Probability Scale
file:  

# Comparison Loss
## 5. Loss on Ranks  
pros:  
  - accounts for varying levels on uncertainty in the point estimates of various ranked items
cons:  
  - assumes ranks are evenly spaced (TODO check)
### a. Square Error Loss on Ranks
file:  
### b. Absolute Error Loss on Ranks
file:  
### c. Zero One Loss on Ranks
file:  

#Loss Function-less Ranking Methods
## 6. Rank by Posterior Means (estimates)
file: posteriormeans.r
pros:  
  - simple, typically used across fields
cons:  
  - ignores varying levels on uncertainty in the point estimates of various ranked items

## 7. Henderson & Newton Threshold Functions (R Value)
see their package (maybe include here)


#Possible Additions:
- multilevel modeling from Ron's earlier student (name?) into ranking
