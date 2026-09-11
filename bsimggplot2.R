use("ggplot2")

set.seed(2)
## Define the Boltzmann Distribution
boltzmann <- function(n) {
  1 / ( 2 ^ (n + 1) )
}

## Create the original grid
mat <- matrix(0, nrow = 20, ncol = 20)
mat[1, 1] <- 400
mat

## Create a function to generate a random row/col number for replacement
rand.ind <- function() {
  sample(1:20, size=2, replace=TRUE)
}

## Define a function to repeat the process for a variable number of times
num_epochs <- 1*10^5
for (epoch in 1:num_epochs) {
  repeat {
    index0 <- rand.ind()
    if (mat[index0[1], index0[2]] != 0) {
      break
    }
  }
  index1 <- rand.ind()
  mat[index1[1], index1[2]] <- mat[index1[1], index1[2]] + 1
  mat[index0[1], index0[2]] <- mat[index0[1], index0[2]] - 1
  
  if ((epoch / num_epochs * 100) %% 5 == 0) {
    cat("Epochs [", epoch / num_epochs * 100, "% loaded ]\n")
  }
}
mat

#### Processing Data
mat.vals <- c(mat)

## Quick hist of mat.vals for verification
hist(mat.vals, 
     breaks=seq(min(mat.vals) - 0.5, max(mat.vals) + 0.5, by=1), 
     probability=TRUE,
     col="ivory3")

## Add the theoretical Boltzmann curve
x.vals <- 0:max(mat.vals)

lines(
  x.vals,
  boltzmann(x.vals),
  col='black',
  lwd=2
)
