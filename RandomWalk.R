#install.packages("ggplot2")
#install.packages("gridExtra")


library(ggplot2)
library(gridExtra)

# Function to generate a random walk
random_walk <- function(n_steps, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed) # Set the random seed for reproducibility
  }
  
  df <- data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  
  df[1, ] <- c(0, 0, 1) # Initialize the starting point
  
  for (i in 2:n_steps) {
    h <- 0.25 # Step size
    angle <- runif(1, min = 0, max = 2 * pi) # Random angle in radians
    
    # Update x and y coordinates based on the random angle
    df[i, 1] <- df[i - 1, 1] + cos(angle) * h
    df[i, 2] <- df[i - 1, 2] + sin(angle) * h
    df[i, 3] <- i # Update time step
  }
  
  return(df)
}

# Generate two reproducible random walks with the same seed
data1 <- random_walk(500, seed = 42)
data2 <- random_walk(500, seed = 42)

# Create the first plot
plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  geom_path(aes(colour = time)) +
  theme_bw() +
  xlab("x-coordinate") +
  ylab("y-coordinate")

# Create the second plot
plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  geom_path(aes(colour = time)) +
  theme_bw() +
  xlab("x-coordinate") +
  ylab("y-coordinate")

# Arrange the two plots side by side
grid.arrange(plot1, plot2, ncol = 2)

