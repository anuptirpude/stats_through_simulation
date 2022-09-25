
##############################################################################################
###############  Simulating Central Limit Theorem  ###########################################
##############################################################################################

# import require libraries
library(ggplot2)
library(gifski)
library(gganimate)

# set seed for similar results
set.seed(30)

# exponential random sample of size one million 
# we will consider this as our population 
# we can take any distribution other than exponential for the check
exp_data <- data.frame(col1 = rexp(1000000, 0.4))

# plotting the population distribution
ggplot(data = exp_data, aes(x = col1)) + 
  geom_histogram() + 
  geom_density(aes(y=..count..),color="red") +
  theme_bw()


# Create loop to draw sample of 100 observation withour replacement
# from population having exponential distribution defined at the begining 
# this entire process is repeated over thousand times using FOR loop
# & sample mean is collected over each drawn sample

mean_collector <- data.frame(mean_col = NA) # blank data frame

for(i in 1:1000){
  # generating random sample of size 100 between 1 to 1000000 
  randum_numer <- sample(x = 1:1000000, 100, replace = FALSE)
  # subsetting population over random sample 
  sampled_data <- exp_data[randum_numer,]
  # taking mean over drawn sample
  sampled_mean <- mean(sampled_data)
  # appending mean of each drawn sample
  mean_collector <- rbind(mean_collector, sampled_mean)
}

# plotting histogram on collected mean's to understand the distribution
ggplot(mean_collector, aes(x= mean_col)) + 
  geom_histogram(colour = "white", fill = "blue") + 
  geom_density(aes(y=..density..*60, colour = 'Empirical'))
# we can see the distribution seem more or less normal

# to see how the collection of means from exponential distibution 
# convered to normal distribution, lets animate the entire generation
mean_collector <- data.frame(mean_col = NA)
append_collector <- data.frame(mean_col = NA, seq = NA)
for(i in 1:1000){
  randum_numer <- sample(x = 1:1000000, 1000, replace = FALSE)
  sampled_data <- exp_data[randum_numer,]
  sampled_mean <- mean(sampled_data)
  
  mean_collector <- rbind(mean_collector, sampled_mean)
  mean_collector2 <- mean_collector
  mean_collector2$seq <- i
  append_collector <- rbind(append_collector, mean_collector2)
}

## below code will create an animation
## & will take 4-5 mins to do the computation 
g <- ggplot(append_collector, aes(x= mean_col)) + 
  geom_histogram(colour = "white", fill = "blue") +
  theme_classic() +
  transition_states(seq) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

animate(g, renderer = gifski_renderer())

## via animation its been clear as the collection of sample mean increases
## the distribution started converging towards normal distibution 
