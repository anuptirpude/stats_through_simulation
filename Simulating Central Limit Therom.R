##############################################################################################
###############  Simulating Central Limit Theorem  ###########################################
##############################################################################################

# import require libraries
library(ggplot2)
library(gifski)
library(gganimate)
library(transformr)

# set seed for similar results
set.seed(30)

# exponential random sample of size one million 
# we can take any distribution other than exponential for the check
# we will consider this as our population 
exp_data <- data.frame(col1 = rexp(1000000, 0.4))

# plotting the population distribution
ggplot(data = exp_data, aes(x = col1)) + 
  geom_density(aes(y=..count..*0.7),color="#000000", fill = "#F85700", alpha = 0.5) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8", alpha = 0.7) + 
  labs(title = "Population Distribution (Exponential)", 
       subtitle = "Taken Over 1 million Observations", size = 5) +
  theme(plot.title = element_text(size = 30),
           plot.subtitle = element_text(size = 20),
        axis.title = element_blank(),
        axis.text =element_blank(),
        axis.ticks =element_blank())


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
# ggplot(mean_collector, aes(x= mean_col)) + 
#   geom_histogram(colour = "white", fill = "blue") + 
#   geom_density(aes(y=..density..*60, colour = 'Empirical'))

ggplot(data = mean_collector, aes(x = mean_col)) + 
  geom_density(aes(y=..count..*0.07),color="#000000", fill = "#F85700", alpha = 0.5) +
  geom_histogram(color = "#000000", fill = "#0099F8", alpha = 0.7) + 
  labs(title = "Population Distribution (Exponential)", subtitle = "Over 1 million Observations", size = 5) +
  theme(plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 20))
# we can see the distribution seem more or less normal

# to see how the collection of means from exponential distibution 
# convered to normal distribution, lets animate the entire generation
mean_collector <- data.frame(mean_col = NA)
append_collector <- data.frame(mean_col = NA, seq = NA)
for(i in 1:1000){
  randum_numer <- sample(x = 1:1000000, 100, replace = FALSE)
  sampled_data <- exp_data[randum_numer,]
  sampled_mean <- mean(sampled_data)
  
  mean_collector <- rbind(mean_collector, sampled_mean)
  mean_collector2 <- mean_collector
  mean_collector2$seq <- i
  append_collector <- rbind(append_collector, mean_collector2)
}

append_collector <- append_collector[!is.na(append_collector$mean_col),]
## below code will create an animation
## & will take 4-5 mins to do the computation 

# g <- ggplot(append_collector, aes(x= mean_col)) + 
#       geom_histogram(colour = "white", fill = "blue") +
#       theme_classic() +
#       transition_states(seq) +
#       enter_fade() +
#       exit_shrink() +
#       ease_aes('sine-in-out')
# 
# animate(g, renderer = gifski_renderer())


# plotting the sample mean's distribution
g <- ggplot(data = append_collector, aes(x = mean_col)) + 
  # geom_density(aes(y=..count..),color="#000000", fill = "#F85700", alpha = 0.5) +
  geom_histogram(color = "#000000", fill = "#0099F8", alpha = 0.7, bins = 30) + 
  labs(title = "Distribution of Sample Mean's", 
       subtitle = "Thousand Samples\nEach Sample with Hundred Observations", size = 5) +
  theme(plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 20),
        axis.title = element_blank(),
        axis.text =element_blank(),
        axis.ticks =element_blank()) +
  transition_states(seq) +
  enter_fade() +
  # enter_drift(x_mod = -1) +
  # exit_shrink() +
  # enter_drift(x_mod = 5) +
  ease_aes('sine-in-out')
  
animate(g, renderer = gifski_renderer() +
          exit_fade(), width = 700, height = 900)

## via animation its been clear as the collection of sample mean increases
## the distribution started converging towards normal distibution 
