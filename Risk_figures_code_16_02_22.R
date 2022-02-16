library(tidyverse)
f_star = function(x){return((x-2)^2*(x-1)*x*(x+1))}
set.seed(1)
x = runif(450, -1, 2)

observations = f_star(x) + rnorm(450, 0, 1)

sample = data_frame('x' = x, 'y' = observations)
true_values = data.frame('x' = x, 'y' = f_star(x))

ggplot(sample, aes(x, y, colour = "Sample")) + 
  geom_point(alpha = 0.5, shape = 20) + 
  geom_line(data = true_values, aes(colour = "f*(x)")) + 
  scale_colour_manual("", 
                      breaks = c("Sample", "f*(x)"),
                      values = c("black", "blue")) + 
  theme_bw() +
  labs(title = "The sample and the true function") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "top")

sum((sample$y - f_star(sample$x))^2) / length(sample$x)

train = sample[1:40, ]
test = sample[41:450, ]

# Getting empirical risks given a regression 
get_errors <- function(reg){
  pred_test <- predict(reg, test[,1])
  
  train_error <- sum((reg$residuals)^2) / 40
  test_error <- sum((test[,2] - pred_test)^2) / 410
  
  return(c(train_error, test_error))
}

# These will store our outcomes
training_outcomes = c()
testing_outcomes = c()

# Regression with just an intercept
reg0 <- lm(y ~ (1), data = train)
training_outcomes[1] = get_errors(reg0)[1]
testing_outcomes[1] = get_errors(reg0)[2]

# Polynomial regressions of increasing complexity
for (i in 1:10){
  reg <- lm(y ~ poly(x, i), data = train)
  training_outcomes[i+1] = get_errors(reg)[1]
  testing_outcomes[i+1] = get_errors(reg)[2]
}

complexity <- seq(0,10)

outcomes = data.frame(cbind(complexity, training_outcomes, testing_outcomes))

# Plot the results
ggplot(outcomes, aes(complexity, training_outcomes, colour = "Training")) + 
  geom_line() +
  geom_line(aes(complexity, testing_outcomes, colour = "Test")) + 
  theme_bw() +
  scale_x_continuous(labels = as.character(complexity), breaks = complexity) +
  labs(title = "Prediction error against model complexity",
       x = "Complexity", y = "Prediction Error")+
  theme(plot.title = element_text(hjust=0.5), legend.position = "top") +
  geom_text(x = 1.5, y = 2, label = "Underfit", inherit.aes = FALSE) +
  geom_text(x = 9, y = 2, label = "Overfit", inherit.aes = FALSE) +
  geom_text(x = 5, y = 1.5, label = "Optimal", inherit.aes = FALSE, colour = "red") +
  geom_vline(xintercept = 5, linetype = "dashed", colour = "red", size = 0.25) +
  scale_colour_manual("", 
                      breaks = c("Training", "Test"),
                      values = c("blue", "yellow")) 

best_reg_points <- data.frame('x' = sample$x, 
                              'y' = lm(y ~ poly(x, 5), data = sample)$fitted.values)

ggplot(train, aes(x, y, colour = "Training points")) + geom_point(alpha = 0.5) + 
  geom_point(data = test, aes(x, y,  colour = "Test points"), alpha = 0.5) +
  geom_line(data = best_reg_points, aes(x, y, colour = "Best model")) +
  geom_line(data = true_values, aes(x, y, colour = "f*(x)")) + 
  theme_bw() +
  scale_colour_manual("", 
                      breaks = c("Training points", "Test points", "Best model", "f*(x)"),
                      values = c("blue", "yellow", "red", "black")) +
  labs(title = "The best performing model") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "top")

training_n = c()
testing_n = c()

for (n in 10:50){
  train <- sample[1:n,]
  test <- na.omit(sample[n+1:450,])
  
  reg <- lm(y ~ poly(x, 5), data = train)
  pred_test <- predict(reg, test[,1])
  
  training_n[n-9] = sum((reg$residuals)^2) / n
  testing_n[n-9] = sum((test[,2] - pred_test)^2) / (450 - n)
}

obs_seq = seq(10, 50)

final_df = data.frame(cbind(obs_seq, training_n, testing_n))

ggplot(final_df, aes(obs_seq, training_n, colour = "Training")) + geom_line() +
  geom_line(aes(obs_seq, testing_n, colour = "Testing")) + theme_bw() +
  scale_colour_manual("", 
                      breaks = c("Training", "Testing"),
                      values = c("blue", "yellow")) +
  labs(title = "Prediction error against amount of training data",
       x = "Training data size (n)", y = "prediction error") +
  geom_text(x = 18, y = 1.5, label = "Big drop", inherit.aes = FALSE, colour = "red") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "top")