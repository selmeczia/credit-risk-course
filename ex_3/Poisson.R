# Excercise 5
library(data.table)
library(ggplot2)
library(ggthemes)
library(vcd)
library(MASS)

# Config
lambda <- 0.1
dt <- 1
interval_bottom <- 0
interval_top <- 100

# possion process
MC_size <- 100

MC_sets <- rep(paste0("set", 1:MC_size))
  
N <- data.table(t = rep(0,100 * MC_size), set = "set1", element = 1, jump = F, jump_dist = 0)
set_number <- 0
j <- 1

for (set in MC_sets){
  for (i in (1:100)){
    row_num <- set_number * 100 + i
    N[row_num]$element <- i
    
    if(i == 1){
      N[row_num]$t <- 0
      N[row_num]$set <- set
      
    } else {
      
    if(runif(1) > 1-(lambda*dt)){
        N[row_num]$t <- N[row_num - 1]$t + 1
        N[row_num]$jump <- T
        N[row_num]$jump_dist <- j
        j <- 0
        
      } else {
        
        N[row_num]$t <- N[row_num - 1]$t
        j <- j+1
        
      }
    N[row_num]$set <- set
    }
  }
  j <- 1
  set_number <- set_number + 1
  print(paste0("currently at: ", set))
}




result <- poisson.test(sum(N$jump), dim(N)[1], r = lambda)
result
result$p.value

annotation <- c(paste0("The p-value for lambda = ", lambda, ","),
                paste0("is: ", round(result$p.value, 3)))

# P-value is larger than 0.05 -> elfogadjuk a nullhipotézist, tehát a lambda = minta

ggplot(N[element == 100], aes(x=t))+
  geom_histogram(aes(y = ..density..),binwidth = 1)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(t)), color = "blue")+
  annotate("text", x = 15, y = c(0.15, 0.14), label = annotation)+
  theme_economist()


#####

ex <- N[jump_dist != 0]

fit <- fitdistr(ex$jump_dist, "exponential")$estimate

ks.test(N[jump_dist != 0]$jump_dist, "pexp", fit$estimate)

ggplot(ex, aes(x = jump_dist))+
  geom_histogram(aes(y = ..density..), binwidth = 2)+
#  stat_function(fun = dexp, color = "red", 
#                arg = list(rate = fit))+
  theme_economist()

hist(ex$jump_dist)
curve(dexp(x, rate = fit), add = T, col = "red", lwd  = 2)


#####




