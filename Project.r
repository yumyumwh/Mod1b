library(ggplot2)
library(tidyr)
library(dplyr)


#here I changed something!

# A <-> B


#parameters

para_a = 0.5 #how much influence has species a on species b
para_b = 0.5 #how much influence has species b on species a
r_a_range = seq(3,4,0.1) #growth rate of species a
r_b = 2 #growth rate of species b

b_range <- seq(0,1,0.01) 
n_steps <- 500      # Total time steps
n_keep <- 100       # How many steps to keep for plotting (after transients)
a_0 <- 0.5          # Initial population density for a

#equations
#a[n+1] = r_a * a[n] * (1 - a[n] - para_b * b[n]) 
#b[n+1] = r_b * b[n] * (1 - b[n] - para_a * a[n])

par(mfrow = c(length(r_a_range)/4, 4)) 
all_solutions <- c()

for (r_a in r_a_range){
  a_sol <- c()
  b_plot <- c()
  for (b in b_range){
    a <- a_0
    for (i in 1:n_steps){
    a <- r_a * a * (1 - a - para_b * b)
    if (i > (n_steps - n_keep)) {
      b_plot <- c(b_plot, b)
      a_sol <- c(a_sol, a)
      }
    }
  }
  
  solutions <- data.frame(b_plot, a_sol)
  names(solutions) <- c("b", sprintf("%s",r_a))
  
  
  

  #plot(b_plot, a_sol, pch = ".", col = "black",
  #     xlab = "Population density (b)", ylab = "Population density (a)",
  #     main = sprintf("r_a = %.1f",r_a),
  #     ylim = c(0,1))
  
  all_solutions <- c(all_solutions, solutions)
}

all_solutions <- as.data.frame(all_solutions)
all_solutions <- all_solutions %>% select(-starts_with("b."))
n <- as.character(r_a_range)
n <- c("b", n)
names(all_solutions) <- n

periods <- c()
perioden <- data.frame(matrix(nrow = length(unique(b_plot)), ncol = length(r_a_range)))
names(perioden) <- as.character(r_a_range)


j = 0
for (bb in unique(b_plot)){
  j = j +1
  x <- all_solutions[all_solutions$b == bb,]
  periods <- c()
  for (i in 1:length(r_a_range)){
  
    period <- nrow(unique(x[i]))
    periods <- c(periods, period)
  }
  
  perioden[j,] <- periods
  
}

perioden["b"] <- unique(b_plot)

to_plot <- perioden %>%
  pivot_longer(-b, names_to = "names", values_to = "Periodicity")

ggplot(data = to_plot, aes(x = names, y = b, fill = Periodicity)) +
  geom_tile(color = "white") +
  xlab("Growth Rate for Species A") +
  ylab("Poulation Density of B") +
  title("Periodicity with changing growth rate")

#What we can look at:#What we can look at:filled.contour()
#Changing initial population density of a
#Changing growth rate of a and b together
#Changing the para_a and para_b values
#Plotting r_a and r_b -> heatmap