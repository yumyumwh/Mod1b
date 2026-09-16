library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(colorspace)


#
# A <-> B


#parameters

para_a = 0.5 #how much influence has species a on species b
para_b = 0.5 #how much influence has species b on species a
r_a_range = seq(3,4,0.1) #growth rate of species a
r_b_range = seq(3,4,0.1) #growth rate of species b
r_b = 2 #growth rate of species b

b_range <- seq(0,1,0.01) 
n_steps <- 500      # Total time steps
n_keep <- 100       # How many steps to keep for plotting (after transients)
a_0 <- 0.1       # Initial population density for a
start_range <- seq(0.1, 0.9, 0.1) #different starting conditions
b_0 <- 0.5       # Initial population density for a
#equations
#a[n+1] = r_a * a[n] * (1 - a[n] - para_b * b[n]) 
#b[n+1] = r_b * b[n] * (1 - b[n] - para_a * a[n])


#How do the equilibrium change with different growth rates if A and B change over time


for (n in 1:length(start_range)){
  
  a_0 <- start_range[n]

  all_solutions_a <- data.frame(matrix(nrow = length(r_a_range), ncol = length(r_b_range)))
  names(all_solutions_a) <- as.character(r_a_range) # j
  rownames(all_solutions_a) <- as.character(r_b_range) # k
  all_solutions_b <- all_solutions_a
  
  j = 0
  for (r_a in r_a_range){
    j = j+1
    k = 0
    for (r_b in r_b_range){
      
      k  = k +1
      a_sol <- c()
      b_sol <- c()
      a <- a_0
      b <- b_0
      for (i in 1:n_steps){
        a <- r_a * a * (1 - a - para_b * b) 
        b <- r_b * b * (1 - b - para_a * a)
        if (i > (n_steps - n_keep)) {
          b_sol <- c(b_sol, b)
          a_sol <- c(a_sol, a)
          
        }
      }
      
      a_sol <- round(a_sol, 3)
      b_sol <- round(b_sol, 3)
      n_a <- length(unique(a_sol))
      n_b <- length((b_sol))
      all_solutions_a[k, j] <- n_a
      all_solutions_b[k,j] <- n_b
    }
    
  }
  
  #plot for Species A
  
  all_solutions_a["r_b"] <- r_b_range
  to_plot <- all_solutions_a %>%
    pivot_longer(-r_b, names_to = "names", values_to = "Periodicity")
  
  
  print(ggplot(data = to_plot, aes(x = names, y = r_b, fill = factor(Periodicity))) +
    geom_tile(color = "white") +
    labs(fill = "Periodicity") +
    xlab("Growth Rate for Species A") +
    ylab("Growth Rate for Speices B") +
    ggtitle(sprintf("Periodicity in Species A (A_0 = %s, B_0 = %s)" ,a_0, b_0)))
  
  #plot for Species B
  
  all_solutions_b["r_b"] <- r_b_range
  to_plot <- all_solutions_b %>%
    pivot_longer(-r_b, names_to = "names", values_to = "Periodicity")
  
  
  print(ggplot(data = to_plot, aes(x = names, y = r_b, fill = factor(Periodicity))) +
          geom_tile(color = "white") +
          labs(fill = "Periodicity") +
          xlab("Growth Rate for Species A") +
          ylab("Growth Rate for Speices B") +
          ggtitle(sprintf("Periodicity in Species B (A_0 = %s, B_0 = %s)" ,a_0, b_0)))
}
####


#How does the equilibrium of population A change with different population densities of B if population density of B is kept constant over time

par(mfrow = c(length(r_a_range)/4, 4)) 

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
  
  
  

  plot(b_plot, a_sol, pch = ".", col = "black", cex = 5,
      xlab = "Population density (b)", ylab = "Population density (a)",
       main = sprintf("r_a = %.1f",r_a),
  )
  
  all_solutions <- c(all_solutions, solutions)
}


#Which periodicity do the reuslts have?

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



ggplot(data = to_plot, aes(x = names, y = b, fill = factor(Periodicity))) +
  geom_tile(color = "white") +
  scale_fill_discrete_sequential(palette = "Reds", nmax = length(unique(to_plot$Periodicity))) +
  #scale_fill_manual(values = c("white", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
   #                            "#FFFF33", "#A65628", "#F781BF", "#999999", "#E6AB02",
    #                           "#D53E4F", "#5E4FA2", "#F46D43", "#E6F598", "#3288BD",
     #                          "#66C2A5", "black"), name = "Periodicities") +
  xlab("Growth Rate for Species A") +
  ylab("Poulation Density of B") +
  ggtitle("Periodicity with changing growth rate")

#What we can look at:#What we can look at:filled.contour()
#Changing initial population density of a
#Changing growth rate of a and b together
#Changing the para_a and para_b values
#Plotting r_a and r_b -> heatmap