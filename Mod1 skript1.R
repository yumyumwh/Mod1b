library(ggplot2)
library(tidyr)
library(dplyr)


#here I changed something!

# A <-> B


#parameters

para_a = 0.5 #how much influence has species a on species b
para_b = 0.5 #how much influence has species b on species a
#growth rate of species b
n_steps <- 500      # Total time steps
n_keep <- 100       # How many steps to keep for plotting (after transients)
a_0 <- 0.5          # Initial population density for a
b_0 <- 0.5          # Initial population density for b


eps <- 1e-3     # thresholde


r_range <- seq(1, 4, 0.1)   # Growth rate, used identically for A and B

# --- single simulation for a fixed (r_a, r_b) pair ---

simulate_pair <- function(r_a, r_b) {
  a <- a_0
  b <- b_0
  
  a_hist <- numeric(n_keep)
  b_hist <- numeric(n_keep)
  keep_start <- n_steps - n_keep
  
  for (i in 1:n_steps) {
    a_new <- r_a * a * (1 - a - para_b * b)
    b_new <- r_b * b * (1 - b - para_a * a)
    
    a <- max(a_new, 0)
    b <- max(b_new, 0)
    
    if (i > keep_start) {
      idx <- i - keep_start
      a_hist[idx] <- a
      b_hist[idx] <- b
    }
  }
  
  list(a_hist = a_hist, b_hist = b_hist)
}

# --- classification ---
classify_outcome <- function(a_hist, b_hist) {
  a_extinct <- max(a_hist) < eps
  b_extinct <- max(b_hist) < eps
  
  if (a_extinct && b_extinct) {
    "Beide sterben aus"
  } else if (a_extinct) {
    "Nur B überlebt"
  } else if (b_extinct) {
    "Nur A überlebt"
  } else {
    "Koexistenz"
  }
}

# --- Parameterraster (r_a, r_b) ---
results <- expand.grid(r_a = r_range, r_b = r_range)
results$outcome <- NA_character_

for (k in seq_len(nrow(results))) {
  sim <- simulate_pair(results$r_a[k], results$r_b[k])
  results$outcome[k] <- classify_outcome(sim$a_hist, sim$b_hist)
}

results$outcome <- factor(
  results$outcome,
  levels = c("Koexistenz", "Nur A überlebt", "Nur B überlebt", "Beide sterben aus")
)

# --- Phasendiagramm ---
p <- ggplot(results, aes(x = r_a, y = r_b, fill = outcome)) +
  geom_tile() +
  scale_fill_manual(values = c(
    "coexistenz"        = "#7FFF00",
    "Only A survived"    = "#3182bd",
    "Only b survived"    = "#BF3EFF",
    "both die" = "#de2d26"
  )) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "white", alpha = 0.6) +
  labs(
    title = "Phase Diagram: Coexistence vs. Mutual Exclusion",
    subtitle = sprintf("para_a = %.2f, para_b = %.2f, a0 = b0 = %.2f", para_a, para_b, a_0),
    x = "growthrate r_a (Art A)",
    y = "growthrate r_b (Art B)",
    fill = "results"
  ) +
  theme_minimal()

print(p)

# --- Test simulation  ---
plot_trajectory <- function(r_a, r_b, steps = 100) {
  a <- a_0; b <- b_0
  a_traj <- numeric(steps); b_traj <- numeric(steps)
  for (i in 1:steps) {
    a_new <- r_a * a * (1 - a - para_b * b)
    b_new <- r_b * b * (1 - b - para_a * a)
    a <- max(a_new, 0); b <- max(b_new, 0)
    a_traj[i] <- a; b_traj[i] <- b
  }
  df <- data.frame(step = 1:steps, a = a_traj, b = b_traj) %>%
    pivot_longer(-step, names_to = "species", values_to = "density")
  ggplot(df, aes(x = step, y = density, color = species)) +
    geom_line() + theme_minimal() +
    labs(title = sprintf("r_a = %.2f, r_b = %.2f", r_a, r_b))
}
plot_trajectory(3.5, 1.5)

