library(ggplot2)

#here I changed something!

# A <-> B


#parameters

para_a = 1
para_b = 1
r_a = 1
r_b = 1

#equations

#a[n+1] = r_a * a[n] * (1 - a[n] - para_b * b[n])
#b[n+1] = r_b * b[n] * (1 - b[n] - para_a * a[n])

#consider only equilibrium state

a = 1 - para_b - 1/r_a

