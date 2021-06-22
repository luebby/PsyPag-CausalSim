### Preliminaries
# install.packages("mosaic")
library(mosaic)

### First Simulation
set.seed(1896) # Reproducibility
n <- 1000 # Sample Size
learning <- rnorm(n)                    # X
knowing <- 5 * learning + rnorm(n)      # Z
understanding <- 3 * knowing + rnorm(n) # Y

Sim_chain <- data.frame(learning, knowing, understanding)

res1_S1 <- lm(understanding ~ 
                learning,
              data = Sim_chain)
coef(res1_S1)

res2_S1 <- lm(understanding ~ 
                learning + knowing,
              data = Sim_chain)
coef(res2_S1)

### Second Simulation
set.seed(1896) # Reproducibility
n <- 1000 # Sample Size
intelligence <- rnorm(n, mean = 100, sd = 15)  # Z
learning.time <- 200 - intelligence + rnorm(n) # X

### Excercise
# test.score <-
  
# Putting it all together
Sim_fork <- data.frame(learning.time, intelligence, test.score)

res1_S2 <- lm(test.score ~ 
                learning.time,
              data = Sim_fork)
coef(res1_S2)

res2_S2 <- lm(test.score ~ 
                learning.time + intelligence,
              data = Sim_fork)
coef(res2_S2)

### Third Simulation
set.seed(1896) # Reproducibility
n <- 1000 # Sample Size
kind <- rnorm(n)                                    # X
look <- rnorm(n)                                    # Y
dating <- ((kind > 1) | (look > 1))                 # ~Z
luck <- rbinom(n, size = 1, prob = 0.05)            # U_Z
dating <- (1 - luck) * dating + luck * (1 - dating) # Z

Sim_collider <- data.frame(look, 
                           dating, 
                           kind) %>%
  mutate(dating = ifelse(dating, 
                         "date", 
                         "no date"))

res1_S3 <- lm(kind ~ 
                look,
              data = Sim_collider)
coef(res1_S3)

res2_S3 <- lm(kind ~ 
                look + dating,
              data = Sim_collider)
coef(res2_S3)

gf_point(kind ~ look, 
         color = ~ dating,
         data = Sim_collider) %>% 
  gf_lm()

### Excercise: Simulation Gender Pay Gap


