rm(list=ls())
install.packages("bvartools")
library("bvartools") # Load the package, which contains the data
data("e6") # Load data
plot(e6) # Plot data

library(vars) # Load package

# Estimate VAR
var_aic <- VAR(e6, type = "const", lag.max = 8, ic = "AIC", season = 4)

# Lag order suggested by AIC
var_aic$p

library(urca) # Load package

# Estimate
vec <- ca.jo(e6, ecdet = "none", type = "trace",
             K = 4, spec = "transitory", season = 4)

summary(vec)

# Beta
round(vec@V, 2)

# Alpha
round(vec@W, 2)

round(vec@GAMMA, 2)

# Transform VEC to VAR with r = 1
var <- vec2var(vec, r = 1)

# Obtain IRF
ir <- irf(var, n.ahead = 20, impulse = "R", response = "Dp",
          ortho = FALSE, runs = 500)

# Plot
plot(ir)
