source("data.R")
library("BayesPPD")

data=createdata("previousresearch_1_1")

historical_data=list(list(y0=data$historical_1_y,x0=data$historical_1_x))

nMC <- 2000
nBI <- 500


result <- glm.random.a0(
  data.type = "Normal",
  y = as.vector(data$current_y),
  x = data$current_x,
  borrow.treat = FALSE,
  historical = historical_data,
  prior.beta.var = rep(10, 4),
  prior.a0.shape1 = c(1, 1),
  prior.a0.shape2 = c(1, 1),
  nMC = nMC,
  nBI = nBI
)

summary(result)