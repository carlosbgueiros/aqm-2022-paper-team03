
#multilevel_model <- m2_ord_logit_clmm_2sd


 ## Function to run simulation and compute predicted probabilities from a model in the ordinal package ####
 # Uses OBSERVED VALUE APPROACH
fun_sim_pred_prob <- function(multilevel_model = NULL, seed = 202206, nsim = 1000) {
  
set.seed(seed)
  
multilevel_model$vcov <- vcov(multilevel_model)
#lrtest(m7_ord_logit_clmm_2sd, m7_ord_logit_2sd)

# 2 next line in the code below is repeated to make hessian without country and country/region random terms. No meaningful substantive value in our analysis.
multilevel_model$vcov <- multilevel_model$vcov[-nrow(multilevel_model$vcov), -ncol(multilevel_model$vcov)] 
multilevel_model$vcov <- multilevel_model$vcov[-nrow(multilevel_model$vcov), -ncol(multilevel_model$vcov)]

S <- MASS::mvrnorm(nsim, multilevel_model$coefficients, multilevel_model$vcov)
#  multilevel_model$optRes$par is an alternative to multilevel_model$coefficients, as it list the results from the optimizer.

# 2 downward mobility values (as in data)
downward_mob_seq <- seq(min(multilevel_model$model$downward_mob), max(multilevel_model$model$downward_mob), length.out = 2)

X <- multilevel_model$model |> 
  select(-c("stmigrant", "cntry", "region")) |> 
  as.matrix()


# empty object for X times the number of scenarios (ie length of mobility)
cases <- array(NA, c(dim(X), length(downward_mob_seq)))
cases[, ,] <- X


sel1 <- which(colnames(X) == "downward_mob")

# modify the X values in mobility column, ie column 1
for (i in 1:length(downward_mob_seq)) {
  cases[, sel1, i] <- downward_mob_seq[i]
}

# get simulated thresholds (nsim of each)
tau <- S[,1:4]
ncat <- ncol(tau) + 1

  # empty object for linear predictor values 
  U <- array(dim = c(nrow(X), nsim, length(downward_mob_seq)))
  # get U by multiplying each draw from S (ie s) with each case
  # only multiply with relevant coefficient (not taus), hence S[, 5:11]
  for (i in 1:length(downward_mob_seq)) {
    U[, , i] <- apply(S[, 5:ncol(S)], 1, function(s)
      cases[, , i] %*% s)
  }
  
# use U to derive probabilities (with link function)
# empty object for probabilities 
probs <- array(NA, c(ncat, nsim, length(downward_mob_seq)))

for (i in 1:length(downward_mob_seq)) {
  # calculate probabilities
  probs[1, , i] <-
    apply(exp(tau[, 1] - U[, , i]) / (1 + exp(tau[, 1] - U[, , i])), 2, mean)
  
  for (j in 2:(ncat - 1)) {
    probs[j, , i] <-
      apply(exp(tau[, j] - U[, , i]) / (1 + exp(tau[, j] - U[, , i])) -
              exp(tau[j - 1] - U[, , i]) / (1 + exp(tau[j - 1] - U[, , i])), 2, mean)
  }
  
  probs[ncat, , i] <-
    apply(1 - exp(tau[, ncat - 1] - U[, , i]) / 
            (1 + exp(tau[, ncat - 1] - U[, , i])), 2, mean)
}

#return(probs)

# First difference in the predicted probability of Expected downward mobility
# predicted probability of Expected downward mobility - no expected mobility

#downward_mob_seq <- seq(min(multilevel_model$model$downward_mob), max(multilevel_model$model$downward_mob), length.out = 2)

prob_fd <- probs[,,2] - probs[,,1] 

 ## return arrays for ploting QIs with the means of the observed values 
ova_plot <- array(dim = c(length(downward_mob_seq), ncat, 3))
ova_plot_fd <- array(dim = c(1, ncat, 3))  # only 1 line because it is the FD

for (i in 1:length(downward_mob_seq)) {
  ova_plot[i, , 1] <- apply(probs[, , i], 1, mean)
  ova_plot[i, , 2] <- apply(probs[, , i], 1, quantile, 0.025)
  ova_plot[i, , 3] <- apply(probs[, , i], 1, quantile, 0.975)
}

ova_plot_fd[1, , 1] <- apply(prob_fd[, ], 1, mean)
ova_plot_fd[1, , 2] <- apply(prob_fd[, ], 1, quantile, 0.025)
ova_plot_fd[1, , 3] <- apply(prob_fd[, ], 1, quantile, 0.975)

ova_plots <- list(ova_plot, ova_plot_fd)

return(ova_plots)

#output <- list(probs, ova_plots)
#return(output)

}

ova_plots <- list()
ova_plots[[1]] <- fun_sim_pred_prob(multilevel_model = partial_pool_models[[1]])
ova_plots[[2]] <- fun_sim_pred_prob(multilevel_model = partial_pool_models[[2]])
ova_plots[[3]] <- fun_sim_pred_prob(multilevel_model = partial_pool_models[[3]])


