
# Preliminaries

# SAME NAMES WILL BE AN ISSUE

beta0 <- 1
beta1 <- 1
beta2 <- 0.2

N <- 100  # Number of samples
M <- 10000  # Number of data sets

#Randomly sampling Y and X from the population N = 100 times
X_1 <- rnorm(N, mean = 1, sd = 2)
u <- rt(N, df = 7)
X_1_squared = (X_1)^2
Y_1 = beta0 + beta1 * X_1 + beta2 * X_1_squared + u

#MAKE X

# Pairs

for (b in 1:B){
    index_b = sample.int(N, size=N, replace =T); # index for b-th bootstrap sample
    y_b = y[index_b, ,drop=F]; #b-th bootstrap sample
    X_b = X[index_b, ,drop=F]; #b-th bootstrap sample

    #CHECK IS IT THIS INSTEAD???
    #    y_b <- y[index_b]  # Resampled y
    #X_b <- X[index_b, ]  # Resampled X

    beta_b = solve(t(X_b) %*% X_b) %*% ( t(X_b) %*% y_b ); # OLS estimate from b-th bootstrap sample
    mat_b_pb[b, ] = beta_b;

    #Do i put after the B loop or before?
    beta1_bootstrap_estimates <- mat_b_pb[, 2]
}

#IS THIS RIGHT????? 

beta1boot <- mean(beta1_bootstrap_estimates)

stderror_beta1 <- std_errors[2] #CHECK THIS IS RIGHT ONE LOLZ

# Calculate the standard deviation of beta1 estimates
    sd_beta1boot <- sd(beta1_bootstrap_estimates)

    # Compute the 95% confidence interval for beta1
    lower_bound_pairs <- beta1boot - 1.96 * sd_beta1boot
    upper_bound_pairs <- beta1boot + 1.96 * sd_beta1boot
  
    # Check if the true value of beta1 is within the confidence interval 
    I_1 <- as.integer(beta1 >= lower_bound_pairs & beta1 <= upper_bound_pairs)
print(I_1)


# Residual

 #--- Full sample OLS estimation
    b_ols = solve(t(X) %*% X) %*% ( t(X) %*% y );
    uhat_ols = y - X%*%b_ols; # residual
    Xb_ols = X %*% b_ols;

    # --- Nonparametric Residual Bootstrap
    mat_b_rb = matrix(NA, B, K); #placeholder for bootstrap estimates
    
    for (b in 1:B){
    index_b = sample.int(N, size=N, replace =T); # index for b-th bootstrap sample
    u_b = uhat_ols[index_b, , drop=F]; #b-th bootstrap sample
    y_b = Xb_ols + u_b; #b-th bootstrap sample
    beta_b = solve(t(X) %*% X) %*% ( t(X) %*% y_b ); # OLS estimate from b-th bootstrap sample
    mat_b_rb[b, ] = beta_b;
    beta1_bootstrap_resi_estimates <- mat_b_pb[, 2]
    }

    #Do i put after the B loop or before?

    #IS THIS RIGHT?????
    beta1boot_resi <- mean(beta1_bootstrap_resi_estimates)

    # Calculate the standard deviation of beta1 estimates
    sd_beta1boot_resi <- sd(beta1_bootstrap_resi_estimates)

    # Compute the 95% confidence interval for beta1
    lower_bound_resi <- beta1boot_resi - 1.96 * sd_beta1boot_resi
    upper_bound_resi <- beta1boot_resi + 1.96 * sd_beta1boot_resi
  
# Check if the true value of beta1 is within the confidence interval 
I_2 <- as.integer(beta1 >= lower_bound_resi & beta1 <= upper_bound_resi)

# Wild

# --- Full sample OLS estimation
    b_ols = solve(t(X) %*% X) %*% ( t(X) %*% y );
    uhat_ols = y - X%*%b_ols; # residual
    Xb_ols = X %*% b_ols;

    # --- Wild Bootstrap
    mat_b_wb = matrix(NA, B, K); #placeholder for bootstrap estimates
    for (b in 1:B){
    u_b = sign( matrix(runif(N, -1,1)) ) * uhat_ols; #b-th bootstrap sample
    y_b = Xb_ols + u_b; #b-th bootstrap sample
    beta_b = solve(t(X) %*% X) %*% ( t(X) %*% y_b ); # OLS estimate from b-th bootstrap sample
    mat_b_wb[b, ] = beta_b;
    }

    beta1_bootstrap_wild_estimates <- mat_b_pb[, 2]

    #Do i put after the B loop or before?

    #IS THIS RIGHT?????
    beta1boot_wild <- mean(beta1_bootstrap_wild_estimates)

    # Calculate the standard deviation of beta1 estimates
    sd_beta1boot_wild <- sd(beta1_bootstrap_wild_estimates)

    # Compute the 95% confidence interval for beta1
    lower_bound_wild <- beta1boot_wild - 1.96 * sd_beta1boot_wild
    upper_bound_wild <- beta1boot_wild + 1.96 * sd_beta1boot_wild
  
    # Check if the true value of beta1 is within the confidence interval 
    I_3 <- as.integer(beta1 >= lower_bound_wild & beta1 <= upper_bound_wild)

#Jackknife

# --- Jackknife
    mat_b_jk = matrix(NA, N, K); #placeholder for bootstrap estimates

    for (b in 1:B){
    y_b = y[-b,,drop=F]; #removing b-th row
    X_b = X[-b,,drop=F]; #removing b-th row
    beta_b = solve(t(X_b) %*% X_b) %*% ( t(X_b) %*% y_b ); # OLS estimate from b-th bootstrap sample
    mat_b_jk[b, ] = beta_b;
}

# WHY CANT WE TAKE THE USUAL STANDARD DEVIATION?

# need to figure out if this is actually taking the sd of just the beta1 jackknife estimates
# --- Standard error estimates
bbar = apply(mat_b_jk, 2, mean);
se_b_jk = sqrt( (N-1)/N * apply( ( mat_b_jk - matrix(1,N,1) %*% bbar )ˆ2 , 2, sum) );

# Compute the 95% confidence interval for beta1
    lower_bound_jack <- beta1boot_jack - 1.96 * se_b_jk
    upper_bound_jack <- beta1boot_jack + 1.96 * se_b_jk
  
    # Check if the true value of beta1 is within the confidence interval 
    I_4 <- as.integer(beta1 >= lower_bound_jack & beta1 <= upper_bound_jack)


