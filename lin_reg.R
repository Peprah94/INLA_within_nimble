#Packages needed to run the model
library(nimble)
library(INLA)
library(mvtnorm)
library(MASS)
library(parallel)
library(coda)

# function for generating samples
sample.linreg <- function(){
  n = 100
  x1 = runif(n) #covariate 1
  x2 = runif(n) #covariate 2
  err = rnorm(n) # error
  y = 3 + 2*x1 -2*x2 + err # Response
  return(list(y = y,x = matrix(c(x1,x2),ncol = 2)))
}

# Function that estimates beta0 and precision
#We return the mean, although we need the marginals
#We have to sort that out later

fit.inla <- function(x, y, beta){
  data <- list(y=y, x=x)
  data$oset = data$x %*% (beta)
  res = INLA::inla(y ~ 1 + offset(oset), data = data)
  intercept = INLA::inla.emarginal(function(x) x,res$marginals.fixed[[1]])
  precision = INLA::inla.emarginal(function(x) x,res$marginals.hyper[[1]])
  ret <- c(intercept, precision)
  return(ret)
  #list(#mlik = res$mlik,
 #dists = list(intercept = res$marginals.fixed[[1]],
 #           precision = res$marginals.hyperpar[[1]])))
}

#Testing the function fit.inla
beta <- (c(0,0))
fit.inla(df$x,df$y, beta)

## Make the INLA fnx compatible with NIMBLE
nimbleINLA<- nimbleRcall(function(x=double(2), y=double(1), beta=double(1)){}, Rfun="fit.inla", 
                                returnType = double(1))

#Compiling the nimble function
CnimbleINLA <- compileNimble(nimbleINLA)

#Testing the compiled function. 
#Should give the same results as fit.inla
CnimbleINLA(df$x,df$y, beta)

#Alternative writting of nimble function
#This fails to use INLA
nimINLA <- nimbleFunction(
  run=function(x=double(2),y=double(1), beta=double(1)){
    library(INLA)
    data <- nimbleList(y=y, x=x) 
    data$oset = data$x %*% (beta)
    res = inla(y ~ 1 + offset(oset), data = data)
    intercept = inla.emarginal(function(x) x,res$marginals.fixed[[1]])
    precision = inla.emarginal(function(x) x,res$marginals.hyper[[1]])
    ans <- nimC(intercept, precision)
    return(ans)
    returnType(double(1))
  }
)

#Code for MCMC in NIMBLE
code <- nimbleCode({
  #Prior for beta1 and beta2
  for(j in 1:2){
    beta[j] ~ dnorm(0,5)  
  }

  #Bivariate linear model specification
  for(i in 1:N){
    linpred[i] <- inla.res[1]+ beta[1]*x[i,1]+ beta[2]*x[i,2]
    y[i] ~ dnorm(linpred[i],inla.res[2]) 
    
    #We test with constant values for constant and the model works
    #linpred[i] <- 1+ beta[1]*x[i,1]+ beta[2]*x[i,2]
    #y[i] ~ dnorm(linpred[i],sd=1 ) 

  }
  
  #Fitting the inla with the simulated parameters
  inla.res[1:2] <- CnimbleINLA(x[1:N,1:2],y[1:N],beta[1:2])
})

## Parameterising the nimble model

#Data
inla_data <- list(y=df$y, 
              x = df$x)

#Constants
const <- list(N = length(df$y))

# Initial values
idm_inits <- function(){list(beta =c(0,0),
                             inla.res =c(1,0.7)
)
}

initsList <- idm_inits()

#Putting all together for the creating compilation
modelInfo <- list(
  code = code,
 constants = const,
  data = inla_data,
  inits = initsList
)

#Create the model in nimble
mwtc <- nimbleModel(code,data = inla_data, 
                    constants = const, 
                    inits = initsList)

# Create the model in C
Cmwtc <- compileNimble(mwtc,showCompilerOutput = FALSE) #Have issues compiling


mcmcconf <- configureMCMC(Cmwtc, monitors = c("beta", "inla.res"))

Rmcmc <- buildMCMC(mcmcconf, 
                   enableWAIC =FALSE)

# Compile 
cmcmc <- compileNimble(Rmcmc, 
                       project = Cmwtc,
                       resetFunctions = TRUE)

# Run the MCMC
mcmc.out <- runMCMC(cmcmc, 
                    niter = 50000,
                    nchains = 3,
                    nburnin = 25000,
                    #inits = initsList,
                    #thin =10, 
                    setSeed = TRUE, 
                    samples=TRUE, 
                    samplesAsCodaMCMC = TRUE, 
                    summary = TRUE, 
                    WAIC = FALSE)

#Output from the MCMC
output <- mcmc.out$summary$all.chains
output

# Diagnostics for the model
coda_samples <- mcmc(mcmc.out$samples)
mcmcplot(coda_samples)

#Save the output
save(output, file="output.RData")













