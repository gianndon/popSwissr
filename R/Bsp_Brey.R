
library("nloptr")

# 
source("C:/Users/dgian/OneDrive - ZHAW/Studium/Data/prepareData.R")

nn <- 3

covMat <- 260*cov(ret.DJ30[,1:nn])
ret.mean <- 260*colMeans(ret.DJ30[,1:nn])
dim(covMat)
ret.mean


#--------------------------------------------------------
# Risiko minimieren
# Objective function
eval_f <- function(x, covMat, r.vec, r.ptf)
{
  return (as.numeric(t(x) %*% covMat %*% x))
}

eval_f(rep(1/nn, nn), covMat)

# lower and upper bounds
lb <- rep(-10000,nn)
ub <- rep(10000,nn)

# Constraints
eval_g_eq <- function(x, covMat, r.vec, r.ptf)
{
  constr <- c(
    sum(x) - 1,
    x %*% r.vec - r.ptf
  )
  return ( constr )
}

# Initial weights
x0 <- rep(1/nn,nn)
# Erw. Portfoliorendite
r.ptf <- 0.05

# Optimierung
# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "local_opts" = local_opts,
              "print_level" = 0 )

res <- nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                # eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts, 
                covMat = covMat,
                r.vec = ret.mean,
                r.ptf = r.ptf
)
print(res)

var.ptf <- res$objective
sigma.ptf <- sqrt(res$objective)
sigma.ptf
weights <- res$solution
weights


#--------------------------------------------------------
# Erwartete Rendite maximieren
# Objective function
eval_f <- function(x, covMat, r.vec, var.ptf)
{
  return (-as.numeric(t(x) %*% r.vec))
}

eval_f(rep(1/nn, nn), covMat, ret.mean)


# Constraint
eval_g_eq2 <- function(x, covMat, r.vec, var.ptf)
{
  constr <- c(
    sum(x) - 1,
    as.numeric(t(x) %*% covMat %*% x) - var.ptf
  )
  return ( constr )
}

# lower and upper bounds
lb <- rep(0,nn)
ub <- rep(1,nn)

# initial weights
x0 <- rep(1/nn,nn)
# Chosen portfolio variance
var.ptf <- 0.15^2
var.ptf

# tests
t(x0) %*% covMat %*% x0
as.numeric(t(x0) %*% covMat %*% x0) - var.ptf
eval_g_eq2(x=x0, covMat=covMat, r.vec=ret.mean, var.ptf=var.ptf)
as.numeric(t(x0) %*% covMat %*% x0) - var.ptf

# Optimierung
# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "local_opts" = local_opts,
              "print_level" = 2 )

res <- nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                # eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq2,
                opts = opts, 
                covMat = covMat,
                r.vec = ret.mean,
                var.ptf = var.ptf
)
print(res)

r.ptf <- -res$objective
r.ptf
# sigma.ptf <- sqrt(res$objective)
weights <- res$solution
weights

t(weights) %*% covMat %*% weights
