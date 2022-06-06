library(TMB)
library(lme4)

## compile and load TMB DLL
compile("vectorMixed.cpp") ##  "-O0 -g" for debugging
dyn.load(dynlib("vectorMixed"))

## extract X and Z from lmer fit (lots of other ways to do this;
## model.matrix() is fine for X, we could use lme4::lFormula() for
## Z-construction instead of going all the way through lmer()
## if we want to build fancy RE model matrices ourselves we need
## Matrix::fac2sparse() and KhatriRao (see vignette("lmer", package = "lme4")
## for details)

m1 <- lFormula(Reaction ~ Days + (Days|Subject), sleepstudy)

## construct data and starting parameter values for TMB
tmbdat <- list(X = m1$X,
               Z = t(m1$reTrms$Zt),
               yobs = sleepstudy$Reaction,
               n_re = 2L)

tmbpars <- list(beta = rep(0, ncol(tmbdat$X)),
                b = rep(0, ncol(tmbdat$Z)),
                theta = rep(0,3), ## 2 SD pars + 1 corr par ((n+1)*n/2)
                logsd = 0)

## build TMB object
obj <- MakeADFun(data = tmbdat,
                 parameters = tmbpars,
                 random = "b",
                 DLL = "vectorMixed",
                 silent = TRUE ## FALSE for debugging etc.
                 )

## fit
tmbfit1 <- with(obj, nlminb(start = par, objective = fn, gradient = gr))
sdr <- sdreport(obj)

