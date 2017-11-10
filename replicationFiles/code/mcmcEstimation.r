# Important note: This script is meant to be sourced from mainScript.r
#
# It re-estimates the components of partisan bias. MCMC estimation can
# be quite slow, depending on your computer's capacity. Be patient, or
# work with the distributed estimates.

res2003d97v <- my.jags(which.elec = 2003,
                  which.map  = "d97",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE,
                  D=298 # two districts missing from 1997 data
)
res2003d97v.bar <- my.jags(which.elec = 2003,
                  which.map  = "d97",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE,
                  D=298 # two districts missing from 1997 data
)
res2003d97w.bar <- my.jags(which.elec = 2003,
                  which.map  = "d97",
                  which.measure = "w.bar",
                  test.ride=FALSE,
                  D=298 # two districts missing from 1997 data
)
res2003d0v <- my.jags(which.elec = 2003,
                  which.map  = "d0",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2003d0v.bar <- my.jags(which.elec = 2003,
                  which.map  = "d0",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2003d0w.bar <- my.jags(which.elec = 2003,
                  which.map  = "d0",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2006d0v <- my.jags(which.elec = 2006,
                  which.map  = "d0",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2006d0v.bar <- my.jags(which.elec = 2006,
                  which.map  = "d0",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2006d0w.bar <- my.jags(which.elec = 2006,
                  which.map  = "d0",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2009d0v <- my.jags(which.elec = 2009,
                  which.map  = "d0",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2009d0v.bar <- my.jags(which.elec = 2009,
                  which.map  = "d0",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2009d0w.bar <- my.jags(which.elec = 2009,
                  which.map  = "d0",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2012d0v <- my.jags(which.elec = 2012,
                  which.map  = "d0",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2012d0v.bar <- my.jags(which.elec = 2012,
                  which.map  = "d0",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2012d0w.bar <- my.jags(which.elec = 2012,
                  which.map  = "d0",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2012d3v <- my.jags(which.elec = 2012,
                  which.map  = "d3",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2012d3v.bar <- my.jags(which.elec = 2012,
                  which.map  = "d3",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2012d3w.bar <- my.jags(which.elec = 2012,
                  which.map  = "d3",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2015d0v <- my.jags(which.elec = 2015,
                  which.map  = "d0",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2015d0v.bar <- my.jags(which.elec = 2015,
                  which.map  = "d0",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2015d0w.bar <- my.jags(which.elec = 2015,
                  which.map  = "d0",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2015d3v <- my.jags(which.elec = 2015,
                  which.map  = "d3",
                  which.measure = "v",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2015d3v.bar <- my.jags(which.elec = 2015,
                  which.map  = "d3",
                  which.measure = "v.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)
res2015d3w.bar <- my.jags(which.elec = 2015,
                  which.map  = "d3",
                  which.measure = "w.bar",
                  model.file=lambda.rho.5,
                  test.ride=FALSE
)


# create a list with all results in
biasRespOnLinzerSimsRPM <- lapply(ls(pattern = "res[0-9]"), get);
names(biasRespOnLinzerSimsRPM) <- ls(pattern = "res[0-9]")

print("list biasRespOnLinzerSimsRPM hes been created, with the following objects:"); summary(biasRespOnLinzerSimsRPM)

# clean
rm(list=ls(pattern = "res[0-9]")) 

# END mcmcEstimation.r
