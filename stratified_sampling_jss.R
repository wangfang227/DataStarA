#-----------------------------------------------------------
# Get example data from package 'sampling'
data("swissmunicipalities", package = "sampling")

#-----------------------------------------------------------
# Build 'sampling frame' data frame

frame <- NULL
frame$id <- swissmunicipalities$Nom

frame$Y1 <- swissmunicipalities$Pop020
frame$Y2 <- swissmunicipalities$Pop2040
frame$Y3 <- swissmunicipalities$Pop4065
frame$Y4 <- swissmunicipalities$Pop65P

library("SamplingStrata")
set.seed(1508)
frame$X1 <- var.bin(swissmunicipalities$POPTOT, bins = 18)
frame$X2 <- var.bin(swissmunicipalities$Surfacesbois, bins = 3)
frame$X3 <- var.bin(swissmunicipalities$Surfacescult, bins = 3)
frame$X4 <- var.bin(swissmunicipalities$Alp, bins = 3)
frame$X5 <- var.bin(swissmunicipalities$Airbat, bins = 3)
frame$X6 <- var.bin(swissmunicipalities$Airind, bins = 3)

frame$domainvalue <- swissmunicipalities$REG
frame <- as.data.frame(frame)

head(frame)

#-----------------------------------------------------------
# Build 'strata' dataframe

strata <- buildStrataDF(frame)
str(strata)

#-----------------------------------------------------------
# Build 'constraints' dataframe

cv <- data.frame(DOM = "DOM1", CV1 = 0.05, CV2 = 0.05, CV3 = 0.05, CV4 = 0.05,
  domainvalue = 1:7)
cv

#-----------------------------------------------------------
# Calculate allocation for atomic strata

errors <- cv[1, 1:5]
allocation <- (bethel(strata, errors))
length(allocation)
sum(allocation)

#-----------------------------------------------------------
# Optimise stratification

solution <- optimizeStrata(errors = cv, strata = strata, cens = NULL, strcens = FALSE, 
  initialStrata = nrow(strata), addStrataFactor = 0, minnumstr = 2, iter = 400, 
  pops = 20, mut_chance = 0.005, elitism_rate = 0.2, highvalue = 1e+08, suggestions = NULL, 
  realAllocation = TRUE, writeFiles = TRUE)

# total sample size for best stratification
sum(ceiling(solution$aggr_strata$SOLUZ))

#-----------------------------------------------------------
# assign aggregate strata labels to atomic strata

newstrata <- updateStrata(strata, solution, writeFiles = TRUE)
head(newstrata)

#-----------------------------------------------------------
# visualise relations between atomic strata and aggregate strata

strata_aggregation <- read.delim("strata_aggregation.txt")
head(strata_aggregation)

#-----------------------------------------------------------
# assign aggregate strata labels to sampling frame units

framenew <- updateFrame(frame, newstrata, writeFiles = TRUE)
#-----------------------------------------------------------
# select sample from optimally stratified frame

sample <- selectSample(framenew, solution$aggr_strata, writeFiles = TRUE)

#-----------------------------------------------------------
# evaluate the optimal solution by repeated sampling

evalSolution(framenew, solution$aggr_strata, nsampl = 1000, writeFiles = TRUE)
expected_cv <- read.csv("expected_cv.csv")
expected_cv

#-----------------------------------------------------------
# consider package 'stratification' optimisation methods

library("stratification")
data("USbanks", package = "stratification")
LHkozak <- strata.LH(x = USbanks, CV = 0.01, Ls = 5, alloc = c(0.5, 0, 0.5), takeall = 0, 
  algo = "Kozak")
LHkozak

#-----------------------------------------------------------
# prepare corresponding input for 'SamplingStrata'

frame <- data.frame(Y1 = USbanks, X1 = rep(1:length(unique(USbanks)), table(USbanks)), 
  domainvalue = rep(1, length(USbanks)))
frame$id <- row.names(frame)
strata <- buildStrataDF(frame)
strata <- strata[order(strata$X1), ]
cv <- data.frame(DOM = "DOM1", CV1 = 0.01, domainvalue = 1)

# Suggestion: solution given by quintiles

pop <- 20
z <- cut(strata$M1, quantile(strata$M1, probs = seq(0, 1, 0.2)), label = FALSE, include.lowest = TRUE)
v <- matrix(z, nrow = pop - 1, ncol = nrow(strata), byrow = TRUE)

#-----------------------------------------------------------
# optimise with 'SamplingStrata'

solution <- optimizeStrata(cv, strata, cens = NULL, strcens = FALSE, alldomains = TRUE, 
  dom = NULL, initialStrata = 5, addStrataFactor = 0, minnumstr = 2, iter = 10000, 
  pops = pop, mut_chance = 5e-04, elitism_rate = 0.2, highvalue = 1e+08, suggestions = v, 
  realAllocation = TRUE, writeFiles = TRUE)

#-----------------------------------------------------------
# sample size with optimal stratification

solution$aggr_strata
sum(bethel(solution$aggr_strata, cv))
newstrata <- updateStrata(strata, solution, writeFiles = TRUE)
framenew <- updateFrame(frame, newstrata, writeFiles = TRUE)

#-----------------------------------------------------------
# plot resulting strata

pdf("strata.pdf", height = 5, width = 7)
bx <- boxplot(Y1 ~ LABEL, data = framenew, col = "orange", notch = TRUE, varwidth = TRUE, 
  xlab = "Strata", ylab = "Resources (millions $) of commercial US banks", plot = TRUE)
dev.off()

#-------------------------------------------------------------

# Code required to replicate Tables 1, 2 and 3 (note: processing time
# is more than 20 hours on an Intel Pentium 2.60 Ghz)

#-------------------------------------------------------------

library("stratification")
library("SamplingStrata")
set.seed(1508)

#----------- Dataset "USbanks" ------------------------

data("USbanks", package = "stratification")

#----------- geometric "USbanks" ------------------------
geo.USbanks <- strata.geo(x = USbanks, CV = 0.01,
                  Ls = 5, alloc = c(0.5, 0, 0.5))

#----------- cumulative root frequencies  "USbanks" --------
cum.USbanks <- strata.cumrootf(x = USbanks, CV = 0.01,
                       Ls = 5, alloc = c(0.5, 0, 0.5))

#------------ Lavallee-Hidiriglou Sethi "USbanks" -----------
LHsethi.USbanks <- strata.LH(x = USbanks, CV = 0.01, Ls = 5,
                     alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Sethi")

#------------ Lavallee-Hidiriglou Kozak "USbanks" -----------
LHkozak.USbanks <- strata.LH(x = USbanks, CV = 0.01, Ls = 5,
                     alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Kozak")

#--------------------------------------------------

# Table 1
method <- c("Geometric","Cum-Root Frequency","LH(Sethi)","LH(Kozak)")
sample.size <- c(geo.USbanks$n,cum.USbanks$n,LHsethi.USbanks$n,LHkozak.USbanks$n)
tab1 <- cbind(method,sample.size)

#------------- Genetic Algorithm "USbanks" ------------------
frame <- data.frame(Y1 = USbanks, X1 = rep(1:length(unique(USbanks)), table(USbanks)), 
                    domainvalue = rep(1, length(USbanks)))
frame$id <- row.names(frame)
strata <- buildStrataDF(frame)
strata <- strata[order(strata$X1), ]
cv <- data.frame(DOM = "DOM1", CV1 = 0.01, domainvalue = 1)
pop <- 20
z <- cut(strata$M1, quantile(strata$M1, probs = seq(0, 1, 0.2)), label = FALSE, include.lowest = TRUE) # suggestions to GA: quintiles
v <- matrix(z, nrow = pop - 1, ncol = nrow(strata), byrow = TRUE)
# note: processing time of next statement is approximately
# 2 hours on an Intel Pentium 2.60 Ghz
GA.USbanks <- optimizeStrata(cv, strata, cens = NULL, strcens = FALSE, alldomains = TRUE, 
                           dom = NULL, initialStrata = 5, addStrataFactor = 0, minnumstr = 2, 
						   iter = 10000, 
                           pops = pop, mut_chance = 5e-04, elitism_rate = 0.2, highvalue = 1e+08, suggestions = v, 
                           realAllocation = TRUE, writeFiles = FALSE, showPlot=FALSE)
newstrata <- updateStrata(strata, GA.USbanks, writeFiles = FALSE)
framenew <- updateFrame(frame, newstrata, writeFiles = FALSE)
bx <- boxplot(Y1 ~ LABEL, data = framenew, col = "orange", notch = TRUE, varwidth = TRUE, 
              xlab = "Strata", ylab = "Resources (millions $) of commercial US banks", plot = TRUE)
bx$stats <- bx$stats[,order(bx$stats[1,])]
# compute bounds of obtained strata
bounds <- rep(0,ncol(bx$stats))
for (i in (1:(ncol(bx$stats)-1))) 
  bounds[i] <- (bx$stats[nrow(bx$stats),i]+abs((bx$stats[1,i+1]-bx$stats[nrow(bx$stats),i])/2))
bounds[ncol(bx$stats)] <- max(USbanks)
bounds
#----------------------------------------------------------- 
# Table 2
#----------------------------------------------------------- 
stratum <- c(1:5)
LH.upperbounds <- c(LHkozak.USbanks$bh,max(USbanks))
GA.upperbounds <- bounds
LH.population <- LHkozak.USbanks$Nh
GA.population <- table(framenew$LABEL)
LH.allocation <- LHkozak.USbanks$nh
GA.allocation <- ceiling(GA.USbanks$aggr_strata$SOLUZ)
tab2 <- cbind(stratum,LH.upperbounds,GA.upperbounds,LH.population,GA.population,LH.allocation,GA.allocation)
#----------------------------------------------------------- 

#----------- Dataset "UScities" ------------------------

data("UScities", package = "stratification")

#----------- geometric  "UScities" ------------------------
geo.UScities <- strata.geo(x = UScities, CV = 0.01,
                          Ls = 5, alloc = c(0.5, 0, 0.5))

#----------- cumulative root frequencies "UScities" --------
cum.UScities <- strata.cumrootf(x = UScities, CV = 0.01,
                               Ls = 5, alloc = c(0.5, 0, 0.5))

#------------ Lavallee-Hidiriglou Sethi "UScities" -----------
LHsethi.UScities <- strata.LH(x = UScities, CV = 0.01, Ls = 5,
                             alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Sethi")

#------------ Lavallee-Hidiriglou Kozak "UScities" -----------
LHkozak.UScities <- strata.LH(x = UScities, CV = 0.01, Ls = 5,
                             alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Kozak")

#------------- Genetic Algorithm "UScities" ------------------

frame <- data.frame(Y1 = UScities, X1 = rep(1:length(unique(UScities)), table(UScities)), 
                    domainvalue = rep(1, length(UScities)))
frame$id <- row.names(frame)
strata <- buildStrataDF(frame)
strata <- strata[order(strata$X1), ]
cv <- data.frame(DOM = "DOM1", CV1 = 0.01, domainvalue = 1)
pop <- 20
z <- cut(strata$M1, quantile(strata$M1, probs = seq(0, 1, 0.2)), label = FALSE, include.lowest = TRUE)
v <- matrix(z, nrow = pop - 1, ncol = nrow(strata), byrow = TRUE)
# note: processing time of next statement is approximately
# 4 hours on an Intel Pentium 2.60 Ghz
GA.UScities <- optimizeStrata(cv, strata, cens = NULL, strcens = FALSE, alldomains = TRUE, 
                             dom = NULL, initialStrata = 5, addStrataFactor = 0, minnumstr = 2, 
							 iter = 20000, 
                             pops = pop, mut_chance = 5e-04, elitism_rate = 0.2, highvalue = 1e+08, suggestions = v, 
                             realAllocation = TRUE, writeFiles = FALSE, showPlot=FALSE)

#----------- Dataset "UScolleges" ------------------------

data("UScolleges", package = "stratification")

#----------- geometric "UScolleges" ------------------------
geo.UScolleges <- strata.geo(x = UScolleges, CV = 0.01,
                          Ls = 5, alloc = c(0.5, 0, 0.5))

#----------- cumulative root frequencies "UScolleges" --------
cum.UScolleges <- strata.cumrootf(x = UScolleges, CV = 0.01,
                               Ls = 5, alloc = c(0.5, 0, 0.5))

#------------ Lavallee-Hidiriglou Sethi "UScolleges" -----------
LHsethi.UScolleges <- strata.LH(x = UScolleges, CV = 0.01, Ls = 5,
                             alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Sethi")

#------------ Lavallee-Hidiriglou Kozak "UScolleges" -----------
LHkozak.UScolleges <- strata.LH(x = UScolleges, CV = 0.01, Ls = 5,
                             alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Kozak")

#------------- Genetic Algorithm "UScolleges" ------------------

frame <- data.frame(Y1 = UScolleges, X1 = rep(1:length(unique(UScolleges)), table(UScolleges)), 
                    domainvalue = rep(1, length(UScolleges)))
frame$id <- row.names(frame)
strata <- buildStrataDF(frame)
strata <- strata[order(strata$X1), ]
cv <- data.frame(DOM = "DOM1", CV1 = 0.01, domainvalue = 1)
pop <- 20
z <- cut(strata$M1, quantile(strata$M1, probs = seq(0, 1, 0.2)), label = FALSE, include.lowest = TRUE)
v <- matrix(z, nrow = pop - 1, ncol = nrow(strata), byrow = TRUE)
# note: processing time of next statement is approximately
# 2 hours on an Intel Pentium 2.60 Ghz
GA.UScolleges <- optimizeStrata(cv, strata, cens = NULL, strcens = FALSE, alldomains = TRUE, 
                             dom = NULL, initialStrata = 5, addStrataFactor = 0, minnumstr = 2, 
							 iter = 10000, 
                             pops = pop, mut_chance = 5e-04, elitism_rate = 0.2, highvalue = 1e+08, suggestions = v, 
                             realAllocation = TRUE, writeFiles = FALSE, showPlot=FALSE)
  						 
#----------- Dataset "Debtors" ------------------------

data("Debtors", package = "stratification")

#----------- geometric "Debtors" ------------------------
geo.Debtors <- strata.geo(x = Debtors, CV = 0.0359,
                          Ls = 5, alloc = c(0.5, 0, 0.5))

#----------- cumulative root frequencies "Debtors" --------
cum.Debtors <- strata.cumrootf(x = Debtors, CV = 0.0359,
                               Ls = 5, alloc = c(0.5, 0, 0.5))

#------------ Lavallee-Hidiriglou Sethi "Debtors" -----------
LHsethi.Debtors <- strata.LH(x = Debtors, CV = 0.0359, Ls = 5,
                             alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Sethi")

#------------ Lavallee-Hidiriglou Kozak "Debtors" -----------
LHkozak.Debtors <- strata.LH(x = Debtors, CV = 0.0359, Ls = 5,
                             alloc = c(0.5, 0, 0.5), takeall = 0, algo = "Kozak")

#------------- Genetic Algorithm "Debtors" ------------------

frame <- data.frame(Y1 = Debtors, X1 = rep(1:length(unique(Debtors)), table(Debtors)), 
                    domainvalue = rep(1, length(Debtors)))
frame$id <- row.names(frame)
strata <- buildStrataDF(frame)
strata <- strata[order(strata$X1), ]
cv <- data.frame(DOM = "DOM1", CV1 = 0.0359, domainvalue = 1)
pop <- 20
z <- cut(strata$M1, quantile(strata$M1, probs = seq(0, 1, 0.2)), label = FALSE, include.lowest = TRUE)
v <- matrix(z, nrow = pop - 1, ncol = nrow(strata), byrow = TRUE)
# note: processing time of next statement is approximately
# 25 hours on an Intel Pentium 2.60 Ghz
GA.Debtors <- optimizeStrata(cv, strata, cens = NULL, strcens = FALSE, alldomains = TRUE, 
                             dom = NULL, initialStrata = 5, addStrataFactor = 0, minnumstr = 2, 
							 iter = 125000, 	# very high number of iterations required
                             pops = pop, mut_chance = 5e-04, elitism_rate = 0.2, highvalue = 1e+08, suggestions = v, 
                             realAllocation = TRUE, writeFiles = FALSE, showPlot=FALSE)
							 
#----------------------------------------------------------- 
# Table 3
#----------------------------------------------------------- 
Dataset <- c("UScities","UScolleges","USbanks","Debtors")
CV <- c("0.01","0.01","0.01","0.0359")
Strata <- rep("5",4)
Geometric <- c(geo.UScities$n,geo.UScolleges$n,geo.USbanks$n,geo.Debtors$n)
CumFreq <- c(cum.UScities$n,cum.UScolleges$n,cum.USbanks$n,cum.Debtors$n)
LH.Sethi <- c(LHsethi.UScities$n,LHsethi.UScolleges$n,LHsethi.USbanks$n,LHsethi.Debtors$n)
LH.Kozak <- c(LHkozak.UScities$n,LHkozak.UScolleges$n,LHkozak.USbanks$n,LHkozak.Debtors$n)
Genetic <- c(
  sum(ceiling(GA.UScities$aggr_strata$SOLUZ)),
  sum(ceiling(GA.UScolleges$aggr_strata$SOLUZ)),
  sum(ceiling(GA.USbanks$aggr_strata$SOLUZ)),
  sum(ceiling(GA.Debtors$aggr_strata$SOLUZ))
  )
tab3 <- cbind(Dataset,CV,Strata,Geometric,CumFreq,LH.Sethi,LH.Kozak,Genetic)



