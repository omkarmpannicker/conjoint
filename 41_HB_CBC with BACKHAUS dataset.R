# 41_Restructure_dataset.R
library(support.CEs)
install.packages("ChoiceModelR", dependencies=TRUE)
library(ChoiceModelR)


## After doing some modification for demonstration purposes in MS Excel, reimport dataset
dataset2 <- read.csv("Convert cho Dataset.csv", 
                     header = TRUE, sep = ";", dec = ",")
# Check structure
str(dataset2)

# Set parameter for calculation
mcmc <- list(R = 10, use = 10)
mcmc2 <- list(R = 10000, use = 2000)
mcmc3 <- list(R = 20000, use = 5000)

# Parameter for data input
none <- TRUE # none option in questionnaire
xcoding <- c(0,0,0,0)

# Parameter data output
save = TRUE
#keep = 10
keep = 10

options <- list(none = none, save = save, keep = keep)


out <- choicemodelr(dataset2, xcoding, mcmc = mcmc, options = options)
out2 <- choicemodelr(dataset2, xcoding, mcmc = mcmc2, options = options)
out3 <- choicemodelr(dataset2, xcoding, mcmc = mcmc3, options = options)

out2

summary(out3)
out3[[1]]



for (i in 1:5) {
print(out3$compdraw[[i]]$mu)
}

out3$compdraw[[3]]$mu
out3$compdraw[[2]]$mu
out3$compdraw[[1]]$mu


out2$compdraw[[1]]$mu


sum_mu = 0
for (i in 1:200) {
  sum_mu = sum_mu + out2$compdraw[[i]]$mu[1]
}
sum_mu/200















