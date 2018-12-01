# Read in dataset
data <- read.csv("survey_383893_R_data_file.csv", header=FALSE, quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE)

# Inspeact data
str(data)

# From wide to long format
resp.ans <- as.vector(t(as.matrix(data[,c(5:8)])))

# Schritt 1 - variables with ranking extraction
d1 <- data[,c(5:8)]
d2 <- t(as.matrix(d1))
as.vector(d2)

# package conjoint
install.packages("conjoint", dependencies=TRUE)
library(conjoint)
sets <- data.frame(EW.gr3h = c(0,0,1,1), P5.99 = c(0,1,0,1))

# Number of respondents
n.resp <- nrow(data)

# Number of profiles
n.sets <- 4

# Creating respondent id
resp.id <- rep(1:n.resp, each = n.sets)
 
sets.n.resp <- do.call("rbind", replicate(n.resp, sets, simplify = FALSE))

complete.dataset <- data.frame(resp.id, sets.n.resp, resp.ans)
complete.dataset



#######################################
install.packages("support.CEs", dependencies=c("Depends", "Suggests"))
install.packages("AlgDesign", dependencies=c("Depends", "Suggests"))

library(support.CEs)
library(survival)
library(conjoint)

# Example 1 - full factorial design
experiment <- expand.grid (price = c("3.99 Euro", "5.99 Euro"), cart = c("Bereitstellung innerhalb von 3 stunden","Bereitstellung ab 3 stunden"))

experiment 

experiment1 <- expand.grid(
  price=c("low", "medium", "high"),
  variety= c("black", "green", "red"),
  kind=c("bags", "granulated", "leafy"),
  aroma=c("yes", "no"))

experiment1
str(experiment1)

# conjoint fuction specific function: Factorial design
design <- caFactorialDesign(experiment1, type="full")
design

# Correlation
cor(caEncodedDesign(design))

# Violations of assumptions
design.red <- design[1:40,]
cor(caEncodedDesign(design.red))

# Fractional factorial design
design.fr <- caFactorialDesign(experiment1, type="fractional", cards=16)
design.fr

design.fr
nrow(design.fr)
cor(caEncodedDesign(design.fr))
table(design.fr$price)

# Orthogonal design
design.or <- caFactorialDesign(experiment1, type="orthogonal")
design.or

design.or
nrow(design.or)
cor(caEncodedDesign(design.or))
table(design.or$price)
table(design.or$variety)
table(design.or$kind)
table(design.or$aroma)



# Case study
library(conjoint)

casestudy <- expand.grid(
  brand=c("Visa", "MasterCard", "AmEx"),
  yearlyfee= c("20", "50"),
  insurance= c("20", "50", "notpossible"),
  transfer=c("possible", "notpossible"),
  pin=c("yes", "no"),
  security=c("pin", "signature"))

str(casestudy)

# conjoint fuction specific function: Factorial design
design <- caFactorialDesign(casestudy, type="full")
design

# Correlation
cor(caEncodedDesign(design))

# Violations of assumptions
design.red <- design[1:40,]
cor(caEncodedDesign(design.red))

# Fractional factorial design
design.fr <- caFactorialDesign(casestudy, type="fractional", cards=16)
design.fr

design.fr
nrow(design.fr)
cor(caEncodedDesign(design.fr))
table(design.fr$price)

# Orthogonal design
design.or <- caFactorialDesign(experiment1, type="orthogonal")
design.or

design.or
nrow(design.or)
cor(caEncodedDesign(design.or))

# to check if it is orthogonal
cor(caEncodedDesign(design.fr))

# to check if it is balanced
table

# keyingress example
# sys_25-Conjoint
# correct it later
data <- read.csv2("Datensatz_19_1.csv", header=TRUE, encoding="UTF-8", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE)

data[,c(9:20)]
resp.ans <- as.vector(t(as.matrix(data[,c(9:20)])))
resp.ans 


# library(conjoint)
help(tea)
data(tea)

# parameter estimates for individual respondent
# caModel (y, x)
caModel(resp.ans[c(1:12)], design.red)

x <- as.data.frame(tprof)
# First case
y1 <- as.data.frame(tpref[1:nrow(x),1])
# Calculate Parametes for first case
model.1 <- caModel(y1, x)
# Show estimated values person 1
model.1

library(conjoint)
Conjoint(tpref, tprof, tlevn)
data(tea)
