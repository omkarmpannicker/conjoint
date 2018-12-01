## CBC complete

#######################################################
## PART 0 - Install and load packages
# install.packages("support.CEs", dependencies = TRUE)
library(support.CEs)
library(survival)

#######################################################
## PART 1 - Creating a choice experiment design
library(support.CEs)

des1 <- rotation.design(attribute.names = list(
  Einkaufskorbgroesse = c("one-day-bag", "few-days-bag", "one-week-bag"),
  Abholbereitschaft = c("just now", "pick up later", "pick up soon"),
  Preis = c("1.99", "3.99", "9.99"),
  Benachrichtigung = c("Telefon", "SMS", "E-Mail")),
  nalternatives = 3, nblocks = 1, row.renames = FALSE,
  randomize = TRUE, seed = 987)

# case study
casestudy <- rotation.design(attribute.names = list(
  brand=c("Visa", "MasterCard", "AmEx"),
  yearlyfee= c("20", "50"),
  insurance= c("20", "50", "notpossible"),
  transfer=c("possible", "notpossible"),
  pin=c("yes", "no"),
  security=c("pin", "signature")), 
  nalternatives = 3, nblocks = 1, row.renames=FALSE, randomize = TRUE, see=987)

# case study
casestudy <- rotation.design(attribute.names = list(
  brand=c("Visa", "MasterCard", "AmEx"),
  yearlyfee= c("20", "50"),
  insurance= c("20", "50", "notpossible"),
  transfer=c("possible", "notpossible")),
  nalternatives = 3, nblocks = 3, row.renames=FALSE, randomize = TRUE, see=987)

## Step 2 - Converting a choice experiment design into questionnaire format
questionnaire(choice.experiment.design = des1)
questionnaire(choice.experiment.design = casestudy)

# Fieldphase

#######################################################
## PART 2 - Read in CBC datasets and create dataframe
data.CBC <- read.csv("survey_237541_R_data_file(1).csv", header=FALSE, stringsAsFactors=FALSE)
# V13 - V21 are respondents choices
data.CBC

data.choice <- data.CBC[,c(13:21)]
data.choice
# Replace all string values
data.choice2 <- as.data.frame(sapply(data.choice, function(x) gsub("A", "", x)))
# Replace names
names(data.choice2) <- c("q1", "q2", "q3", "q4", "q5", "q6",
                         "q7", "q8", "q9")
data.choice2

# not understood
# Insert BLOCK
data.choice2 <- data.frame(BLOCK = seq(1,1, len = nrow(data.choice2)), data.choice2)
data.choice2

# Insert ID
data.choice2 <- data.frame(ID = c(1:nrow(data.choice2)), data.choice2)
data.choice2


#######################################################
## PART 3 - Build design matrix
desmat1 <- make.design.matrix(choice.experiment.design = des1,
                              optout = TRUE, 
                              categorical.attributes = c("Einkaufskorbgroesse",
                              "Abholbereitschaft", "Preis", "Benachrichtigung"),
                              unlabeled = TRUE)
desmat1[1:3, ]

#######################################################
## PART 4 - Build dataset
dataset1 <- make.dataset(respondent.dataset = data.choice2,
                         choice.indicators = 
                           c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"),
                         design.matrix = desmat1)
dataset1[1:8, ]

#######################################################
## PART 5 - Estimate model
clogout1 <- clogit(RES ~ ASC + few.days.bag + one.week.bag +
                   pick.up.later + pick.up.soon + X3.99 + 
                     X9.99 + SMS + E.Mail + strata(STR),
                   data = dataset1)
clogout1

# Goodness of fit
gofm(clogout1)
gofm(clogout1i)
# Interaction effect between x3.99 and few.days. bag
clogout1i <- clogit(RES ~ ASC + few.days.bag + one.week.bag +
                     pick.up.later + pick.up.soon + X3.99 + 
                     X9.99 + SMS + E.Mail + X3.99:few.days.bag + strata(STR),
                   data = dataset1)
clogout1i

# Sawtooth --> Segmenting simulatenously conditional logit estimation

#######################################################
#######################################################
#######################################################
## Option 2 - Price as metric variable
#######################################################
## PART 3 - Build design matrix

desmat2 <- make.design.matrix(choice.experiment.design = des1,
                              optout = TRUE, 
                              categorical.attributes = c("Einkaufskorbgroesse",
                                                         "Abholbereitschaft", "Benachrichtigung"),
                              continuous.attributes = c("Preis"),
                              unlabeled = TRUE)
desmat2[1:3, ]


#######################################################
## PART 4 - Build dataset
dataset2 <- make.dataset(respondent.dataset = data.choice2,
                         choice.indicators = 
                           c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"),
                         design.matrix = desmat2)
dataset2[1:8, ]


#######################################################
## PART 5 - Estimate model
clogout2 <- clogit(RES ~ ASC + few.days.bag + one.week.bag +
                     pick.up.later + pick.up.soon + Preis + 
                     SMS + E.Mail + strata(STR),
                   data = dataset2)
clogout2

# with interaction effect
clogout2i <- clogit(RES ~ ASC + few.days.bag + one.week.bag +
                     pick.up.later + pick.up.soon + Preis + 
                     SMS + E.Mail + Preis:SMS + Preis:E.Mail + strata(STR),
                   data = dataset2)
clogout2i


# Goodness of fit
gofm(clogout2)
gofm(clogout2i)



#######################################################
## PART 6 - Marginal Willingness to pay
mwtp(output = clogout2, monetary.variables = c("Preis"),
     nonmonetary.variables = c("few.days.bag", "one.week.bag",
       "pick.up.later", "pick.up.soon", "SMS", "E.Mail"),
     seed = 987)

# Marginal willingness to pay for few.day.bag is EUR5.751
# Marginal willingness to pay for pick.up.later is -EUR29.967






