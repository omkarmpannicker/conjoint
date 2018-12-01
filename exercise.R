#######################################
install.packages("support.CEs", dependencies=c("Depends", "Suggests"))
install.packages("AlgDesign", dependencies=c("Depends", "Suggests"))

library(support.CEs)
library(survival)
library(conjoint)

carexample <- expand.grid (price = c("low", "middle", "high"), type = c("suv", "sport", "convertible"), sits = c("2sitter", "4sitter"), hp = c("100", "150", "200"))
carexample

carexample_full <- caFactorialDesign(carexample, type="full")
cor(caEncodedDesign(carexample_full))
carexample_fractional <- caFactorialDesign(carexample, type="fractional", cards=16)
carexample_orthogonal <- caFactorialDesign(carexample, type="orthogonal")

### 
table(carexample_orthogonal$price)
table(carexample_orthogonal$type)
table(carexample_orthogonal$sits)
table(carexample_orthogonal$hp)

edited.carexample_fractional <- edited.carexample_fractional[-15,]
cor(caEncodedDesign(edited.carexample_fractional))
table(edited.carexample_fractional$price)
table(edited.carexample_fractional$type)
table(edited.carexample_fractional$sits)
table(edited.carexample_fractional$hp)



# Coffeedata
coffee <- read.table("COFFEE.DAT", header=TRUE)



