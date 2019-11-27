## https://github.com/bethatkinson/rpart/issues/7

library(rpart)
mtcars2 <- mtcars

mtcars2$gear <- factor(mtcars2$gear)
mtcars2$carb <- factor(mtcars2$carb)

set.seed(10)
rp1 <- rpart(mpg ~ . - gear, data = mtcars2)
set.seed(10)
rp2 <- rpart(mpg ~ ., data = mtcars2[names(mtcars2) != "gear"])

all.equal(rp1[setdiff(names(rp1), c("call", "terms"))],
          rp2[setdiff(names(rp2), c("call", "terms"))], check.attributes=FALSE)


set.seed(10)
rp3 <- rpart(mpg ~ . - gear - carb, data = mtcars2)
set.seed(10)
rp4 <- rpart(mpg ~ ., data = mtcars2[setdiff(names(mtcars2), c("gear", "carb"))])

all.equal(rp3[setdiff(names(rp3), c("call", "terms"))],
          rp4[setdiff(names(rp4), c("call", "terms"))], check.attributes=FALSE)
