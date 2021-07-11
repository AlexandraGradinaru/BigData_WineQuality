library(rsample)
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)

wine <- read_csv2("wine.csv") #incarcarea setulu de date

wine %>% ggplot(aes(quality)) + geom_density()

set.seed(123)
wine_split <- initial_split(wine, prop=0.7)
wine_train <- training(wine_split)
wine_test <- testing(wine_split)

m1 <- rpart(
  formula = quality ~ .,
  data = wine_train,
  method = "anova"
)

m1 # afisare arbore rezultat in mod test

rpart.plot(m1) #afisare grafica arbore
plotcp(m1)
m1$cptable #afisarea parametrilor alpha


m2 <- rpart(
  formula = quality ~ .,
  data = wine_train,
  method = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10 )
)
m2
plotcp(m2)

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hyper_grid) 
models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = quality ~ .,
    data = wine_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

get_cp <- function(x){
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x){
  min <- which.min(x$cptable[,"xerror"])
  xerror <- x$cptable[min, "xerror"]
}

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error =  purrr::map_dbl(models, get_min_error)
  )

mutated_grid %>% 
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula =  quality ~ .,
  data =  wine_train,
  method = "anova",
  control = list(minsplit = 5, maxdepth = 12, cp = 0.01)
)

pred <- predict(m1, newdata = wine_test)
RMSE(pred = pred, obs = wine_test$quality)
optimal_tree
rpart.plot(optimal_tree)


#BAGGING
library(ipred)
set.seed(123)

fitControl <- trainControl(
  method = "cv",
  number = 10
)
bagged_cv <- train(
  quality ~.,
  data = wine_train,
  method = "treebag",
  trControl = fitControl,
  importance = TRUE
)
bagged_cv
plot(varImp(bagged_cv), 10)

pred1 <- predict(bagged_cv, wine_test)
RMSE(pred1, wine_test$quality)

for_plotting <- tibble(
  i = 1:1469,
  pred = pred1[],
  actual = wine_test$quality
)
ggplot(for_plotting, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +
  geom_point(aes(y = actual, color = "blue"))

ggplot(for_plotting, aes(x=i)) + 
  geom_point(aes(y = pred-actual))
