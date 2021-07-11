library(rpart)
library(rpart.plot)
library(tidyverse)
library(ISLR)
library(rsample)
library(caret)
library(partykit)

wine <- read_csv2("wine.csv") #incarcarea setulu de date

wine %>% ggplot(aes(alcohol)) + geom_density()

wine_clas <- wine %>% 
  mutate(alcohol = ifelse(alcohol <11, "No", "Yes"))
wine_clas <- wine_clas %>% 
  mutate(alcohol = factor(alcohol))
table(wine_clas$alcohol)

set.seed(123)
wine_clas_split <- initial_split(wine_clas, prop = 0.7, strata = "alcohol")
wine_clas_train <- training(wine_clas_split)
wine_clas_test <- testing(wine_clas_split)

table(wine_clas_train$alcohol)
table(wine_clas_test$alcohol) # 35% din vinuri alcoolemie mai mare de 10%

set.seed(123)

c1 = rpart(
  formula = alcohol ~ .,
  data = wine_clas_train,
  method = "class"
)

c1
summary(c1)
rpart.plot(c1)

pred_c1 <- predict(c1, newdata = wine_clas_test, target = "class" )
pred_c1 <- as_tibble(pred_c1)%>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
table(pred_c1$class, wine_clas_test$alcohol)

confusionMatrix(factor(pred_c1$class), factor(wine_clas_test$alcohol)) # acuratete de 90%


set.seed(123)
c1_pruned <- prune(c1, cp=0.01)
c1_pruned

c2 <- rpart( alcohol ~ .,
             data = wine_clas_train,
             method = "class",
             control = list(cp=0)
             )
c2
summary(c2)
rpart.plot(c2)


pred_c2 <- predict(c2, newdata = wine_clas_test, target = "class" )
pred_c2 <- as_tibble(pred_c2)%>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
confusionMatrix(factor(pred_c2$class), factor(wine_clas_test$alcohol))

c2_pruned <- prune(c2, cp=0.02)
summary(c2_pruned)

pred_c2_pruned <- predict(c2_pruned, newdata = wine_clas_test, target = "class" )
pred_c2_pruned <- as_tibble(pred_c2_pruned)%>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
confusionMatrix(factor(pred_c2_pruned$class), factor(wine_clas_test$alcohol))


library(tree)

set.seed(123)
c1_tree <- tree(alcohol ~ .,
                data = wine_clas_train)
c1_tree
summary(c1_tree)

pred_c1_tree <- predict(c1_tree, newdata = wine_clas_test, target = "class" )
pred_c1_tree <- as_tibble(pred_c1_tree)%>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
confusionMatrix(factor(pred_c1_tree$class), factor(wine_clas_test$alcohol))


set.seed(123)
c1_tree_gini <- tree(alcohol ~ .,
                data = wine_clas_train)
c1_tree_gini
summary(c1_tree_gini)

pred_c1_tree_gini <- predict(c1_tree_gini, newdata = wine_clas_test, target = "class" )
pred_c1_tree_gini <- as_tibble(pred_c1_tree_gini)%>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
confusionMatrix(factor(pred_c1_tree_gini$class), factor(wine_clas_test$alcohol))






