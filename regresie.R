library(tidyverse)
library(modelr)
library(scatterplot3d)

wine <- read_csv2("wine.csv") #dataset upload

wine %>% ggplot(aes(fixed_acidity, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(volatile_acidity, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(citric_acid, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(residual_sugar, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(chlorides, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(free_sulfur_dioxide, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(total_sulfur_dioxide, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(density, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(pH, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(sulphates, quality)) + geom_point() + geom_smooth()
wine %>% ggplot(aes(alcohol, quality)) + geom_point() + geom_smooth()

#histograms for data distribution visualization
hist(wine$fixed_acidity, main = "fixed acidity")
hist(wine$volatile_acidity, main = "volatile_acidity")
hist(wine$citric_acid, main = "citric_acid")
hist(wine$residual_sugar, main = "residual_sugar")
hist(wine$chlorides, main = "chlorides")
hist(wine$free_sulfur_dioxide, main = "free_sulfur_dioxide")
hist(wine$total_sulfur_dioxide, main = "total_sulfur_dioxide")
hist(wine$density, main = "density")
hist(wine$pH, main = "pH")
hist(wine$sulphates, main = "sulphates")
hist(wine$alcohol, main = "alcohol")
hist(wine$quality, main = "quality")

#simple regresion with one parameter
mod_quality_alcohol <- lm(data = wine, quality ~ alcohol)
summary(mod_quality_alcohol) 

grid_alcohol <- wine %>% 
  data_grid(alcohol = seq_range(alcohol, 200)) %>% 
  add_predictions(mod_quality_alcohol, "quality")
ggplot(wine, aes(alcohol, quality)) + geom_point() +geom_line(data = grid_alcohol, color="pink", size=2)

confint(mod_quality_alcohol)
 
#simple liniar regressions for each independent variable based on the dependent variable quality
mod_quality_sugar <- lm(data = wine, quality ~ residual_sugar)
summary(mod_quality_sugar)

mod_quality_acid_citric <- lm(data = wine, quality ~ citric_acid)
summary(mod_quality_acid_citric)#no connection

mod_quality_density <- lm(data = wine, quality ~ density)
summary(mod_quality_density)

mod_quality_pH <- lm(data = wine, quality ~ pH)
summary(mod_quality_pH)#there is a connection

mod_quality_fixed_acidity <- lm(data = wine, quality ~ fixed_acidity)
summary(mod_quality_fixed_acidity)

mod_quality_volatile_acidity <- lm(data = wine, quality ~ volatile_acidity)
summary(mod_quality_volatile_acidity)

mod_quality_chlorides <- lm(data = wine, quality ~ chlorides)
summary(mod_quality_chlorides)

mod_quality_free_sulfur_dioxide <- lm(data = wine, quality ~ free_sulfur_dioxide)
summary(mod_quality_free_sulfur_dioxide) #no connection 

mod_quality_total_sulfur_dioxide <- lm(data = wine, quality ~ total_sulfur_dioxide)
summary(mod_quality_total_sulfur_dioxide)#there is a connection 

mod_quality_sulphates <- lm(data = wine, quality ~ sulphates)
summary(mod_quality_sulphates)#there is a connection based on the relation between the 2 variables

#multiple regression
mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity <- lm(data = wine, quality ~ alcohol + residual_sugar + sulphates+fixed_acidity+volatile_acidity)
summary(mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity)

#prediction for the dependent variable quality based on the previous model
newquality <- tibble(alcohol = 10.00,
                     residual_sugar=2.10,
                     sulphates=0.55,
                     fixed_acidity=6.7,
                     volatile_acidity=0.310
                     )
predict(mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity, newdata = newquality, interval="confidence")
predict(mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity, newdata = newquality, interval="prediction")
