library(tidyverse)
library(modelr)
library(scatterplot3d)

wine <- read_csv2("wine.csv") #incarcarea setului de date

#crearea graficelor, utilizand geom_point() si geom_smooth()
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

#crearea unor histograme pentru vizualizarea distributiei variabilelor
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

#regresie simpla cu un parametru
#regresie liniară simpla pentru calitatea vinului in functie de alcool
mod_quality_alcohol <- lm(data = wine, quality ~ alcohol)
summary(mod_quality_alcohol) #există relație

#generarea unui nou set de date
grid_alcohol <- wine %>% 
  data_grid(alcohol = seq_range(alcohol, 200)) %>% 
  add_predictions(mod_quality_alcohol, "quality")
#punctele si dreapta de regresie
ggplot(wine, aes(alcohol, quality)) + geom_point() +geom_line(data = grid_alcohol, color="pink", size=2)
#corelatie intre X si Y
#intervalele de incredere pentru B0 si B1
confint(mod_quality_alcohol)

#regresii liniare simple pentru fiecare variabila independenta in functie de variabila dependenta,calitatea
mod_quality_sugar <- lm(data = wine, quality ~ residual_sugar)
summary(mod_quality_sugar)

mod_quality_acid_citric <- lm(data = wine, quality ~ citric_acid)
summary(mod_quality_acid_citric) #nu există asociere datorată relației

mod_quality_density <- lm(data = wine, quality ~ density)
summary(mod_quality_density)

mod_quality_pH <- lm(data = wine, quality ~ pH)
summary(mod_quality_pH)#exista legatura

mod_quality_fixed_acidity <- lm(data = wine, quality ~ fixed_acidity)
summary(mod_quality_fixed_acidity)#exista legatura datorată relației, dar slabă

mod_quality_volatile_acidity <- lm(data = wine, quality ~ volatile_acidity)
summary(mod_quality_volatile_acidity)#exista legatura,dar slab

mod_quality_chlorides <- lm(data = wine, quality ~ chlorides)
summary(mod_quality_chlorides)#exista legatura,dar slaba

mod_quality_free_sulfur_dioxide <- lm(data = wine, quality ~ free_sulfur_dioxide)
summary(mod_quality_free_sulfur_dioxide) #nu există legătură datorată relației

mod_quality_total_sulfur_dioxide <- lm(data = wine, quality ~ total_sulfur_dioxide)
summary(mod_quality_total_sulfur_dioxide)#există legătură

mod_quality_sulphates <- lm(data = wine, quality ~ sulphates)
summary(mod_quality_sulphates)#există legătură

#regresie multipla 
mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity <- lm(data = wine, quality ~ alcohol + residual_sugar + sulphates+fixed_acidity+volatile_acidity)
summary(mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity)

#predictie pentru variabila dependenta calitate in functie de modelul creat anterior
newquality <- tibble(alcohol = 10.00,
                     residual_sugar=2.10,
                     sulphates=0.55,
                     fixed_acidity=6.7,
                     volatile_acidity=0.310
                     )
predict(mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity, newdata = newquality, interval="confidence")
predict(mod_quality_alcohol_sugar_sulphates_fixed_acidity_volatile_acidity, newdata = newquality, interval="prediction")
