library(tidyverse)
library(readr)
winequalityN <- read_csv("winequalityN.csv")
View(winequalityN)


wine <- winequalityN %>% drop_na()
wine$type <- factor(wine$type)
wine$quality <- factor(wine$quality)

wine_1 <- winequalityN %>% drop_na() %>% select(-type)
res <- cor(wine_1)
res

library(corrplot)
corrplot(res,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

wine_dummy <- wine %>% 
  select(alcohol, density, `residual sugar`, 
         chlorides, `total sulfur dioxide`)
set.seed(555)
test_ind <- sample(nrow(wine_dummy), 0.3*nrow(wine_dummy))
wine_train <- wine_dummy[-test_ind,]
wine_check <- wine_dummy[test_ind,]
wine_test <- wine_check %>% select(-alcohol)

# library(rpart)
# library(rpart.plot)
# wine_tree <- rpart(alcohol ~ ., data = wine_train)
# rpart.plot(wine_tree)
# res <- predict(wine_tree, wine_test)
# not_drunk <- 50/(0.789*(res/100))
# not_drunk

# wine_test['not_drunk_volume'] = not_drunk 
# wine_test['alcohol'] = res

# ggplot(wine, aes(x = density, y = alcohol)) +
#   geom_point() + 
#   geom_point(data = wine_train, color = "green", alpha = 0.5) +
#   geom_point(data = wine_test, color = "red", alpha = 0.5)
# 
# ggplot(wine_check, aes(x = density, y = alcohol)) +
#   geom_point(color = "red", alpha = 0.5) +
#   geom_point(data = wine_test, color = "green", alpha = 0.5)


model_linear <- lm(alcohol ~ ., data = wine_train)
res_linear <- predict(model_linear, wine_test)
wine_test_linear <- wine_test
wine_test_linear['alcohol'] = res_linear

ggplot(wine_check, aes(x = density, y = alcohol)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_point(data = wine_test_linear, color = "green", 
             alpha = 0.5)

summary(model_linear)

not_drunk <- (0.5*6.8*68.83*10)/(0.79*res_linear)
summary(not_drunk)
hist(not_drunk)


not_drunk2 <- (0.5*6.8*68.83*10)/(0.79*wine_check$alcohol)
summary(not_drunk2)
hist(not_drunk2)


# model_gamma <- glm(alcohol ~ ., data = wine_train, family = "Gamma")
# res_gamma <- predict(model_linear, wine_test, type = "response")
# wine_test_gamma <- wine_test
# wine_test_gamma['alcohol'] = res_gamma
# 
# ggplot(wine_check, aes(x = density, y = alcohol)) +
#   geom_point(color = "red", alpha = 0.5) +
#   geom_point(data = wine_test_gamma, color = "green", 
#              alpha = 0.5)
# 
# not_drunk3 <- (0.5*6.8*68.83*10)/(0.79*res_gamma)
# summary(not_drunk3)
# hist(not_drunk3)

wine_check['not_drunk'] = not_drunk2
wine_test['not_drunk'] = not_drunk
ggplot(wine_check, aes(x = not_drunk)) + 
  geom_histogram(fill = "green", color = "black") +
  geom_histogram(data = wine_test_linear, fill = "red" ,alpha = 0.6)
