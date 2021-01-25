options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
str(titanic)
titanic %>% ggplot(aes(Age, color = Sex, y=..count..))+ geom_density()
titanic %>% ggplot(aes(Age, color = Sex))+ geom_density()
titanic %>% ggplot(aes(Sex, Age)) + geom_jitter(width = 0.1, alpha = 0.2)
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>% ggplot(aes(sample=Age)) + geom_qq(dparams = params) + geom_abline()
titanic %>% ggplot(aes(Survived, fill=Survived)) +geom_bar()
titanic %>% filter(Survived == 1) %>% ggplot(aes(Sex, fill=Sex)) +geom_bar(position = position_dodge())
titanic %>% filter(Sex == "male") %>% ggplot(aes(Survived, fill=Survived)) +geom_bar()
titanic %>% filter(Sex == "female") %>% ggplot(aes(Survived, fill=Survived)) +geom_bar()
titanic %>% ggplot(aes(Age, Survived, fill = Survived, y = ..count..)) +geom_density(alpha = 0.2)+facet_grid(Survived~.)
titanic %>% filter (Fare > 0) %>% ggplot(aes(Survived, Fare)) + geom_boxplot()+ geom_jitter(alpha = 0.2) +scale_y_continuous(trans = "log2")
titanic %>% ggplot(aes(Pclass)) +geom_bar()
titanic %>% ggplot(aes(Pclass, fill = Survived)) +geom_bar(position = position_fill()) 
titanic %>% ggplot(aes(Survived, fill = Pclass)) +geom_bar(position = position_fill()) 
titanic %>% ggplot(aes(Age, fill = Survived, y=..count..)) + geom_density(alpha = 0.2) + facet_grid(Sex~Pclass)
