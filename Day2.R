#Day2
library(tidyverse)
library(boot)

survey <- read_csv(".../survey_results_public.csv")

head(survey)
unique(survey$HoursPerWeek)
#do some data cleaning
compensation_record <-survey %>%
  filter(HoursPerWeek > 0 ) %>%
  mutate(CleanedSalary = str_replace_all(Salary, "[[:punct:]]", "")) %>%
  mutate(CleanedSalary = as.numeric(CleanedSalary)) %>%
  filter(CleanedSalary > 0) %>%
  filter(CleanedSalary < 150000)


mymodel <- glm(CleanedSalary ~ (HoursPerWeek), data = compensation_record, family = poisson)

glm.diag.plots(mymodel)

unique(compensation_record$JobSatisfaction)

#it does not seem a good predictor from the diagnostic model
#we inspect job satisfaction instead

compensation_record <-compensation_record %>%
  filter(!is.na(JobSatisfaction)) %>%
  mutate(JobSatisfaction = as.numeric(JobSatisfaction))

mymodel2 <- glm(CleanedSalary ~ (JobSatisfaction), data = compensation_record, family = poisson)

glm.diag.plots(mymodel2)
min(compensation_record$Salary)
#we inspect career satisfaction 

compensation_record <-compensation_record %>%
  filter(!is.na(CareerSatisfaction)) %>%
  mutate(CareerSatisfaction = as.numeric(CareerSatisfaction))

mymodel3 <- glm(CleanedSalary ~ (CareerSatisfaction), data = compensation_record, family = poisson)

glm.diag.plots(mymodel3)

#plot first model

ggplot(compensation_record, aes(x = HoursPerWeek, y = CleanedSalary))+
  geom_point()+
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"))

#surprisingly(at least for me) we can observe that there is a negative relation btw salary and hpw, (maybe is not that surprising)




  
