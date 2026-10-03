#Task 1- read the cleaned COVID data 

covid_data <- read.csv("cleaned_covid_data.csv")
#Task 2 - states that begin with A 
#Task 3 - plot deaths over time for A states 
A_states <- subset(covid_data, grepl("^A", Province_State))
A_states$Last_Update <- as.Date(A_states$Last_Update)
library(ggplot2)
ggplot(A_states, aes(x = Last_Update, y = Deaths)) + geom_line() +
  facet_wrap(~ Province_State)
#Task4 - peak Case Fatality Ratio for each State
state_max_fatality_rate <- aggregate(Case_Fatality_Ratio ~ Province_State, data = covid_data, FUN = max,na.rm = TRUE)
#task 5 - plot peak Case Fatality Ratio
ggplot(state_max_fatality_rate, aes(x=Province_State, y = Case_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
#Bonus - cumulative deaths for entire US
covid_data$Last_Update <- as.Date(covid_data$Last_Update)
US_deaths <- aggregate(Deaths ~ Last_Update, data = covid_data, FUN = sum, na.rm = TRUE)
ggplot(US_deaths, aes(x = Last_Update, y = Deaths)) + geom_line()
