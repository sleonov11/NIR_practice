library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")
library(readr)

# Загрузка данных (волна 27, вариант 14)
data <- read_csv("r27i_os_31.csv")
data2 = select(data, wj13.2, wh5 ,w_marst, w_diplom, w_age, status, wj6.2) #Где:
# 1) wj13.2 – За последние 12 месяцев какова была Ваша среднемесячная зарплата
# на этом предприятии после вычета налогов - независимо от того, платят Вам ее
#вовремя или нет?
# 2) wh5 – Пол респондента (1-М, 2-Ж)
# 3) w_marst – Семейное положение (значения 1-6)
# 4) w_diplom – Законченное образование (значения 1-6)
# 5) w_age – Количество полных лет 
# 6) status – Тип населенного пункта (значения 1-4)
# 7) wj6.2 – Сколько часов в среднем продолжается Ваша обычная рабочая неделя?
# Исключим строки с отсутствующими значениями NA
data2[data2 > 720000] <- NA
data2 = na.omit(data2)
glimpse(data2)

# Зарплата  с эл-ми нормализации
sal = as.numeric(data2$wj13.2)
sal1 = as.character(data2$wj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))

# Пол
data2["sex"]=data2$wh5
data2$sex[which(data2$sex!='1')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)

# Семейное положение
data2["wed"] = data2$w_marst

data2$wed1 = 0 # В браке
data2$wed1[which(data2$wed == '2')] <- 1
data2$wed1[which(data2$wed == '6')] <- 1
data2$wed1 = as.numeric(data2$wed1)

data2$wed2 = 0  # Разведен - вдовец
data2$wed2[which(data2$wed == '4')] <- 1
data2$wed2[which(data2$wed == '5')] <- 1
data2$wed2 = as.numeric(data2$wed2)

data2$wed3 = 0 # В браке не состоял
data2$wed3[which(data2$wed == '1')] <- 1
data2$wed3[which(data2$wed == '3')] <- 1
data2$wed3 = as.numeric(data2$wed3)

# Высшее образование
data2["higher_educ"] = 0
data2$higher_educ[which(data2$w_diplom == '6')] <- 1

# Возраст с эл-ми нормализации
age1 = as.character(data2$w_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))

# Тип населенного пункта
data2["city_status"] = 0
data2$city_status[which(data2$status == '1')] <- 1
data2$city_status[which(data2$status == '2')] <- 1
data2$city_status = as.numeric(data2$city_status)

# Длительность рабочей недели с эл-ми нормализации
dur1 = as.character(data2$wj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))

# task 1
model_1 = lm(salary ~ sex + wed1 + wed2 + wed3 + higher_educ + age +
               city_status + dur, data = data2)
summary(model_1) #R^2 = 0.1935
alias(model_1) # wed3 является линейной комбинацией переменных wed1 и wed2,
# исключим wed3.
model_12 = lm(salary ~ sex + wed1 + wed2 + higher_educ + age + city_status +
                dur, data = data2)
alias(model_12) # линейной зависимости нет
summary(model_12) # R^2 = 0.1022 низкий, p-val 2.2e-16 значение хорошее
vif(model_12) # по всем параметрам < 2


# task 2
# log
num1 = abs(trunc(min(data2$age))) + 1
num2 = abs(trunc(min(data2$dur))) + 1
num3 = abs(trunc(min(data2$salary))) + 1
model_2 = lm(salary ~ sex + wed1 + wed2 + higher_educ + log(age + num1) +
               city_status + log(dur + num2), data = data2)
model_22 = lm(salary ~ sex + wed1 + wed2 + higher_educ + log(age + num1) +
                city_status + dur, data = data2)
model_23 = lm(salary ~ sex + wed1 + wed2 + higher_educ + age + city_status +
                log(dur + num2), data = data2)
model_24 = lm(log(salary + num3) ~ sex + wed1 + wed2 + higher_educ + age +
                city_status + dur, data = data2)
summary(model_2) # R^2 = 0.2105 Лучшая модель
vif(model_2) # vif < 2
summary(model_22) # R^2 = 0.2086 
vif(model_22) # vif < 2
summary(model_23) # R^2 = 0.1939 
vif(model_23) # vif < 2
summary(model_24) # R^2 = 0.1731 , худшая из вышеперечисленных
vif(model_24) # < 2


#Перебор степеней для age, dur и salary
best_deg_age <- NULL
best_deg_dur <- NULL
best_deg_salary <- NULL
max_adj_r2 <- -Inf

# Перебор степеней для age
for (deg in seq(0.1, 2, by = 0.1)) {
model_age <- lm(salary ~ sex + wed1 + wed2 + higher_educ + I(age^deg) + city_status + dur, data = data2)
current_adj_r2 <- summary(model_age)$adj.r.squared
if (current_adj_r2 > max_adj_r2) {
  max_adj_r2 <- current_adj_r2
  best_deg_age <- deg
  }
}
#Лучшая степень для age: 1.2 R^2 = 0.25686 

# Перебор степеней для dur
for (deg in seq(0.1, 2, by = 0.1)) {
  model_dur <- lm(salary ~ sex + wed1 + wed2 + higher_educ + age + city_status + I(dur^deg), data = data2)
  current_adj_r2 <- summary(model_dur)$adj.r.squared
  if (current_adj_r2 > max_adj_r2) {
    max_adj_r2 <- current_adj_r2
    best_deg_dur <- deg
  }
}
#Лучшая степень для dur:0.9  R^2 = 0.2195

# Перебор степеней для salary (преобразование зависимой переменной)
for (deg in seq(0.1, 2, by = 0.1)) {
  model_salary <- lm(I(salary^deg) ~ sex + wed1 + wed2 + higher_educ + age + city_status + dur, data = data2)
  current_adj_r2 <- summary(model_salary)$adj.r.squared
  if (current_adj_r2 > max_adj_r2) {
    max_adj_r2 <- current_adj_r2
    best_deg_salary <- deg
  }
}
#Лучшая степень для salary: 1  R^2 = 0.1935

# Построение финальной модели с лучшими степенями
model_3 = lm(salary ~ sex + wed1 + wed2 + higher_educ + I(age^1.2) + city_status + I(dur^0.9), data = data2)
summary(model_3) # R^2 = 0.3155 
vif(model_3) #vif < 2 кроме web1,web2 < 3.373182

# task 3
model3_2 = lm(salary ~ sex + I(age^1.2) + city_status + dur + higher_educ, data = data2)
summary(model3_2)
vif(model3_2)
# Лучшая модель: model3_2 - это улучшенная model_3, я убрал из нее незначемые
# переменные и R^2 = 0.2421, vif<1.1. Данная модель объясняет зависимость
# з.п от возраста, пола, места жительства и длительности рабочей недели

#task 4
summary(model3_2) #103 - количество наблюдений
# Residual standard error: 0.846 on 97 degrees of freedom
# (78 пропущенных наблюдений удалены)
# df = Число наблюдений - (кол-во параметров в модели + свободный член) =
# = 103 - 5 - 1 = 97
# Рассчитаем t-критерий (критерий Стьюдента) с помощью команды qt.
# Критическое значение t-критерия (p = 95%)
t_critical = qt(0.975, df = 97)
t_critical #t-критерий:  1.984723

model_41 = lm(salary ~ sex, data = data2)
model_42 = lm(salary ~ wed1, data = data2)
model_43 = lm(salary ~ wed2, data = data2)
model_44 = lm(salary ~ higher_educ, data = data2)
model_45 = lm(salary ~ age, data = data2)
model_46 = lm(salary ~ city_status, data = data2)
model_47 = lm(salary ~ dur, data = data2)

summary(model_41)
confint(model_41)#R^2 = 0.04604 p-value 0.00216 (**) - взаимосвязь есть, Коэффициент (b): -0.20109
# Estimate = 0.45498 - связь положительная [0.1665104  0.743440850]- 0 не входит в интервал =>
# отвергнуть статистическую гипотезу о том, что коэффициент равен 0 МОЖНО
# ВЫВОД: Мужчины получают зарплату выше

summary(model_42)
confint(model_42)#R^2 = -0.002963 p-value 0.495 - взаимосвязи нет Коэффициент (b):-0.0501

summary(model_43)#R^2 = 0.0481 p-value 0.00175 ** - взаимосвязь есть, Коэффициент (b):-0.18840
confint(model_43)#Estimate = -0.5713 - связь отрицательная [-0.387, -0.069]- 0 не входит в интервал =>
# отвергнуть статистическую гипотезу о том, что коэффициент равен 0 МОЖНО
# ВЫВОД: Разведенные/вдовцы получают зарплату ниже

summary(model_44)#R^2 = 0.05695 p-value 0.000711 *** - взаимосвязь есть, Коэффициент (b):-0.18840
confint(model_41)#Estimate = 0.51668 - связь положительная [-0.92619618 -0.2164906] - 0 не входит в интервал =>
# отвергнуть статистическую гипотезу о том, что коэффициент равен 0 МОЖНО
# ВЫВОД: Люди с высшим образованием получают зарплату выше

summary(model_45)#R^2 = 0.002651  p-value 0.2256 - взаимосвязи практически нет Коэффициент (b): 7.726e-17
confint(model_45)#[-0.2373963 0.05637648] Вывод: Возраст в исходной форме не оказывает значимого влияния на зарплату. Доверительный интервал содержит 0.

summary(model_46)#R^2 = -0.002654 p-value 0.4703 - взаимосвязи нет Коэффициент (b):-0.0737

summary(model_47)#R^2 = 0.05905 p-value: 0.0005737*** - взаимосвязь есть, Коэффициент (b):8.682e-17
confint(model_47)#Estimate = 2.535e-01 - связь положительная   [0.110861 0.3962064]- 0 не входит в интервал =>
# отвергнуть статистическую гипотезу о том, что коэффициент равен 0 МОЖНО
# ВЫВОД: Чем дольше рабочий день, тем выше зарплата

# task 5

# Подмножество 1: Мужчины, женатые, с высшим образованием
subset1 <- subset(data2, 
                  sex == 1 &          # Мужчины
                    wed1 == 1 &         # Женатые
                    higher_educ == 1)   # С высшим образованием
# Проверяем лучшую модель (model_2) т.к model3_2 не подходит под наше условие
# Исключаю sex, wed1, higher_educ т.к. константы
model_52 = lm(salary ~wed2  + log(age + num1) + city_status + log(dur + num2), data = subset1)
summary(model_52)
#R^2 = -0.2638  связь отрицательная p-value: 0.9608 - слишком высокое значение
# Доверительные интервалы для коэффициентов
conf_int <- confint(model_52)
print(conf_int)
#Вывод: на з.п данной подгруппы не влияют исследуемые переменные 

# Подмножество 2: Мужчины, живущие в городе, разведённые
subset2 <- subset(data2,
                  sex == 1 &          # Мужчины
                    city_status == 1 &  # Город
                    wed2 == 1)          # Разведённые

model_53 =lm(salary ~  wed1 + higher_educ + log(age + num1) +
                log(dur + num2), data = subset2)
summary(model_53) #R^2 = 0.7309 - связь очень хорошая p-value: 0.08034 - 
# показатель низкий
#Вывод: на з.п данной подгруппы влияют такие критерии, как наличие высшего обр
#возраст и длительность рабочей недели.Не влияет семейное положение

  
  