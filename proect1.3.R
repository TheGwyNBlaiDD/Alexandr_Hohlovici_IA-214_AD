# Установка необходимых пакетов, если они не установлены
if (!require(tidyverse)) install.packages("tidyverse")
library("tidyverse")

# Загрузка данных
file_path <- "E:\\university\\AD\\project\\train-data.csv"
data <- read.csv(file_path, stringsAsFactors = TRUE)
summary(data)
head(data)
data <- data %>% select(Year, Kilometers_Driven, Fuel_Type, Transmission, Owner_Type, Mileage, Engine, Power, Seats, Price)
head(data)

# Преобразование столбцов Mileage, Engine и Power в числовой формат
data$Mileage <- as.numeric(gsub(" km/kg| kmpl", "", data$Mileage))
data$Engine <- as.numeric(gsub(" CC", "", data$Engine))
data$Power <- as.numeric(gsub(" bhp", "", data$Power))
head(data)

#EDA

ggplot(data = data, aes(x = Price)) +
  geom_histogram()

# Гистограмма: Цена-Год
ggplot(data, aes(x = Year, y = Price)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Год", x = "Год", y = "Цена")

# Гистограмма: Цена-Коробка передач
ggplot(data, aes(x = Transmission, y = Price)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Коробка передач", x = "Коробка передач", y = "Цена")

# Гистограмма: Цена-Мощность
ggplot(data, aes(x = Power, y = Price)) +
  geom_point() +
  labs(title = "Гистограмма: Цена-Мощность", x = "Мощность", y = "Цена")

# Гистограмма: Цена-Расход
ggplot(data, aes(x = Mileage, y = Price)) +
  geom_point() +
  labs(title = "Гистограмма: Цена-Расход", x = "Расход", y = "Цена")

# Гистограмма: Цена-Владелец
ggplot(data, aes(x = Owner_Type, y = Price)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Владелец", x = "Владелец", y = "Цена")

# Гистограмма: Цена-Тип топлива
ggplot(data, aes(x = Fuel_Type, y = Price)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Тип топлива", x = "Тип топлива", y = "Цена")

# Гистограмма: Цена-Пробег
ggplot(data, aes(x = Kilometers_Driven, y = Price)) +
  geom_point() +
  labs(title = "Гистограмма: Цена-Пробег", x = "Пробег", y = "Цена")

#сделать визуализацию от цены к другим переменым и в модели поиграть с перемеными .
#values_to_remove <- c("Electric", "LPG", "CNG")
filtered_data <- data %>% filter(Fuel_Type != "Electric")
set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(filtered_data), replace=TRUE, prob=c(0.7,0.3))
train_data  <- filtered_data[sample, ]
test_data   <- filtered_data[!sample, ]
head(train_data)
head(test_data)

# Построение модели линейной регрессии
model <- lm(Price ~ ., data = train_data)
summary(model)

# Расчет R-квадрат
rsquared <- summary(model)$r.squared
cat("R-квадрат:", rsquared, "\n")

#Предсказываение цены
predictions <- predict(model, newdata = test_data)
print(predictions)

# Создаем график с использованием ggplot2
ggplot(data = test_data, aes(x = Price, y = predictions)) +
  geom_point(colour = "blue", shape = 10) +  # Точки для предсказаний
  geom_point(aes(y = Price), colour = "red", shape = 10) +  # Точки для фактических значений
  geom_abline(intercept = 0, slope = 1, colour = "green", linewidth = 1) +  # Линия идеального соответствия
  ggtitle("Predictions vs. Actual Values") +  # Заголовок графика
  xlab("Actual Values") +  # Подпись оси X
  ylab("Predicted Values") +  # Подпись оси Y
  theme_minimal() +  # Применяем минималистичный стиль оформления
  # Добавляем легенду
  scale_colour_manual(
    name = "Legend",
    values = c("blue", "red", "green"),
    labels = c("Predicted", "Actual", "Perfect Fit")
  )
