library(ggplot2)

# Načtení dat z CSV souboru do proměnné df
df <- read.csv("data/GOODS1n.csv")

# Základní informace o struktuře dat
str(df)

# Všechny možné kategorie produktů
unique(df$Class)

# Základní statistika pro jednotlivé sloupce
summary(df)

# Kontrola chybějících hodnot
colSums(is.na(df))

# Sloupcový graf pro jednotlivé kategorie produktů
barplot(
  table(df$Class),
  main = "Zastoupení jednotlivých kategorií produktů",
  xlab = "Kategorie produktu",
  ylab = "Počet produktů",
  col = c("red", "blue", "green", "yellow"),
  names.arg = c("Confection", "Drink", "Luxury", "Meat")
)

# Cena produktu
ggplot(df, aes(x = Cost)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "blue",
    color = "black",
    bins = 20
  ) +
  geom_density(color = "darkblue", linewidth = 1) +
  labs(title = "Cena produktu", x = "Cena", y = "Hustota")

# Výše investice do marketingu
ggplot(df, aes(x = Promotion)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "red",
    color = "black",
    bins = 30
  ) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "Výše investice do marketingu", x = "Investice", y = "Hustota")

# Zisk před marketingovou kampaní
ggplot(df, aes(x = Before)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "green",
    color = "black",
    bins = 30
  ) +
  geom_density(color = "darkgreen", linewidth = 1) +
  labs(title = "Zisk před marketingovou kampaní", x = "Zisk", y = "Hustota")

# Zisk po marketingové kampani
ggplot(df, aes(x = After)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "yellow",
    color = "black",
    bins = 30
  ) +
  geom_density(color = "orange", linewidth = 1) +
  labs(title = "Zisk po marketingové kampani", x = "Zisk", y = "Hustota")

# Korelační matice
cor(df[, c("Cost", "Promotion", "Before", "After")])

# Přidání nového sloupce do datového rámce
df$RevenueIncrease <- (df$After - df$Before) / df$Before * 100

# Scatter plot pro jednotlivé sloupce
ggplot(df) +
  geom_point(aes(
    x = Promotion,
    y = RevenueIncrease,
    color = Class
  )) +
  labs(
    title = "Scatter plot pro jednotlivé sloupce",
    x = "Výše investice do marketingu",
    y = "Procentuální navýšení zisku",
    color = "Kategorie produktu"
  )

# Derivace nových proměnných pro každou z kategorií produktů
df$Class_Confection <- ifelse(df$Class == "Confection", 1, 0)
df$Class_Drink <- ifelse(df$Class == "Drink", 1, 0)
df$Class_Luxury <- ifelse(df$Class == "Luxury", 1, 0)
df$Class_Meat <- ifelse(df$Class == "Meat", 1, 0)

# Vynásobení každé z nových proměnných hodnotou Promotion pro daný produkt
df$Class_Confection <- df$Class_Confection * df$Promotion
df$Class_Drink <- df$Class_Drink * df$Promotion
df$Class_Luxury <- df$Class_Luxury * df$Promotion
df$Class_Meat <- df$Class_Meat * df$Promotion

# Odstranění redundantních sloupců
df$Class <- NULL
df$Promotion <- NULL

# Výsledek příznakování
head(df)

# Rozdělení dat na trénovací a testovací sadu
set.seed(123) # pro reprodukovatelnost

train_index <- sample(seq_len(nrow(df)), size = 0.9 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Kontrola rozměrů datových rámců
dim(train_data)
dim(test_data)

# Vytvoření lineárního regresního modelu
model <- lm(
  RevenueIncrease ~
    Cost +
    Class_Confection +
    Class_Drink +
    Class_Luxury +
    Class_Meat,
  data = train_data
)

# Základní informace o modelu
summary(model)

# Použití modelu na testovací sadu
predictions <- predict(model, newdata = test_data)

# Porovnání predikcí s reálnými hodnotami
comparison <- data.frame(
  Actual = test_data$RevenueIncrease,
  Predicted = predictions
)

comparison