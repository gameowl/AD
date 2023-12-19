# Instalare și încărcare pachet ggplot2
library(ggplot2)
library(dplyr)
library(corrplot)
library(fmsb)

# Citirea setului de date COVID-19 Dataset
dataset <- read.csv("C:/Users/cuzia/OneDrive/Desktop/Anul III/AD/CovidData.csv")
glimpse(dataset)
colnames(dataset)

# 1. Histograma Vârstei Pacienților
ggplot(dataset, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histograma Vârstei Pacienților", x = "Vârstă", y = "Frecvență")

# 2. Diagramă de Bare pentru Sexul Pacienților
ggplot(dataset, aes(x = factor(SEX, labels = c("Femei", "Bărbați")), fill = factor(SEX))) +
  geom_bar() +
  labs(title = "Distribuția Sexului Pacienților", x = "Sex", y = "Număr de Pacienți") +
  scale_fill_manual(values = c("1" = "pink", "2" = "blue")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Sex"))

# 3. Diagramă de Bare pentru Tipul de Îngrijire
ggplot(dataset, aes(x = factor(PATIENT_TYPE, labels = c("Domiciliu", "Internat")), fill = factor(PATIENT_TYPE))) +
  geom_bar() +
  labs(title = "Distribuția Tipului de Îngrijire", x = "Tipul Îngrijirii", y = "Număr de Pacienți") +
  scale_fill_manual(values = c("1" = "green", "2" = "orange")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Tipul Îngrijirii"))

# 4. Diagramă de Bare pentru Rezultatele Testelor COVID-19
ggplot(dataset, aes(x = factor(CLASIFFICATION_FINAL), fill = factor(CLASIFFICATION_FINAL))) +
  geom_bar() +
  labs(title = "Distribuția Rezultatelor Testelor COVID-19", x = "Rezultatul Testelor", y = "Număr de Pacienți") +
  scale_fill_manual(name = "Rezultatul Testelor", values = c("1" = "red", "2" = "yellow", "3" = "orange", "4" = "green"))


# 5. Box Plot pentru Vârste în Funcție de Tipul de Îngrijire
ggplot(dataset, aes(x = factor(PATIENT_TYPE), y = AGE, fill = factor(PATIENT_TYPE))) +
  geom_boxplot() +
  labs(title = "Variabilitatea Vârstelor în Funcție de Tipul de Îngrijire", x = "Tip de Îngrijire", y = "Vârstă") +
  scale_fill_manual(values = c("1" = "green", "2" = "orange"))

# 6. Matrice de Corelație pentru Variabile Numerice
correlation_matrix <- cor(dataset[, c("AGE", "INTUBED", "ICU")])
corrplot(correlation_matrix, method = "color")

# 7. Heatmap pentru Variabilele de Sănătate
subset_health_vars <- dataset[, c("PNEUMONIA", "DIABETES", "COPD", "ASTHMA", "INMSUPR", "HYPERTENSION", "CARDIOVASCULAR", "RENAL_CHRONIC", "OTHER_DISEASE", "OBESITY", "TOBACCO")]
health_data <- data.frame(t(subset_health_vars))
colnames(health_data) <- c("PNEUMONIA", "DIABETES", "COPD", "ASTHMA", "INMSUPR", "HYPERTENSION", "CARDIOVASCULAR", "RENAL_CHRONIC", "OTHER_DISEASE", "OBESITY", "TOBACCO")
health_data_scaled <- data.frame(scale(health_data))
radarchart(health_data_scaled)

# 8. Scatter Plot pentru Vârstă și Rezultatele Testelor COVID-19
ggplot(dataset, aes(x = AGE, y = CLASSIFICATION, color = factor(CLASSIFICATION))) +
  geom_point() +
  labs(title = "Scatter Plot: Vârstă vs. Rezultatele Testelor COVID-19", x = "Vârstă", y = "Clasificare") +
  scale_color_manual(values = c("1" = "red", "2" = "yellow", "3" = "orange", "4" = "green"))

# 9. Bar Plot pentru Instituțiile Medicale
ggplot(dataset, aes(x = MEDICAL_UNIT)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribuția Pacienților în Instituțiile Medicale", x = "Unitate Medicală", y = "Număr de Pacienți")

# 10. Pie Chart pentru Pacienții Decedați vs. Cei În Viață
ggplot(dataset, aes(x = "", fill = factor(ifelse(DATE_DIED == "9999-99-99", "In Viață", "Decedat")))) +
  geom_bar(stat = "count", width = 1) +
  labs(title = "Procent Pacienți Decedați vs. Cei În Viață", fill = "Clasificare") +
  coord_polar(theta = "y")

