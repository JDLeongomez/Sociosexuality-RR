# Paquetes ----
library(readxl)
library(dplyr)
library(stringr)
library(lmerTest)
library(interactions)
library(ggplot2)
library(report)
library(performance)

# Cargar base de datos ----
bd <- read_excel("Base de datos.xlsx") |>
  mutate(Sexo = recode(Sexo,
                       "1" = "Hombre",
                       "2" = "Mujer"))

# Estructura base de datos ----  
str(bd)

# Modelo ----
mod <- lmer(Calificacion ~ Canal * SOI * Sexo +
              (1 + SOI | `ID Participante`) +
              (1 | Estimulo),
            data = bd)

bd2 <- bd |> 
  filter(Canal == "Audio")

mod <- lmer(Calificacion ~ SOI * Sexo +
              (1 + SOI | `ID Participante`) +
              (1 | Estimulo),
            data = bd)
summary(mod)
r2_nakagawa(mod)

interact_plot(mod, pred = SOI, modx = Sexo,
              interval = TRUE)

mod <- lmer(Calificacion ~ Canal * SOI * Sexo +
              (1 + SOI | `ID Participante`) +
              (1 | Estimulo),
            data = bd)

# lm = linear model

# lmer = linear mixed-effect regression

# Ajuste y supuestos del modelo ----
check_model(mod)

# Resumen del modelo ----
anova(mod)

# Reporte ----
report(mod, format = "markdown")

# Explorar interacción Canal:SOI:Sexo ----
interact_plot(mod, 
              pred = SOI, 
              modx = Canal,
              mod2 = Sexo,
              interval = TRUE)

ggplot(mod@frame, aes(y = predict(mod), x = SOI, color = Canal)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  
  facet_wrap(~Sexo)

# Paquetes usados (se deben citar) ----
cite_packages(format = "markdown")
