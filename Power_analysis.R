# Load packages----
library(readxl)
library(tidyverse)
library(ggrain)
library(ggpubr)
#devtools::install_github("jtlandis/ggside")
library(ggside)
library(scales)

# Set WD----
setwd("Pilot data")

# Load data----
db <- read_excel("Base de datos_FIN.xlsx") |>
  mutate(Sexo = recode(Sexo,
                       "1" = "Men",
                       "2" = "Women"),
         Canal = recode(Canal,
                        "Rostro" = "Face",
                        "Voz" = "Voice",
                        "Voz + rostro" = "Voice + Face")) |>
  rename("ID" = "ID Participante",
         "Age" = "Edad",
         "Gender" = "Sexo",
         "Channel" = "Canal",
         "Stimulus" = "Estimulo",
         "Reported_sociosexuality" = "SOI",
         "Rated_sociosexuality" = "Calificacion") |> 
  mutate(across(where(is.character), as.factor)) |> 
  mutate(Stimulus_sex = ifelse(grepl("H", Stimulus), "Male stimuli", "Female stimuli")) |> 
  select(ID:Stimulus, Stimulus_sex, Reported_sociosexuality:Rated_sociosexuality)

# Descriptives----

## Participant age----
ID_age_desc <- db |> 
  group_by(Gender, ID) |> 
  summarise(Age = mean(Age, na.rm = TRUE)) |> 
  group_by(Gender)

ID_age_desc |> 
  summarise(n = n(),
            Mean = mean(Age),
            SD = sd(Age),
            Median = median(Age),
            MAD = mad(Age),
            Min = min(Age),
            Max = max(Age))

ID_age_desc |> 
  ggplot(aes(x = Gender, y = Age, fill = Gender)) +
  geom_rain(alpha = .5) +
  guides(fill = 'none', color = 'none') +
  coord_flip() +
  scale_y_continuous(breaks = breaks_width(1)) +
  theme_pubclean()

## Reported sociosexuality----
St_ss_desc <- db |> 
  group_by(Stimulus_sex, Stimulus) |> 
  summarise(Reported_sociosexuality = mean(Reported_sociosexuality, na.rm = TRUE)) |> 
  group_by(Stimulus_sex)

St_ss_desc |> 
  summarise(n = n(),
            Mean = mean(Reported_sociosexuality),
            SD = sd(Reported_sociosexuality),
            Median = median(Reported_sociosexuality),
            MAD = mad(Reported_sociosexuality),
            Min = min(Reported_sociosexuality),
            Max = max(Reported_sociosexuality))

St_ss_desc |> 
  ggplot(aes(x = Stimulus_sex, y = Reported_sociosexuality, fill = Stimulus_sex)) +
  geom_rain(alpha = .5) +
  guides(fill = 'none', color = 'none') +
  labs(x = "Stimulus sex", y = "Reported sociosexuality") +
  scale_y_continuous(breaks = breaks_width(1)) +
  theme_pubclean()

## Rated sociosexuality----
ID_ss_desc <- db |> 
  group_by(Gender, Stimulus_sex, ID) |> 
  summarise(Rated_sociosexuality = mean(Rated_sociosexuality, na.rm = TRUE)) |> 
  group_by(Gender, Stimulus_sex)

ID_ss_desc |> 
  group_by(Gender, Stimulus_sex) |> 
  summarise(n = n(),
            Mean = mean(Rated_sociosexuality),
            SD = sd(Rated_sociosexuality),
            Median = median(Rated_sociosexuality),
            MAD = mad(Rated_sociosexuality),
            Min = min(Rated_sociosexuality),
            Max = max(Rated_sociosexuality))

ID_ss_desc |> 
  ggplot(aes(x = Gender, y = Rated_sociosexuality, fill = Stimulus_sex)) +
  geom_rain(alpha = .5) +
  labs(x = "Participant gender", fill = "Stimulus sex", y = "Rated sociosexuality") +
  scale_y_continuous(breaks = breaks_width(1)) +
  theme_pubclean()

## Correlation between reported and rates sociosexuality
db |> 
  ggplot(aes(x = Reported_sociosexuality, y = Rated_sociosexuality, colour = Gender)) +
  geom_point(alpha = 0.01) +
  geom_smooth(method = "lm", colour = "black") +
  guides(colour = 'none') +
  labs(x = "Reported sociosexuality", y = "Rated sociosexuality") +
  facet_grid(Stimulus_sex ~ Gender) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(breaks = breaks_width(1)) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, cor.coef.name = "tau", colour = "black") +
  stat_regline_equation(label.y = 8.5, colour = "black", size = 3)


db |> 
  ggplot(aes(x = Reported_sociosexuality, y = Rated_sociosexuality, colour = Gender)) +
  geom_hex(bins = 10, color = "white") +
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_xsidedensity(aes(y = after_stat(density)), colour = "grey60", fill = "grey60") +
  geom_ysidedensity(aes(x = after_stat(density)), colour = "grey60", fill = "grey60") +
  geom_smooth(method = "lm", colour = "black") +
  guides(colour = 'none') +
  labs(x = "Reported sociosexuality", y = "Rated sociosexuality", fill = "Count") +
  facet_grid(Stimulus_sex ~ Gender) +
  #scale_x_continuous(breaks = breaks_width(1)) +
  #scale_y_continuous(breaks = breaks_width(1)) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, cor.coef.name = "tau", colour = "black", fill = "green") +
  stat_regline_equation(label.y = 8.5, colour = "black", size = 3)
