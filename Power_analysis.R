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
dat_st <- read_excel("Orientación Sociosexual.xlsx") |> 
  rename(Stimulus = Codigo,
         Stimulus_age = Edad)

dat <- read_excel("Base de datos_FIN.xlsx") |>
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
  select(ID:Stimulus, Stimulus_sex, Reported_sociosexuality:Rated_sociosexuality) |> 
  left_join(dat_st[, c("Stimulus", "Stimulus_age")], by = "Stimulus") |> 
  select(ID:Stimulus_sex, Stimulus_age, Reported_sociosexuality:Rated_sociosexuality)

# Descriptives----
## Age----
### Participant age----
ID_age_desc <- dat |> 
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

### Stimulus age----
St_age_desc <- dat |> 
  group_by(Stimulus_sex, Stimulus) |> 
  summarise(Age = mean(Stimulus_age, na.rm = TRUE)) |> 
  group_by(Stimulus_sex)

St_age_desc |> 
  summarise(n = n(),
            Mean = mean(Age),
            SD = sd(Age),
            Median = median(Age),
            MAD = mad(Age),
            Min = min(Age),
            Max = max(Age))

### Age plot----
p_age <- ID_age_desc |> 
  rename(cod = ID) |> 
  mutate(role = "Raters") |> 
  bind_rows(St_age_desc |> 
              rename(Gender = Stimulus_sex,
                     cod = Stimulus)) |>
  ggplot(aes(x = Gender, y = Age, fill = Gender)) +
  geom_rain(alpha = .5) +
<<<<<<< HEAD
  stat_summary(fun.y = "mean", aes(color = Gender)) +
=======
  scale_fill_manual(values = rep(c("#00AFBB", "#FC4E07"), times = 2)) +
  scale_colour_manual(values = rep(c("#00AFBB", "#FC4E07"), times = 2)) +
>>>>>>> e87462e22fa7ed35c9c6f7c4c265cc88b0c323da
  guides(fill = 'none', color = 'none') +
  #coord_flip() +
  scale_y_continuous(breaks = breaks_width(2)) +
  theme_pubclean() +
  labs(x = NULL) +
  facet_wrap(~role, scales = "free_x") +
  stat_summary(fun.data = "mean_sd", linewidth = 1, size = 0.5, 
               aes(colour = Gender))
  
## Sociosexuality----    
### Reported sociosexuality----
St_ss_desc <- dat |> 
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

#### Plot----
p_rep_ss <- St_ss_desc |> 
  ggplot(aes(x = Stimulus_sex, y = Reported_sociosexuality, fill = Stimulus_sex)) +
  geom_rain(alpha = .5) +
<<<<<<< HEAD
  stat_summary(fun.y = "mean", aes(color = Stimulus_sex)) +
=======
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) +
  scale_colour_manual(values = c("#00AFBB", "#FC4E07")) +
>>>>>>> e87462e22fa7ed35c9c6f7c4c265cc88b0c323da
  guides(fill = 'none', color = 'none') +
  labs(x = "Stimulus sex", y = "Reported sociosexuality") +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_pubclean() +
  stat_summary(fun.data = "mean_sd", linewidth = 1, size = 0.5, 
               aes(colour = Stimulus_sex))

### Rated sociosexuality----
ID_ss_desc <- dat |> 
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

#### Plot----
p_rat_ss <- ID_ss_desc |> 
  ggplot(aes(x = Gender, y = Rated_sociosexuality, fill = Stimulus_sex)) +
  geom_rain(alpha = .5) +
<<<<<<< HEAD
  stat_summary(fun.y = "mean", aes(color = Stimulus_sex)) +
=======
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) +
  scale_colour_manual(values = c("#00AFBB", "#FC4E07")) +
>>>>>>> e87462e22fa7ed35c9c6f7c4c265cc88b0c323da
  guides(fill = 'none', color = 'none') +
  labs(x = "Participant gender", fill = "Stimulus sex", y = "Rated sociosexuality") +
  scale_y_continuous(breaks = breaks_width(1)) +
  theme_pubclean() +
  stat_summary(fun.data = "mean_sd", 
               linewidth = 1, size = 0.5, 
               aes(colour = Stimulus_sex))

## Combined plot----
ggarrange(p_age,
          ggarrange(p_rep_ss, p_rat_ss,
                    labels = c("", "C")),
          nrow = 2,
          labels = "AUTO")

## Correlation between reported and rates sociosexuality
dat |> 
  ggplot(aes(x = Reported_sociosexuality, y = Rated_sociosexuality, colour = Gender)) +
  geom_point(alpha = 0.05) +
  geom_xsidedensity(aes(y = after_stat(density)), colour = "#00AFBB", fill = "#00AFBB") +
  geom_ysidedensity(aes(x = after_stat(density)), colour = "#FC4E07", fill = "#FC4E07") +
  geom_smooth(method = "lm", colour = "black") +
  scale_colour_manual(values = c("#00AFBB", "#FC4E07")) +
  guides(colour = 'none') +
  labs(x = "Reported sociosexuality", y = "Rated sociosexuality") +
  facet_grid(Stimulus_sex ~ Gender) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(breaks = breaks_width(1)) +
  stat_cor(p.accuracy = 0.001, 
           r.accuracy = 0.01, 
           cor.coef.name = "tau", 
           label.y = 9.8,
           geom = "label", 
           colour = "black", 
           fill = "white",
           alpha = 0.7) +
  stat_regline_equation(label.y = 8.5, 
                        geom = "label", 
                        colour = "black", 
                        fill = "white",
                        alpha = 0.7, 
                        size = 3)

dat |> 
  ggplot(aes(x = Reported_sociosexuality, y = Rated_sociosexuality, colour = Gender)) +
  geom_hex(bins = 10, color = "white") +
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_xsidedensity(aes(y = after_stat(density)), colour = "#00AFBB", fill = "#00AFBB") +
  geom_ysidedensity(aes(x = after_stat(density)), colour = "#00AFBB", fill = "#00AFBB") +
  geom_smooth(method = "lm", colour = "black") +
  guides(colour = 'none') +
  labs(x = "Reported sociosexuality", y = "Rated sociosexuality", fill = "Count") +
  facet_grid(Stimulus_sex ~ Gender) +
  #scale_x_continuous(breaks = breaks_width(1)) +
  #scale_y_continuous(breaks = breaks_width(1)) +
  stat_cor(p.accuracy = 0.001, 
           r.accuracy = 0.01, 
           cor.coef.name = "tau", 
           label.y = 10,
           geom = "label", 
           colour = "black", 
           fill = "white",
           alpha = 0.7) +
  stat_regline_equation(label.y = 8.5, 
                        geom = "label", 
                        colour = "black", 
                        fill = "white",
                        alpha = 0.7, 
                        size = 3)
