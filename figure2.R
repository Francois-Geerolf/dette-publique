library(tidyverse)
library(readxl)

# Version - Comptes de la Nation 2024 -------


T_7301_skip_parameter <- 3
t_3101_line_parameter <- 11

# https://www.insee.fr/fr/statistiques/8574657?sommaire=8574832

url_insee <- "https://www.insee.fr/fr/statistiques/fichier/"

# https://www.insee.fr/fr/statistiques/8068582
# https://www.insee.fr/fr/statistiques/fichier/8574657/T_1101_1103.xlsx
version1 <- "8574657"

# https://www.insee.fr/fr/statistiques/8068622
# https://www.insee.fr/fr/statistiques/fichier/8574703/t_3101.xlsx
version2 <- "8574703"

# https://www.insee.fr/fr/statistiques/8068612
# https://www.insee.fr/fr/statistiques/fichier/8574693/T_7301.xlsx
version3 <- "8574693"

tidy1 <- function(data){
  data |>
    mutate(line = 1:n()) |>
    rename(variable = ...1, Variable = ...2) |>
    gather(year, value, -variable, -Variable, -line) |>
    filter(!is.na(value)) |>
    mutate(year = as.numeric(year))
}

tidy2 <- function(data){
  data |>
    mutate(line = 1:n()) |>
    rename(Variable = ...1) |>
    gather(year, value, -Variable, -line) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value)) |>
    mutate(year = as.numeric(year))
}

# PIB --------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version1, "/T_1101_1103.xlsx"),
                    temp)

T_1101 <- read_excel(temp, sheet = "T_1101 en niveau", skip = 3) |>
  tidy1()

PIB <- T_1101 |>
  filter(variable == "B1GQ") |>
  select(year, PIB = value)

unlink(temp)

# Deflateur du PIB ---------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version1, "/T_1101_1103.xlsx"),
                    temp)

T_1103 <- read_excel(temp, sheet = "T_1103 en évolution", skip = 3) |>
  tidy1()

deflateur <- T_1103 |>
  filter(variable == "B1GQ") |>
  select(year, deflateur = value)

unlink(temp)

# Croissance réelle du PIB ---------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version1, "/T_1101_1103.xlsx"),
                    temp)

T_1102 <- read_excel(temp, sheet = "T_1102 en évolution", skip = 3) |>
  tidy1()

croissance_reelle <- T_1102 |>
  filter(variable == "B1GQ") |>
  select(year, croissance_reelle = value)

unlink(temp)

# Dette en % du PIB --------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version2, "/t_3101.xlsx"),
                    temp)

t_3101 <- read_excel(temp, sheet = "T_3101", skip = 3) |>
  tidy2()

dette_PIB <- t_3101 |>
  filter(line == t_3101_line_parameter) |>
  select(year, dette_PIB = value)

unlink(temp)

# Deficit en % du PIB --------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version2, "/t_3106.xlsx"),
                    temp)

t_3106 <- read_excel(temp, skip = 1) |>
  tidy2()

deficit_PIB <- t_3106 |>
  filter(line == 10) |>
  select(year, deficit_PIB = value)

unlink(temp)

# Charge d'intérêt ----------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version3, "/T_7301.xlsx"),
                    temp)

# change maybe here -----
T_7301 <- read_excel(temp, skip = T_7301_skip_parameter, sheet = 2) |>
  tidy1()

charge_interets <- T_7301 |>
  filter(line %in% c(56, 47)) |>
  select(year, line, value) |>
  spread(line, value) |>
  transmute(year, charge_interets = `56` - `47`)

unlink(temp)
rm(temp)

# Données ----------


figure2 <- PIB |>
  full_join(deflateur, by = "year") |>
  full_join(croissance_reelle, by = "year") |>
  full_join(dette_PIB, by = "year") |>
  full_join(deficit_PIB, by = "year") |>
  full_join(charge_interets, by = "year") |>
  mutate(taxe_inflationniste_PIB = dette_PIB*deflateur/100,
         taxe_inflationniste = taxe_inflationniste_PIB*PIB/100) |>
  filter(year >= 1978)

save(figure2, file = "figure2.rds")
load("figure2.rds")

figure2 |>
  transmute(date = as.Date(paste0(year, "-01-01")),
            `Charge d'intérêts (% du PIB)` = charge_interets/PIB,
            `Charge d'intérêts réelle (% du PIB)` = charge_interets/PIB-taxe_inflationniste/PIB) |>
  gather(variable, value, -date) |>
  ggplot() + geom_line(aes(x = date, y = value, color = variable)) +
  theme_minimal() + xlab("") + ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.2),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_date(breaks = as.Date(paste0(seq(1978, 2100, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-100, 100, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(data = . %>% 
               filter(date == max(date)),
             aes(x = date, y = value,  color = variable, label = scales::percent(value, acc = 0.1))) +
  labs(caption = "Source: Insee, calculs de l'auteur")

save(figure2, file = "figure2.rds")

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)

