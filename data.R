library(tidyverse)
library(readxl)

skip <- list(
  T_1101 = 3,
  T_1102 = 3,
  T_1103 = 3,
  T_3101 = 3,
  t_3106 = 1,
  T_7301 = 3
)

t_3101_line_dette <- 3
t_3101_line_dette_PIB <- 11

t_3106_line_deficit <- 2
t_3106_line_deficit_PIB <- 10

url_insee <- "https://www.insee.fr/fr/statistiques/fichier/"

# Version - Comptes de la Nation 2024 -------
# https://www.insee.fr/fr/statistiques/8574657?sommaire=8574832
# https://www.insee.fr/fr/statistiques/fichier/8574657/T_1101_1103.xlsx
# https://www.insee.fr/fr/statistiques/fichier/8574703/t_3101.xlsx
# https://www.insee.fr/fr/statistiques/fichier/8574693/T_7301.xlsx

version1 <- "8574657"
version2 <- "8574703"
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

T_1101 <- read_excel(temp, sheet = "T_1101 en niveau", skip = skip$T_1101) |>
  tidy1()

PIB <- T_1101 |>
  filter(variable == "B1GQ") |>
  select(year, PIB = value)

unlink(temp)

# Croissance réelle du PIB ---------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version1, "/T_1101_1103.xlsx"),
                    temp)

T_1102 <- read_excel(temp, sheet = "T_1102 en évolution", skip = skip$T_1102) |>
  tidy1()

croissance_reelle <- T_1102 |>
  filter(variable == "B1GQ") |>
  select(year, croissance_reelle = value)

unlink(temp)

# Deflateur du PIB ---------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version1, "/T_1101_1103.xlsx"),
                    temp)

T_1103 <- read_excel(temp, sheet = "T_1103 en évolution", skip = skip$T_1103) |>
  tidy1()

deflateur <- T_1103 |>
  filter(variable == "B1GQ") |>
  select(year, deflateur = value)

unlink(temp)

# Dette en % du PIB --------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version2, "/t_3101.xlsx"),
                    temp)

t_3101 <- read_excel(temp, sheet = "T_3101", skip = skip$T_3101) |>
  tidy2()

dette <- t_3101 |>
  filter(line == t_3101_line_dette) |>
  select(year, dette = value)

dette_PIB <- t_3101 |>
  filter(line == t_3101_line_dette_PIB) |>
  select(year, dette_PIB = value)

unlink(temp)

# Deficit en % du PIB --------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version2, "/t_3106.xlsx"),
                    temp)

t_3106 <- read_excel(temp, skip = skip$t_3106) |>
  tidy2()

deficit <- t_3106 |>
  filter(line == t_3106_line_deficit) |>
  select(year, deficit = value)

deficit_PIB <- t_3106 |>
  filter(line == t_3106_line_deficit_PIB) |>
  select(year, deficit_PIB = value)

unlink(temp)

# Charge d'intérêt ----------

temp <- tempfile()

curl::curl_download(paste0(url_insee, version3, "/T_7301.xlsx"),
                    temp)

# change maybe here -----
T_7301 <- read_excel(temp, skip = skip$T_7301, sheet = 2) |>
  tidy1()

charge_interets <- T_7301 |>
  filter(line %in% c(56, 47)) |>
  select(year, line, value) |>
  spread(line, value) |>
  transmute(year, charge_interets = `56` - `47`)

unlink(temp)
rm(temp)

# Données ----------


data <- PIB |>
  full_join(deflateur, by = "year") |>
  full_join(croissance_reelle, by = "year") |>
  full_join(dette, by = "year") |>
  full_join(dette_PIB, by = "year") |>
  full_join(deficit, by = "year") |>
  full_join(deficit_PIB, by = "year") |>
  full_join(charge_interets, by = "year") |>
  mutate(taxe_inflationniste_PIB = dette_PIB*deflateur/100,
         taxe_inflationniste = taxe_inflationniste_PIB*PIB/100) |>
  filter(year >= 1978)

save(data, file = "data.rds")
write_csv(data, file = "data.csv")