
library(tidyverse)
load("figure2.rds")

year1 <- 2023
year2 <- 2024

year1p <- paste0(year1)
year2p <- paste0(year2)

donnees <- figure2 %>%
  filter(year >= year1) %>%
  select(-year) %>%
  t() 

colnames(donnees) <- c(year1p, year2p)

donnees

table1 <- tibble(`% ou Md€` = c("% du PIB", 
                                "Md€"),
                 `Dette YYYY1` = c(donnees["dette_PIB", year1p]/100,
                                  donnees["PIB", year1p]*donnees["dette_PIB", year1p]/100),
                 `Dette YYYY2` = c(donnees["dette_PIB", year2p]/100,
                                  donnees["PIB", year2p]*donnees["dette_PIB", year2p]/100),
                 `Déficit YYYY2` = c(-donnees["deficit_PIB", year2p]/100,
                                    -donnees["PIB", year2p]*donnees["deficit_PIB", year2p]/100),
                 `Taxe inflationniste` = c(-donnees["taxe_inflationniste_PIB", year2p]/100,
                                           -donnees["taxe_inflationniste", year2p]),
                 `Effet croissance` = c(-donnees["croissance_reelle", year2p]*donnees["dette_PIB", year1p]/10000,
                                        -donnees["croissance_reelle", year2p]*donnees["dette_PIB", year1p]/10000*donnees["PIB", year2p]),
                 `Charge d'intérêts` = c(donnees["charge_interets", year2p]/donnees["PIB", year2p],
                                         donnees["charge_interets", year2p]),
                 `Charge d'intérêts réelle` = c(donnees["charge_interets", year2p]/donnees["PIB", year2p]-donnees["taxe_inflationniste_PIB", year2p]/100,
                                                donnees["charge_interets", year2p] - donnees["taxe_inflationniste", year2p])) |>
  gt::gt() |>
  gt::fmt_percent(
    rows = 1,
    decimals = 1
  ) |>
  gt::fmt_number(
    rows = 2,
    decimals = 0
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    `% ou Md€` = gt::html(""),
    `Dette YYYY1` = gt::html(paste0("Dette<br>", year1)),
    `Dette YYYY2` = gt::html(paste0("Dette<br>", year2)),
    `Déficit YYYY2` = gt::html(paste0("Déficit<br>", year2)),
    `Taxe inflationniste` = gt::html("Taxe<br>inflationniste"),
    `Effet croissance` = gt::html("Effet<br>croissance")
  ) |>
  gt::tab_footnote("Source: Insee, calculs de l'auteur")


table1

table1  |>
  gt::gtsave(filename = "table1.png")

table1  |>
  gt::gtsave(filename = "table1.pdf")

system("pdfcrop table1.pdf table1.pdf")
