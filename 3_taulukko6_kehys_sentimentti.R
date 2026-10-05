# ==============================================================
#  Taulukko 5: Kehysten käyttö ja sävy ennen ja jälkeen 24.2.2022
#
#  Itsenäinen skripti. Tuottaa saman taulukon
#  kolmessa muodossa: lukumäärät, osuudet ja yhdistetty n (%).
# ==============================================================

library(readr)
library(dplyr)
library(tidyr)

#  Asetukset

polku_data <- "xxx/llm_results_new_dataset_2105.csv"
polku_ulos <- "xxx/gradu"

cutoff <- as.Date("2022-02-24")

kehykset <- c("geopolitical", "economic", "normative",
              "institutional", "procedural")

kehys_nimet <- c(
  geopolitical  = "Geopoliittinen",
  economic      = "Taloudellinen",
  normative     = "Normatiivinen",
  institutional = "Institutionaalinen",
  procedural    = "Proseduraalinen"
)

# Sarakkeiden järjestys taulukossa
sentimentti_tasot <- c("Kannatus", "Ehdollinen kannatus", "Vastustus")


#  1. Luku ja duplikaattien poisto

df_raaka <- read_csv(polku_data, show_col_types = FALSE)

df <- df_raaka %>% distinct(Quote, .keep_all = TRUE)

cat("=== Aineisto ===\n")
cat(sprintf("Rivejä luettaessa          : %d\n", nrow(df_raaka)))
cat(sprintf("Rivejä duplikaattien pois. : %d  (poistettu %d)\n",
            nrow(df), nrow(df_raaka) - nrow(df)))

#  2. Periodi ja sentimentti

df <- df %>%
  mutate(
    Date    = as.Date(Date),
    periodi = factor(if_else(Date < cutoff, "Ennen", "Jälkeen"),
                     levels = c("Ennen", "Jälkeen")),
    sentimentti = factor(
      case_when(
        Pos_Reg == 3 ~ "Kannatus",
        Pos_Reg == 2 ~ "Ehdollinen kannatus",
        Pos_Reg == 1 ~ "Vastustus",
        TRUE         ~ NA_character_
      ),
      levels = sentimentti_tasot
    )
  )

# Tarkistus 1: vastaako päivämäärärajaus aineiston omaa
# postinvasion-lippua? Jos ei, jompikumpi on väärin.
if ("postinvasion" %in% names(df)) {
  eri <- sum((df$periodi == "Jälkeen") != (df$postinvasion == 1), na.rm = TRUE)
  cat(sprintf("Periodi vs. postinvasion   : %s\n",
              if (eri == 0) "täsmää" else paste(eri, "ristiriitaa (TARKISTA)")))
}

# Tarkistus 2: puuttuvat sentimenttiarvot päätyisivät omaksi
# sarakkeekseen, joten ne raportoidaan erikseen.
n_na <- sum(is.na(df$sentimentti))
cat(sprintf("Puuttuvia sentimenttiarvoja: %d%s\n", n_na,
            if (n_na > 0) "  <- päätetään erikseen, suodatetaanko" else ""))


#  3. Pitkä muoto: yksi rivi per kehysmerkintä

df_long <- df %>%
  select(periodi, sentimentti, all_of(kehykset)) %>%
  pivot_longer(
    cols      = all_of(kehykset),
    names_to  = "kehys",
    values_to = "arvo"
  ) %>%
  filter(arvo == TRUE, !is.na(sentimentti)) %>%
  mutate(kehys = factor(recode(kehys, !!!kehys_nimet),
                        levels = unname(kehys_nimet)))

# Huom. yksi puheenvuoro voi saada useamman kehyksen, joten
# rivien määrä tässä on suurempi kuin puheenvuorojen määrä.
cat(sprintf("Kehysmerkintöjä yhteensä   : %d (puheenvuoroja %d)\n\n",
            nrow(df_long), nrow(df)))


#  4. Lukumäärät ja osuudet

df_laskuri <- df_long %>%
  count(periodi, kehys, sentimentti, name = "lkm", .drop = FALSE) %>%
  group_by(periodi, kehys) %>%
  # Nimittäjä on kehyksen ja periodin sisäinen kehysmerkintöjen määrä.
  mutate(osuus = if (sum(lkm) > 0) lkm / sum(lkm) else NA_real_) %>%
  ungroup()

pros <- function(x, tarkkuus = 1) {
  ifelse(is.na(x), "-",
         paste0(formatC(100 * x, format = "f", digits = tarkkuus - 1,
                        decimal.mark = ","), " %"))
}


# --- 4a. Lukumäärätaulukko -------------------------------------
taulukko_lkm <- df_laskuri %>%
  select(periodi, kehys, sentimentti, lkm) %>%
  pivot_wider(names_from = sentimentti, values_from = lkm, values_fill = 0) %>%
  mutate(Yhteensä = rowSums(across(all_of(sentimentti_tasot)))) %>%
  arrange(kehys, periodi)


# --- 4b. Osuustaulukko -----------------------------------------
taulukko_osuus <- df_laskuri %>%
  mutate(osuus_txt = pros(osuus)) %>%
  select(periodi, kehys, sentimentti, osuus_txt) %>%
  pivot_wider(names_from = sentimentti, values_from = osuus_txt,
              values_fill = "0 %") %>%
  arrange(kehys, periodi)


# --- 4c. Yhdistetty n (%) -------------
taulukko_yhd <- df_laskuri %>%
  mutate(solu = paste0(lkm, " (", pros(osuus), ")")) %>%
  select(periodi, kehys, sentimentti, solu) %>%
  pivot_wider(names_from = sentimentti, values_from = solu,
              values_fill = "0 (0 %)") %>%
  left_join(
    df_laskuri %>%
      group_by(periodi, kehys) %>%
      summarise(Yhteensä = sum(lkm), .groups = "drop"),
    by = c("periodi", "kehys")
  ) %>%
  arrange(kehys, periodi)


#  5. Tulostus

cat("=== Taulukko 5: lukumäärät ===\n")
print(as.data.frame(taulukko_lkm), row.names = FALSE)

cat("\n=== Taulukko 5: osuudet (riveittäin) ===\n")
print(as.data.frame(taulukko_osuus), row.names = FALSE)

cat("\n=== Taulukko 5: n (%) — gradun muoto ===\n")
print(as.data.frame(taulukko_yhd), row.names = FALSE)

# Tarkistus 3: sarakesummien pitää täsmätä pitkän muodon riveihin
cat(sprintf("\nTarkistus: lukumäärien summa %d = kehysmerkintöjä %d -> %s\n",
            sum(taulukko_lkm$Yhteensä), nrow(df_long),
            if (sum(taulukko_lkm$Yhteensä) == nrow(df_long)) "OK" else "EI TÄSMÄÄ"))

#  6. Tallennus

write_excel_csv(taulukko_lkm,   file.path(polku_ulos, "taulukko5_lkm.csv"))
write_excel_csv(taulukko_osuus, file.path(polku_ulos, "taulukko5_osuudet.csv"))
write_excel_csv(taulukko_yhd,   file.path(polku_ulos, "taulukko5_n_ja_osuus.csv"))

cat("\nTallennettu kansioon:", polku_ulos, "\n")
