#RDS-muunnos:

library(data.table)

# Lue RDS-tiedosto
df <- readRDS("~/Documents/Gradu_paikallinen/Corpus_uus/EUPDCorp_1999-2024_v1.RDS")

# Tarkista rakenne
str(df)

# Kirjoita csv
fwrite(
  df,
  file = "ep_speeches_1999_2024.csv",
  sep = ",",
  quote = TRUE,
  na = "",
  logical01 = FALSE
)
