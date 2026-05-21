library(data.table)

# Lataa .RData-tiedosto
load("~/Documents/Gradu_paikallinen/hunter_code/enlargement.discourse.replication.EUP.July25.RData")

# Tarkista, että objekti löytyy
ls()

# Tallenna muuttujaan
df <- ep.enlargement

# Tarkista rakenne
str(df)

# Talenna CSV
fwrite(
  df,
  file = "ep_enlargement.csv",
  sep = ",",
  quote = TRUE,
  na = "",
  logical01 = FALSE
)