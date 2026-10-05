library(openxlsx)
library(dplyr)
library(psych)

polku <- "xxx/Validiointi"

tiedosto_model <- file.path(polku, "random_139_puhetta.xlsx")
tiedosto_human <- file.path(polku, "random_139_puhetta_oma.xlsx")

kehykset    <- c("geopolitical", "economic", "normative",
                 "institutional", "procedural", "none")
paakehykset <- setdiff(kehykset, "none")
tasot       <- c("FALSE", "TRUE")


#  1. Apufunktiot

to_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x == 1)
  x <- toupper(trimws(as.character(x)))
  # Skandit pois vertailusta, jotta koodi toimii myös silloin kun R:n
  # locale ei ole UTF-8 (esim. Rscript palvelimella).
  x <- gsub("\u00C4|\u00E4", "A", x, perl = TRUE)   # Ä/ä -> A
  x <- gsub("\u00D6|\u00F6", "O", x, perl = TRUE)   # Ö/ö -> O
  out <- rep(NA, length(x))
  out[x %in% c("TRUE",  "TOSI",    "T", "1", "KYLLA", "YES")] <- TRUE
  out[x %in% c("FALSE", "EPATOSI", "F", "0", "EI",    "NO")]  <- FALSE
  out
}

norm_key <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u2018\u2019\u201A\u201B]", "'",  x, perl = TRUE)  # heittomerkit
  x <- gsub("[\u201C\u201D\u201E\u201F]", "\"", x, perl = TRUE)  # lainausmerkit
  x <- gsub("[\u00A0\u2007\u202F]",        " ",  x, perl = TRUE)  # sitovat välilyönnit
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014]", "-", x, perl = TRUE)  # viivat
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

# Cohenin kappa 2x2-taulusta suoraan (bootstrapia varten).
kappa_2x2 <- function(m, h) {
  tab <- table(factor(m, levels = tasot), factor(h, levels = tasot))
  n   <- sum(tab)
  if (n == 0) return(NA_real_)
  p_o <- sum(diag(tab)) / n
  p_e <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (isTRUE(all.equal(p_e, 1))) return(NA_real_)
  (p_o - p_e) / (1 - p_e)
}


#  2. Aineistojen luku

df_model <- read.xlsx(tiedosto_model)
df_human <- read.xlsx(tiedosto_human)

# Sarakkeiden olemassaolo varmistetaan ennen kuin mitään lasketaan.
puuttuu_model <- setdiff(c("Quote", kehykset), names(df_model))
puuttuu_human <- setdiff(c("Quote", kehykset), names(df_human))
if (length(puuttuu_model) > 0)
  stop("Mallin tiedostosta puuttuu sarakkeet: ",
       paste(puuttuu_model, collapse = ", "))
if (length(puuttuu_human) > 0)
  stop("Ihmisen tiedostosta puuttuu sarakkeet: ",
       paste(puuttuu_human, collapse = ", "))

df_model <- df_model %>%
  mutate(.key = norm_key(Quote),
         across(all_of(kehykset), to_logical))

df_human <- df_human %>%
  mutate(.key = norm_key(Quote),
         across(all_of(kehykset), to_logical))


#  3. Rivien kohdistus Quote-sarakkeen perusteella

dupl <- unique(c(df_model$.key[duplicated(df_model$.key)],
                 df_human$.key[duplicated(df_human$.key)]))
if (length(dupl) > 0) {
  cat("\nVAROITUS: Quote-sarakkeessa on", length(dupl),
      "kaksoiskappaletta. Esimerkkejä:\n")
  print(substr(head(dupl, 3), 1, 150))
  stop("Liitos ei ole luotettava ilman yksikäsitteistä tunnistetta. ",
       "Lisää aineistoon juokseva ID-sarake tai poista duplikaatit.")
}

vain_mallissa  <- setdiff(df_model$.key, df_human$.key)
vain_ihmisella <- setdiff(df_human$.key, df_model$.key)

cat("\n=== Aineistojen kohdistus ===\n")
cat(sprintf("Rivejä mallin tiedostossa : %d\n",  nrow(df_model)))
cat(sprintf("Rivejä ihmisen tiedostossa: %d\n",  nrow(df_human)))
cat(sprintf("Vain mallilla             : %d\n",  length(vain_mallissa)))
cat(sprintf("Vain ihmisellä            : %d\n",  length(vain_ihmisella)))

if (length(vain_mallissa) > 0) {
  cat("\nTäsmäämättömiä (vain malli), 3 ensimmäistä:\n")
  print(substr(head(vain_mallissa, 3), 1, 150))
}
if (length(vain_ihmisella) > 0) {
  cat("\nTäsmäämättömiä (vain ihminen), 3 ensimmäistä:\n")
  print(substr(head(vain_ihmisella, 3), 1, 150))
}

df <- inner_join(
  # Date otetaan mukaan, jotta jakso-vertailun voi tehdä lopussa.
  df_model %>% select(.key, Quote, any_of("Date"), all_of(kehykset)),
  df_human %>% select(.key, all_of(kehykset)),
  by     = ".key",
  suffix = c("_model", "_human")
)

cat(sprintf("\nYhdistettyjä pareja       : %d\n", nrow(df)))
if (nrow(df) == 0)
  stop("Yhtään riviä ei täsmännyt. Tarkista, että Quote-sarake on ",
       "sama molemmissa tiedostoissa (esim. katkaisu Excelissä).")


#  4. Sisäinen johdonmukaisuus: none = TOSI vain jos mikään
#     muu kehys ei ole TOSI

tarkista_none <- function(d, rooli) {
  muut     <- as.matrix(d[, paste0(paakehykset, "_", rooli)])
  on_muita <- rowSums(muut, na.rm = TRUE) > 0
  none     <- d[[paste0("none_", rooli)]]
  ristiriita <- which((none & on_muita) | (!none & !on_muita))
  cat(sprintf("%-8s ristiriitaisia none-merkintöjä: %d / %d\n",
              rooli, length(ristiriita), nrow(d)))
  ristiriita
}

cat("\n=== none-sarakkeen johdonmukaisuus ===\n")
ristiriidat <- list(
  model = tarkista_none(df, "model"),
  human = tarkista_none(df, "human")
)


#  5. Cohenin kappa kehyksittäin

laske_kappa <- function(f) {
  m_raw <- df[[paste0(f, "_model")]]
  h_raw <- df[[paste0(f, "_human")]]

  kelpaa <- !is.na(m_raw) & !is.na(h_raw)
  m <- factor(as.character(m_raw[kelpaa]), levels = tasot)
  h <- factor(as.character(h_raw[kelpaa]), levels = tasot)

  tab <- table(malli = m, ihminen = h)

  cat("\n---", f, "---\n")
  print(tab)
  if (sum(!kelpaa) > 0)
    cat("Puuttuvia (NA) pareja poistettu:", sum(!kelpaa), "\n")

  p_o   <- sum(diag(tab)) / sum(tab)
  pabak <- 2 * p_o - 1   # prevalenssi- ja bias-korjattu kappa

  k <- tryCatch(psych::cohen.kappa(tab), error = function(e) NULL)

  data.frame(
    kehys         = f,
    n             = sum(tab),
    n_puuttuvaa   = sum(!kelpaa),
    molemmat      = tab["TRUE",  "TRUE"],
    vain_malli    = tab["TRUE",  "FALSE"],
    vain_ihminen  = tab["FALSE", "TRUE"],
    ei_kumpikaan  = tab["FALSE", "FALSE"],
    prev_malli    = mean(m == "TRUE"),
    prev_ihminen  = mean(h == "TRUE"),
    yksimielisyys = p_o,
    kappa    = if (is.null(k)) NA_real_ else as.numeric(k$kappa),
    ci_ala   = if (is.null(k)) NA_real_ else k$confid[1, 1],
    ci_yla   = if (is.null(k)) NA_real_ else k$confid[1, 3],
    pabak    = pabak,
    stringsAsFactors = FALSE
  )
}

cat("\n=== Ristiintaulukot ===\n")
kappa_taulu <- bind_rows(lapply(kehykset, laske_kappa))


#  6. Bootstrap-luottamusvälit

bootstrap_kappa <- function(f, R = 5000, seed = 20260902) {
  set.seed(seed)
  m <- as.character(df[[paste0(f, "_model")]])
  h <- as.character(df[[paste0(f, "_human")]])
  ok <- !is.na(m) & !is.na(h)
  m <- m[ok]; h <- h[ok]
  n <- length(m)

  est <- replicate(R, {
    i <- sample.int(n, n, replace = TRUE)
    kappa_2x2(m[i], h[i])
  })

  q <- quantile(est, c(0.025, 0.975), na.rm = TRUE)
  data.frame(kehys        = f,
             boot_ala     = unname(q[1]),
             boot_yla     = unname(q[2]),
             boot_maarit  = mean(!is.na(est)),  # osuus, jossa kappa määritelty
             stringsAsFactors = FALSE)
}

boot_taulu  <- bind_rows(lapply(kehykset, bootstrap_kappa))
kappa_taulu <- left_join(kappa_taulu, boot_taulu, by = "kehys")

#  7. Tulostaulukko

kappa_esitys <- kappa_taulu %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n=== Cohenin kappa kehyksittäin ===\n")
print(kappa_esitys, row.names = FALSE)

cat("\nKeskimääräinen kappa (5 varsinaista kehystä):",
    round(mean(kappa_taulu$kappa[kappa_taulu$kehys %in% paakehykset],
               na.rm = TRUE), 3), "\n")



#  8. Erimielisyystapaukset laadullista tarkastelua varten

erimielisyydet <- bind_rows(lapply(kehykset, function(f) {
  m <- df[[paste0(f, "_model")]]
  h <- df[[paste0(f, "_human")]]
  eri <- which(!is.na(m) & !is.na(h) & m != h)
  if (length(eri) == 0) return(NULL)
  data.frame(kehys   = f,
             rivi    = eri,
             malli   = m[eri],
             ihminen = h[eri],
             Quote   = substr(df$Quote[eri], 1, 500),
             stringsAsFactors = FALSE)
}))

cat("\nErimielisyystapauksia yhteensä:", nrow(erimielisyydet), "\n")


#  9. Tallennus

write.xlsx(
  list(kappa          = kappa_taulu,
       erimielisyydet = erimielisyydet),
  file.path(polku, "kappa_tulokset.xlsx"),
  overwrite = TRUE
)

cat("Tulokset tallennettu:", file.path(polku, "kappa_tulokset.xlsx"), "\n")


# 10. Erimielisyysmäärät ennen ja jälkeen

library(tidyr)

raja <- as.Date("2022-02-24")

if (!"Date" %in% names(df)) {
  stop("Sarake Date puuttuu yhdistetystä aineistosta. ",
       "Lisää se kohdan 3 inner_joinin select-lausekkeeseen.")
}

pvm    <- df[["Date"]]
df$pvm <- if (is.numeric(pvm)) as.Date(pvm, origin = "1899-12-30") else as.Date(pvm)

df$jakso <- factor(ifelse(df$pvm < raja, "ennen", "jälkeen"),
                   levels = c("ennen", "jälkeen"))

# Erimielisyys jokaiselle puhe × kehys -parille.
# Huom. yhdistetyssä aineistossa sarakkeet ovat muotoa <kehys>_model
# ja <kehys>_human, joten pivotointi tehdään molemmille erikseen.
erimielisyydet_jaksoittain <- df %>%
  mutate(puhe_id = row_number()) %>%
  select(puhe_id, jakso,
         all_of(paste0(kehykset, "_model")),
         all_of(paste0(kehykset, "_human"))) %>%
  pivot_longer(
    cols         = -c(puhe_id, jakso),
    names_to     = c("kehys", "arvioija"),
    names_pattern = "^(.*)_(model|human)$",
    values_to    = "arvo"
  ) %>%
  pivot_wider(names_from = arvioija, values_from = arvo) %>%
  rename(malli = model, ihminen = human) %>%
  filter(!is.na(malli), !is.na(ihminen)) %>%
  mutate(eri = malli != ihminen)

# Tarkistus: pitäisi olla 107
cat("\nErimielisyyksiä yhteensä:", sum(erimielisyydet_jaksoittain$eri), "\n")

# 1) Jakauma jaksoittain, suhteutettuna puheiden ja koodauspäätösten määrään
erimielisyydet_jaksoittain %>%
  group_by(jakso) %>%
  summarise(
    puheita         = n_distinct(puhe_id),
    paatoksia       = n(),
    erimielisyyksia = sum(eri),
    .groups = "drop"
  ) %>%
  mutate(
    osuus_erimielisyydesta = erimielisyyksia / sum(erimielisyyksia),
    erimielisyysaste       = erimielisyyksia / paatoksia
  )

# 2) Jakauma kehyksittäin ja jaksoittain
erimielisyydet_jaksoittain %>%
  group_by(kehys, jakso) %>%
  summarise(erimielisyyksia = sum(eri), n = n(),
            aste = erimielisyyksia / n, .groups = "drop") %>%
  pivot_wider(names_from = jakso, values_from = c(erimielisyyksia, aste))





