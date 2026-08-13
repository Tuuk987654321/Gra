library(openxlsx)

#Validointi. Random-näytteet exceliin

set.seed(123)

df_sample <- df %>%
  sample_n(50)


write.xlsx(df_sample, "random_50_puhetta.xlsx")

#------------------------------
#Kappa

df_model <- read.xlsx("/xxx/Validiointi/random_50_puhetta.xlsx")
df_human <- read.xlsx("/xxx/Validiointi/random_50_puhetta_oma.xlsx")

head(df_model$Quote)
head(df_human$Quote)

df <- df_model

df$geopolitical_human  <- df_human$geopolitical
df$economic_human      <- df_human$economic
df$normative_human     <- df_human$normative
df$institutional_human <- df_human$institutional
df$procedural_human    <- df_human$procedural

df <- df %>%
  mutate(across(
    c(geopolitical, economic, normative, institutional, procedural,
      geopolitical_human, economic_human, normative_human, institutional_human, procedural_human),
    as.factor
  ))

library(irr)

frames <- c("geopolitical", "economic", "normative", "institutional", "procedural")

results <- lapply(frames, function(f) {
  model <- df[[f]]
  human <- df[[paste0(f, "_human")]]
  
  k <- kappa2(data.frame(model, human))
  
  data.frame(
    kehys = f,
    kappa = k$value,
    p_value = k$p.value
  )
})

kappa_results <- do.call(rbind, results)
kappa_results

nrow(df)

