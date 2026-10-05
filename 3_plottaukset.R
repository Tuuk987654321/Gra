library(tidyverse)
library(lubridate)
library(gt)
library(dplyr)
library(openxlsx)
library(readr)
library(ggplot2)
library(scales)

#HUOM. tiedostot 3 ja 4 tulee ajaa peräkkäin.

df_raaka <- read_csv("xxx/llm_results_new_dataset_2105.csv")

#duplikaatit talteen ennen poistoa (kannanottotaso, N = 1 707)

df_duplicates <- df_raaka %>%
  group_by(Quote) %>%
  filter(n() >= 2) %>%
  ungroup()

write_csv(df_duplicates, "duplicate_speeches.csv")

# Duplikaatit poistetaan
df <- df_raaka %>% distinct(Quote, .keep_all = TRUE)

df_long <- df %>%
  mutate(vuosi = year(Date)) %>%
  select(vuosi, geopolitical, economic, normative, institutional, procedural) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "kehys",
    values_to = "arvo"
  )

#nimet
frame_labels <- c(
  geopolitical   = "Geopoliittinen",
  economic       = "Taloudellinen",
  normative      = "Normatiivinen",
  institutional  = "Institutionaalinen",
  procedural     = "Proseduraalinen"
)



frame_colors <- c(
  geopolitical   = "#A6CEE3",
  economic       = "#B2DF8A",
  normative      = "#FDBF6F",
  institutional  = "#CAB2D6",
  procedural     = "#FFFF99"
)


#Vuosittaiset määrät
df_vuosittain <- df_long %>%
  group_by(vuosi, kehys) %>%
  summarise(lkm = sum(arvo == TRUE, na.rm = TRUE), .groups = "drop")

#Abs. pylväs
ggplot(df_vuosittain, aes(x = vuosi, y = lkm, fill = kehys)) +
  geom_col() +
  scale_fill_manual(
    values = frame_colors,
    labels = frame_labels
  ) +
  theme_minimal() +
  labs(
    title = "EU:n laajentumiskehysten kehitys vuosittain",
    x = "Vuosi",
    y = "Puheiden määrä",
    fill = "Kehys"
  )

#suht
df_suhteellinen <- df_vuosittain %>%
  group_by(vuosi) %>%
  mutate(osuus = lkm / sum(lkm)) %>%
  ungroup()

ggplot(df_suhteellinen, aes(x = vuosi, y = osuus, fill = kehys)) +
  geom_col() +
  scale_fill_manual(
    values = frame_colors,
    labels = frame_labels
  ) +
  theme_minimal() +
  labs(
    title = "EU:n laajentumiskehysten suhteellinen jakauma vuosittain",
    x = "Vuosi",
    y = "Osuus puheista",
    fill = "Kehys"
  )

# määrät taulukko
df_long <- df %>%
  mutate(vuosi = year(Date)) %>%
  select(vuosi, geopolitical, economic, normative, institutional, procedural) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "kehys",
    values_to = "arvo"
  )

frame_labels <- c(
  geopolitical   = "Geopoliittinen",
  economic       = "Taloudellinen",
  normative      = "Normatiivinen",
  institutional  = "Institutionaalinen",
  procedural     = "Proseduraalinen"
)



#Ennen jälkeen 24.02.2022

cutoff <- as.Date("2022-02-24")

df_long <- df %>%
  mutate(
    Date = as.Date(Date),
    period = if_else(Date < cutoff, "Ennen", "Jälkeen"),
    period = factor(period, levels = c("Ennen", "Jälkeen"))
  ) %>%
  select(period, geopolitical, economic, normative, institutional, procedural) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "frame",
    values_to = "value"
  ) %>%
  filter(value == TRUE)


df_period <- df_long %>%
  group_by(period, frame) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(share = n / sum(n),) %>%
  ungroup()

df_period %>%
  select(period, frame, share) %>%
  pivot_wider(
    names_from = frame,
    values_from = share
  )

frame_labels <- c(
  geopolitical   = "Geopoliittinen",
  economic       = "Taloudellinen",
  normative      = "Normatiivinen",
  institutional  = "Institutionaalinen",
  procedural     = "Proseduraalinen"
)

frame_colors <- c(
  economic       = "#B2DF8A",
  geopolitical   = "#A6CEE3",
  institutional  = "#CAB2D6",
  normative      = "#FDBF6F",
  procedural     = "#FFFF99"
)

ggplot(df_period, aes(x = period, y = share, fill = frame)) +
  geom_col() +
  scale_fill_manual(values = frame_colors, labels = frame_labels) +
  theme_minimal() +
  labs(
    title = "Kehysten jakauma ennen ja jälkeen 24.02.2022",
    x = "",
    y = "Osuus",
    fill = "Kehys"
  )

#Abs. taulukko
df_abs <- df_long %>%
  filter(value == TRUE) %>%
  group_by(period, frame) %>%
  summarise(n = n(), .groups = "drop")

df_abs_table <- df_abs %>%
  pivot_wider(
    names_from = frame,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(period)

write_csv(df_abs_table, "frames_absolute_pre_post_korjattu.csv")

df_table <- df_period %>%
  select(period, frame, share) %>%
  pivot_wider(
    names_from = frame,
    values_from = share
  ) %>%
  arrange(period)

write_csv(df_table, "frame_distribution_pre_post_2022_korjattu.csv")

#Kehykset/sentimentti
cutoff <- as.Date("2022-02-24")

df_long <- df %>%
  mutate(
    Date = as.Date(Date),
    periodi = if_else(Date < cutoff, "Ennen", "Jälkeen"),
    
    # Sentimentti (1–3 → suomi)
    sentimentti = factor(
      case_when(
        Pos_Reg == 1 ~ "Vastustus",
        Pos_Reg == 2 ~ "Ehdollinen kannatus",
        Pos_Reg == 3 ~ "Kannatus",
        TRUE ~ NA_character_
      ),
      levels = c("Kannatus", "Ehdollinen kannatus", "Vastustus")
    )
   
      
  ) %>%
  select(periodi, sentimentti, geopolitical, economic, normative, institutional, procedural) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "kehys",
    values_to = "arvo"
  ) %>%
  filter(arvo == TRUE)

df_long <- df_long %>%
  mutate(
    kehys = recode(
      kehys,
      geopolitical  = "Geopoliittinen",
      economic      = "Taloudellinen",
      normative     = "Normatiivinen",
      institutional = "Institutionaalinen",
      procedural    = "Proseduraalinen"
    )
  )

df_plot <- df_long %>%
  group_by(periodi, kehys, sentimentti) %>%
  summarise(lkm = n(), .groups = "drop") %>%
  group_by(periodi, kehys) %>%
  mutate(osuus = lkm / sum(lkm), label = scales::percent(osuus, accuracy = 1)) %>%
  ungroup()

sentiment_colors <- c(
  "Vastustus" = "#F4A6A6",
  "Ehdollinen kannatus"  = "#88E788",
  "Kannatus" = "#7CA37C"
)

ggplot(df_plot, aes(x = periodi, y = osuus, fill = sentimentti)) +
  geom_col() +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  facet_wrap(~ kehys) +
  scale_fill_manual(values = sentiment_colors) +
  theme_minimal() +
  labs(
    title = "Kehysten käyttö ja sävy ennen ja jälkeen 24.2.2022",
    x = "",
    y = "Osuus",
    fill = " "
  )
 
  
  df_table <- df_plot %>%
    select(periodi, kehys, sentimentti, osuus) %>%
    pivot_wider(
      names_from = sentimentti,
      values_from = osuus,
      values_fill = 0
    ) %>%
    arrange(kehys, periodi)


  write_csv(df_table, "kehykset_sentimentti_pre_post_ilman_duplikaatteja.csv")



nrow(df)

write_csv(df, "llm_results_duplikaatit_pois.csv")

#Puheenvuorojen määrä ennen ja jälkeen
cutoff <- as.Date("2022-02-24")

df %>%
  mutate(
    Date = as.Date(Date),
    periodi = if_else(Date < cutoff, "Ennen", "Jälkeen")
  ) %>%
  count(periodi, name = "Puheenvuoroja")

#Puh.vuor määrät vuosittain
df %>%
  mutate(
    Date = as.Date(Date),
    vuosi = year(Date)
  ) %>%
  count(vuosi, name = "Puheenvuoroja") %>%
  arrange(vuosi)


#Kehykset vuosittain
# Huom. rakennetaan df_vuosittain-objektista, koska df_long on tässä
# vaiheessa jo kirjoitettu uudelleen eikä sisällä vuosi-saraketta.

df_taulukko <- df_vuosittain %>%
  mutate(kehys = recode(kehys, !!!frame_labels)) %>%
  pivot_wider(
    names_from = kehys,
    values_from = lkm,
    values_fill = 0
  )

df_taulukko <- df_taulukko %>%
  mutate(
    Yhteensä = rowSums(across(-vuosi))
  ) %>%
  arrange(vuosi)

df_taulukko

write_csv(df_taulukko, "kehykset_vuosittain_korjattu.csv")


# Lukumäärät omana taulukkonaan
df_table_lkm <- df_plot %>%
  select(periodi, kehys, sentimentti, lkm) %>%
  pivot_wider(
    names_from  = sentimentti,
    values_from = lkm,
    values_fill = 0
  ) %>%
  mutate(Yhteensä = rowSums(across(where(is.numeric)))) %>%
  arrange(kehys, periodi)

df_table_lkm
#write_csv(df_table_lkm, "kehykset_sentimentti_pre_post_lkm.csv")

# Yhdistetty taulukko: n (%)
df_table_yhd <- df_plot %>%
  mutate(
    solu = paste0(lkm, " (", scales::percent(osuus, accuracy = 1, suffix = " %"), ")")
  ) %>%
  select(periodi, kehys, sentimentti, solu) %>%
  pivot_wider(
    names_from  = sentimentti,
    values_from = solu,
    values_fill = "0 (0 %)"
  ) %>%
  left_join(
    df_plot %>%
      group_by(periodi, kehys) %>%
      summarise(Yhteensä = sum(lkm), .groups = "drop"),
    by = c("periodi", "kehys")
  ) %>%
  arrange(kehys, periodi)

df_table_yhd
write_csv(df_table_yhd, "kehykset_sentimentti_pre_post_n_ja_osuus.csv")




#lukumäärät
df_abs <- df_long %>%
  group_by(periodi, kehys) %>%
  summarise(lkm = n(), .groups = "drop")



#Ukraina
df_ukraine <- df %>%
  filter(Region == "Ukraine")

df_long <- df_ukraine %>%
  mutate(
    sentimentti = factor(
      case_when(
        Pos_Reg == 1 ~ "Vastustus",
        Pos_Reg == 2 ~ "Ehdollinen kannatus",
        Pos_Reg == 3 ~ "Kannatus",
        TRUE ~ NA_character_
      ),
      levels = c("Kannatus", "Ehdollinen kannatus", "Vastustus")
    )
  ) %>%
  select(sentimentti, geopolitical, economic, normative, institutional, procedural) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "kehys",
    values_to = "arvo"
  ) %>%
  filter(arvo == TRUE)

df_long <- df_long %>%
  mutate(
    kehys = recode(
      kehys,
      geopolitical  = "Geopoliittinen",
      economic      = "Taloudellinen",
      normative     = "Normatiivinen",
      institutional = "Institutionaalinen",
      procedural    = "Proseduraalinen"
    )
  )

df_plot <- df_long %>%
  group_by(kehys, sentimentti) %>%
  summarise(lkm = n(), .groups = "drop") %>%
  group_by(kehys) %>%
  mutate(osuus = lkm / sum(lkm), label = scales::percent(osuus, accuracy = 1)) %>%
  ungroup()

ggplot(df_plot, aes(x = kehys, y = osuus, fill = sentimentti)) +
  geom_col() +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_manual(values = sentiment_colors) +
  theme_minimal() +
  labs(
    title = "Kehykset ja sävy – Ukraina",
    x = "Kehys",
    y = "Osuus",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

df_table <- df_plot %>%
  select(kehys, sentimentti, osuus) %>%
  pivot_wider(
    names_from = sentimentti,
    values_from = osuus,
    values_fill = 0
  ) %>%
  arrange(kehys)

write_csv(df_table, "ukraine_frames_sentiment.csv")

#Balkans
df_balkans <- df %>%
  filter(Region == "Balkans")

df_long <- df_balkans %>%
  mutate(
    sentimentti = factor(
      case_when(
        Pos_Reg == 1 ~ "Vastustus",
        Pos_Reg == 2 ~ "Ehdollinen kannatus",
        Pos_Reg == 3 ~ "Kannatus",
        TRUE ~ NA_character_
      ),
      levels = c("Kannatus", "Ehdollinen kannatus", "Vastustus")
    )
  ) %>%
  select(sentimentti, geopolitical, economic, normative, institutional, procedural) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "kehys",
    values_to = "arvo"
  ) %>%
  filter(arvo == TRUE)

df_long <- df_long %>%
  mutate(
    kehys = recode(
      kehys,
      geopolitical  = "Geopoliittinen",
      economic      = "Taloudellinen",
      normative     = "Normatiivinen",
      institutional = "Institutionaalinen",
      procedural    = "Proseduraalinen"
    )
  )

df_plot <- df_long %>%
  group_by(kehys, sentimentti) %>%
  summarise(lkm = n(), .groups = "drop") %>%
  group_by(kehys) %>%
  mutate(osuus = lkm / sum(lkm), label = scales::percent(osuus, accuracy = 1)) %>%
  ungroup()

ggplot(df_plot, aes(x = kehys, y = osuus, fill = sentimentti)) +
  geom_col() +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_manual(values = sentiment_colors) +
  theme_minimal() +
  labs(
    title = "Kehykset ja sävy – Balkan",
    x = "Kehys",
    y = "Osuus",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

df_table <- df_plot %>%
  select(kehys, sentimentti, osuus) %>%
  pivot_wider(
    names_from = sentimentti,
    values_from = osuus,
    values_fill = 0
  ) %>%
  arrange(kehys)

write_csv(df_table, "balkan_frames_sentiment.csv")


#Balkan vs Ukraina

cutoff <- as.Date("2022-02-24")

df_long <- df %>%
  filter(Region %in% c("Ukraine", "Balkans")) %>%
  mutate(
    Region = recode(
      Region,
      "Ukraine" = "Ukraina",
      "Balkans" = "Länsi-Balkan"),
    Date = as.Date(Date),
    periodi = if_else(Date < cutoff, "Ennen", "Jälkeen"),
    
    sentimentti = factor(
      case_when(
        Pos_Reg == 1 ~ "Vastustus",
        Pos_Reg == 2 ~ "Ehdollinen kannatus",
        Pos_Reg == 3 ~ "Kannatus",
        TRUE ~ NA_character_
      ),
      levels = c("Kannatus", "Ehdollinen kannatus", "Vastustus")
    )
  ) %>%
  pivot_longer(
    cols = c(geopolitical, economic, normative, institutional, procedural),
    names_to = "kehys",
    values_to = "arvo"
  ) %>%
  filter(arvo == TRUE)

df_long <- df_long %>%
  mutate(
    kehys = recode(
      kehys,
      geopolitical  = "Geopoliittinen",
      economic      = "Taloudellinen",
      normative     = "Normatiivinen",
      institutional = "Institutionaalinen",
      procedural    = "Proseduraalinen"
    )
  )

df_plot <- df_long %>%
  group_by(Region, periodi, kehys, sentimentti) %>%
  summarise(lkm = n(), .groups = "drop") %>%
  group_by(Region, periodi, kehys) %>%
  mutate(osuus = lkm / sum(lkm)) %>%
  ungroup() %>%
  mutate(label = scales::percent(osuus, accuracy = 1))

sentiment_colors <- c(
  "Vastustus" = "#F4A6A6",
  "Ehdollinen kannatus" = "#88E788",
  "Kannatus" = "#7CA37C"
)

ggplot(df_plot, aes(x = periodi, y = osuus, fill = sentimentti)) +
  geom_col() +
  geom_text(
    aes(label = ifelse(osuus > 0.05, label, "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  facet_grid(Region ~ kehys) +
  scale_fill_manual(values = sentiment_colors) +
  theme_minimal() +
  labs(
    title = "Kehykset ja sävy ennen ja jälkeen 24.2.2022",
    x = "",
    y = "Osuus",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    strip.text = element_text(size = 10),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(10, 10, 30, 10)
  )

  #Tarkastus
df_long %>%
  group_by(Region, periodi, kehys) %>%
  summarise(lkm = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = kehys,
    values_from = lkm
  ) %>%
  arrange(Region, periodi)

# Geopoliittsen ja normatiivisen yhteisesiintyminen
cutoff <- as.Date("2022-02-24")

df_cooccurrence <- df %>%
  mutate(
    Date = as.Date(Date),
    periodi = if_else(Date < cutoff, "Ennen", "Jälkeen"),
    geo_norm = geopolitical & normative
  ) %>%
  group_by(periodi) %>%
  summarise(
    puheita_yhteensa = n(),
    geo_norm_lkm = sum(geo_norm, na.rm = TRUE),
    geo_norm_osuus = geo_norm_lkm / puheita_yhteensa,
    .groups = "drop"
  )

df_cooccurrence

ggplot(df_cooccurrence,
       aes(x = periodi, y = geo_norm_osuus)) +
  geom_col(fill = "#A6CEE3", width = 0.6) +
  geom_text(
    aes(label = percent(geo_norm_osuus, accuracy = 1)),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(df_cooccurrence$geo_norm_osuus) * 1.15)) +
  theme_minimal() +
  labs(
    title = "Geopoliittisen ja normatiivisen kehyksen yhteisesiintyminen",
    subtitle = "Ennen ja jälkeen 24.2.2022",
    x = "",
    y = "Osuus puheista"
  )
