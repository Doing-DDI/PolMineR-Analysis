library(polmineR)
library(tidyverse)

### Wörteranzahl pro Fraktion/pro Jahr bestimmen

fraktionen <- c("CDU/CSU", "SPD", "FDP", "GRUENE", "DIE LINKE", "AfD", "Die Linke", "BSW") #Linke in zwei Schreibweisen vorhanden
years <- seq(2009, 2024)

## Partition erstellen, damit nur die Legislaturperioden 17-20 untersucht werden

lp_17 <- partition("GERMAPARL2", protocol_lp = seq(17, 20))

## Data Frame initialisieren für die Tokenanzahl pro Jahr und pro Fraktion zu speichern
words_per_year <- data.frame(Fraktion = character(), Wortanzahl = integer(), stringsAsFactors = FALSE)

## Mit der for-Schleife jeweils Partitionen für alle Fraktionen und Jahre erstellen,
## die Tokenanzahl bestimmen und im DataFrame speichern
for (fraktion in fraktionen) {
  for (year in years) {
    partition_year <- lp_17 %>% partition(protocol_year = year)
    
    partition_fl <- partition(partition_year, speaker_parlgroup = fraktion)
    
    wortanzahl <- size(partition_fl)
    
    words_per_year <- rbind(words_per_year, data.frame(Fraktion = fraktion, Jahr = year, Wortanzahl = wortanzahl, stringsAsFactors = FALSE))
  }
}

### Suche nach Term "Daten" und Komposita, die "Daten" enthalten.
daten_query <- '"(?!.*([Ss]oldat|[Kk]andidat|[Uu]pdat)).*[Dd]aten.*"'

## Nennungen von Datenkomposita pro Fraktion/pro Jahr erheben, mit 
## DataFrame mit Wörteranzahl pro Fraktion/pro Jahr kombinieren
## und Anteil pro 1000 Wörter errechnen

daten_fraktion <- dispersion(
  lp_17,
  query = daten_query,
  s_attribute = c("speaker_parlgroup", "protocol_year"),
  cqp = TRUE
) %>% 
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "value") %>% 
  filter(speaker_parlgroup %in% fraktionen) %>% 
  rename(Fraktion = speaker_parlgroup, Jahr = year) %>% 
  mutate(Jahr = as.integer(Jahr)) %>%
  left_join(., words_per_year, by = c("Fraktion", "Jahr")) %>%
  filter(Wortanzahl != 0) %>%
  mutate(Freq_per_1000 = value/Wortanzahl * 1000)
