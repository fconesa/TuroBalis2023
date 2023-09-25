

# ===================================================
# @fconesa - 2023

# Codi que acompanya l'article: 
# Conesa, F.C. (2023) 
# 'L’últim front al Maresme: la batalla del Turó del Balís a finals de gener de 1939'
# enviat per a publicació a la revista Ebre 38 -> https://revistes.ub.edu/index.php/ebre38

# FIGURA 14
# Relació de baixes segons zones de combat

# Font: CAPS MONÉS 2-38, p. 13 (Full 1089)
# "Specchie combattimenti sostenuti dalla divisione e relative perdite subite"
# "Relazione sulla battaglia di Catalogna. Corpo Truppe Volontarie. Comando Divisione Legionaria Frecce Azzurre. Stato Maggiore"
# Institut Cartogràfic i Geològic de Catalunya, Fons de la guerra civil (1936-1939) 
# [accés online 12/08/2023 a https://cartotecadigital.icgc.cat/digital/collection/fmones/id/1089/rec/360]


# ===================================================
# llibreries R 
require(ggplot2) # plot
require(lubridate) # lubridate
require(dplyr) # mutate, recode
require(svglite) # exportar en svg


# ===================================================
# dataset

# importar taula .csv en un nou data frame (DF)
DFbaixesZona = read.csv("E:/proj_balis/data_export_vis/fmones_1089_full_AWS_baixes_zones/csv_1089_baixesZona.csv")
#DFbaixesZona = read.csv("[directori_usuari]/1089_baixesZona.csv")

# freqüències de baixes en format numèric
DFbaixesZona[,3:6] = sapply(DFbaixesZona[,3:6], as.numeric)

# transformació "dies" en columnes YYYY-MM-D
DFbaixesZona_ymd = DFbaixesZona %>%
  mutate(year = lubridate::year(dies), 
         month = lubridate::month(dies), 
         day = lubridate::day(dies))

# afegir columnes per visualitzar nameés la primera i única freqüència per anys i mesos
DFbaixesZona_ymd = DFbaixesZona_ymd %>%
  mutate(
    D = ifelse(lag(day) != day |
                 is.na(lag(day)), day, ""),
    M = ifelse(lag(month) != month |
                 is.na(lag(month)), month, ""),
    Y = ifelse(lag(year) != year |
                 is.na(lag(year)), year, ""))

# nom del mes per a la visualització de l'eix x al gràfic
DFbaixesZona_ymd$M = recode(DFbaixesZona_ymd$M, "12"="Desembre", "1"="Gener", "2"="Febrer")


# ===================================================
# gràfic

# afegir columna amb total baixes/zona
DFbaixesZona_ymd$total = rep(c(rowSums(DFbaixesZona_ymd[,3:6])))

# DF amb relació d'episodis destacats per remarcar etiquetes al gràfic
# visualització individualitzada pel dia 1939-01-29 (Balís)
highL_baixesZona = data.frame(name=c("Montalt-Balís"), 
                              dies=c("1939-01-29"),
                              highlight=TRUE)

# DF amb selecció d'episodis destacats 
highL_baixesZonaT = left_join(highL_baixesZona, DFbaixesZona_ymd, by="dies")

# paleta de colors
colorEpisodi = "#ff9400" # episodis/Balís

#ggplot
DFbaixesZona_ymd %>%
  arrange(total) %>%
  mutate(zonaCombat = factor(zonaCombat , unique(zonaCombat ))) %>%
  ggplot() + aes(x=zonaCombat, y=total) +
  geom_segment( aes(x=zonaCombat, xend=zonaCombat, y=0, yend=total)) +
  geom_point(size=4, color="darkred") +
  geom_label(data=highL_baixesZonaT, aes(label=name, x=zonaCombat , y=total +0.5, color=highlight), inherit.aes=FALSE, hjust= -0.15, hjust="inward", size=6) +
  scale_color_manual(values=colorEpisodi, guide=FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title = element_text(size=18, face="bold"),
    axis.text = element_text(size=16),
    panel.grid.major.y = element_blank(),
  ) + 
  ylab("Total \n") +
  xlab("\n Zones de combat - CTV Fletxes Blaves") + 
  ggtitle("Baixes entre ferits i morts ordenades per zones de combat")

# exportar
ggsave("img_baixesZona.svg", width = 18, height = 10)

# --- FI --- 
# --
# -
  