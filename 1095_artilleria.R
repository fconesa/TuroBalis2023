

# ===================================================
# @fconesa - 2023

# Codi que acompanya l'article: 
# Conesa, F.C. (2023) 
# 'L’últim front al Maresme: la batalla del Turó del Balís a finals de gener de 1939'
# enviat per a publicació a la revista Ebre 38 -> https://revistes.ub.edu/index.php/ebre38

# FIGURA 10
# Nombre de projectils disparats
#
# Font: CAPS MONÉS 2-38, p. 18 (Full 1095)
# "Specchie dei colpi sparati dall'Artiflieria Divisionale"
# "Relazione sulla battaglia di Catalogna. Corpo Truppe Volontarie. Comando Divisione Legionaria Frecce Azzurre. Stato Maggiore"
# Institut Cartogràfic i Geològic de Catalunya, Fons de la guerra civil (1936-1939) 
# [accés online 12/08/2023 a https://cartotecadigital.icgc.cat/digital/collection/fmones/id/1095/rec/360]


# ===================================================
# llibreries R 
require(ggplot2) # plot
require(tidyr) # gather
require(dplyr) # mutate, recode
require(lubridate) # lubridate
library(cowplot) # plot_grid
require(svglite) # exportar en svg


# ===================================================
# dataset

# importar taula .csv en un nou data frame (DF)
DFartilleria = read.csv("[directori_usuari]/1095_artilleria.csv")

# freqüències de projectils en format numèric
DFartilleria$XI.gr.65.17 = as.numeric(DFartilleria$XI.gr.65.17)
DFartilleria$X.gr.75.27 = as.numeric(DFartilleria$X.gr.75.27)
DFartilleria$I.gr.100.17 = as.numeric(DFartilleria$I.gr.100.17)

# transformació "dies" en columnes YYYY-MM-D
DFartilleria_ymd = DFartilleria %>%
  mutate(year = lubridate::year(dies), 
         month = lubridate::month(dies), 
         day = lubridate::day(dies))

# afegir columnes per visualitzar només la primera i única freqüència per anys i mesos
DFartilleria_ymd = DFartilleria_ymd %>%
  mutate(
    D = ifelse(lag(day) != day |
                 is.na(lag(day)), day, ""),
    M = ifelse(lag(month) != month |
                 is.na(lag(month)), month, ""),
    Y = ifelse(lag(year) != year |
                 is.na(lag(year)), year, ""))

# nom del mes per a la visualització de l'eix x al gràfic
DFartilleria_ymd$M = recode(DFartilleria_ymd$M, "12"="Desembre", "1"="Gener", "2"="Febrer")


# ===================================================
# gràfic 01 lollipop

# afegir columna amb total projectils/dia
DFartilleria_ymd$total = rep(c(rowSums(DFartilleria_ymd[,2:4])))

# DF amb relació d'episodis destacats per remarcar etiquetes al gràfic
# visualització individualitzada pel dia 1939-01-29 (Balís)
highL_artilleria = data.frame(name=c("riu Segre-Serós", "riu Set-Castelldans", "Montalt-Balís"), 
                       dies=c("1938-12-23", "1939-01-03", "1939-01-29"), 
                       highlight=c(FALSE, FALSE, TRUE))

# DF amb selecció d'episodis destacats 
highL_artilleriaT = left_join(highL_artilleria, DFartilleria_ymd, by="dies")

# crear columnes artificials (fons) per remarcar els episodis destacats (dies en factor)
highL_artilleriaCol = DFartilleria_ymd %>%
  mutate(dies_factor = as.numeric(as.factor(dies))) %>%
  filter(dies %in% c("1938-12-23", "1939-01-03", "1939-01-29"))

# paleta de colors
colorEpisodi = c("#919191", "#ff9400") # episodis/Balís

# ggplot + plot.margin
artilleriaTotal = 
  ggplot(DFartilleria_ymd, aes(x=dies, y=total)) +
  geom_segment(aes(x=dies, xend=dies, y=0, yend=total), color="grey") +
  geom_point(size=3, color="red", alpha=DFartilleria_ymd$total>0) +
  geom_rect(data = highL_artilleriaCol, aes(xmin=dies_factor -0.5, xmax=dies_factor +0.5, ymin=0, ymax=total), 
            fill=alpha("grey", 0.3), linewidth=0, inherit.aes=FALSE) +
  geom_label(data = highL_artilleriaT, aes(label=name, x=dies, y=total +0.5, color=highlight), inherit.aes=FALSE, hjust=-0.15, size=6) +
  scale_color_manual(values=colorEpisodi, guide=FALSE) +
  scale_x_discrete(labels=paste(DFartilleria_ymd$D , DFartilleria_ymd$M, DFartilleria_ymd$Y, sep="\n")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title.y = element_text(size=18, face="bold"),
    axis.title.x = element_blank(), axis.text.x = element_blank(),
    axis.text = element_text(size=16),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0.5, 0.5, -6, 0.5, unit = "cm")
  ) + 
  ylab("Total projectils \n") + 
  ggtitle("Projectils d'artilleria disparats")
artilleriaTotal


# ===================================================
# gràfic 02 mapa de calor

# conversió en format "llarg" (wide to long) per reorganitzar les dades amb una variable "tipus" 
# tres grups: projectils segons grup artiller/calibre
DFartilleria_ymdL <- gather(DFartilleria_ymd, tipus, nProjectils, XI.gr.65.17:I.gr.100.17, factor_key=TRUE)

# canviar l'ordre en que apareixen els calibres de més petit a més gran (XI, X, I)
levels(DFartilleria_ymdL$tipus) = c("XI.gr.65/17", "X.gr.75/27", "I.gr.100/17")

# afegir columna a "DFartilleriaL"per escurçar el name dels grups artillers/calibres i reordenar l'ordre de la seqüència
DFartilleria_ymdL$tipusCurt = recode_factor(DFartilleria_ymdL$tipus, "XI.gr.65/17"="65/17", "X.gr.75/27"="75/27", "I.gr.100/17"="100/17")
ordre_calibreCurt = c("100/17", "75/27", "65/17")

# ggplot + plot_margin 
artilleriaCalibre = 
ggplot(DFartilleria_ymdL, aes(x=dies, y=tipusCurt, fill=nProjectils, label=nProjectils)) + 
  geom_tile(col="white") +
  geom_text(colour='black', size=3, alpha=DFartilleria_ymdL$nProjectils>0) +
  scale_fill_distiller(type='div', palette="Spectral", limits= c(1, 3500), guide=FALSE) + 
  scale_y_discrete(limits = ordre_calibreCurt) +
  scale_x_discrete(labels=paste(DFartilleria_ymdL$D , DFartilleria_ymdL$M, DFartilleria_ymdL$Y, sep="\n")) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title = element_text(size=18, face="bold"), 
    axis.text = element_text(size=16),
    plot.margin = margin(0.5, 0.5, -3, 0.5, unit = "cm")
  ) + 
  ylab("Calibre \n") +
  xlab("Ofensiva de Catalunya (en dies) - CTV Fletxes Blaves") 
artilleriaCalibre


# ===================================================
# combinar gràfics 01+02

# grid
plot_grid(artilleriaTotal, artilleriaCalibre, ncol = 1, align = "v")

# exportar
ggsave("img_artilleriaCombi.svg", width = 18, height = 10)

# --- FI --- 
# --
# -



