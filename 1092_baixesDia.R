

# ===================================================
# @fconesa - 2023

# Codi que acompanya l'article: 
# Conesa, F.C. (2023) 
# 'L’últim front al Maresme: la batalla del Turó del Balís a finals de gener de 1939'
# enviat per a publicació a la revista Ebre 38 -> https://revistes.ub.edu/index.php/ebre38

# FIGURA 13
# Relació de baixes
#
# Font: CAPS MONÉS 2-38, p. 16 (Full 1092)
# "Specchio perdite"
# "Relazione sulla battaglia di Catalogna. Corpo Truppe Volontarie. Comando Divisione Legionaria Frecce Azzurre. Stato Maggiore"
# Institut Cartogràfic i Geològic de Catalunya, Fons de la guerra civil (1936-1939) 
# [accés online 12/08/2023 a https://cartotecadigital.icgc.cat/digital/collection/fmones/id/1092/rec/360]


# ===================================================
# llibreries R 
require(ggplot2) # plot
require(tidyr) # gather
require(dplyr) # mutate, recode
require(lubridate) # lubridate
require(Cairo) # unicode chr
require(svglite) # exportar en svg


# ===================================================
# dataset

# importar taula .csv en un nou data frame (DF)
DFbaixesDia = read.csv("[directori_usuari]/1092_baixesDia.csv")

# freqüències de baixes en format numèric
DFbaixesDia[,-1] = sapply(DFbaixesDia[,-1], as.numeric)

# transformació "dies" en columnes YYYY-MM-D
DFbaixesDia_ymd = DFbaixesDia %>%
  mutate(year = lubridate::year(dies), 
         month = lubridate::month(dies), 
         day = lubridate::day(dies))

# afegir columnes per visualitzar només la primera i única freqüència per anys i mesos
DFbaixesDia_ymd = DFbaixesDia_ymd %>%
  mutate(
    D = ifelse(lag(day) != day |
                 is.na(lag(day)), day, ""),
    M = ifelse(lag(month) != month |
                 is.na(lag(month)), month, ""),
    Y = ifelse(lag(year) != year |
                 is.na(lag(year)), year, ""))

# nom del mes per a la visualització de l'eix x al gràfic
DFbaixesDia_ymd$M = recode(DFbaixesDia_ymd$M, "12"="Desembre", "1"="Gener", "2"="Febrer")


# ===================================================
# subset

# DF selecció de totals malalts, ferits i morts
DFbaixesDia_ymdT = DFbaixesDia_ymd %>% select(dies, total.mort:Y)

# conversió en format "llarg" (wide to long) per reorganitzar les dades amb una variable "tipus" 
# tres grups: projectils segons grup artiller/calibre
DFbaixesDia_ymdTL <- gather(DFbaixesDia_ymdT, tipus, total, total.mort:total.mal, factor_key=TRUE)

# organitzar ordre dels grups segons tious de baixa; malalts, ferits i per últim, morts
DFbaixesDia_ymdTL$tipus = factor(DFbaixesDia_ymdTL$tipus, levels=c("total.mal", "total.ferit", "total.mort"))


# ===================================================
# gràfic

# DF amb relació d'episodis destacats per remarcar etiquetes al gràfic
# visualització individualitzada pel dia 1939-01-29 (Balís)
highL_ferits = data.frame(name=c("riu Set-Castelldans", "Sta Coloma Queralt", "riu Llobregat", "Montalt-Balís", "riu Ridaura"), 
                          dies=c("1939-01-03", "1939-01-16", "1939-01-24", "1939-01-29", "1939-02-04"), 
                          highlight=c(FALSE, FALSE, FALSE, TRUE, FALSE))

# DF amb freqüències totals segons grup "tipus" per visualitzar etiquetes
highL_feritsT = left_join(highL_ferits, DFbaixesDia_ymdT, by="dies")

# seqüència amb l'ordre de les baixes per ordenar les columnes
ordreBaixes = c("total.mal", "total.ferit", "total.mort")

# paleta de colors i seqüència per icones en caràcters UNICODE
colorEpisodi = c("#919191", "#ff9400") # episodis/Balís
colorTipus = c("lightgrey", "brown", "red") # tipus de baixes

# ggplot
ggplot(DFbaixesDia_ymdTL, aes(x=dies, y=total, fill=tipus)) +
  geom_bar(stat = "identity") +
  geom_label(data = highL_feritsT, aes(label=name, x=dies, y=total.ferit +0.5, color=highlight), inherit.aes=FALSE, hjust=1.05, vjust=-0.05, size=6) +
  scale_fill_manual(name="Tipus de baixes",
                    labels=c("Malalts", "Ferits", "Morts"),
                    values=colorTipus) +
  scale_color_manual(values=colorEpisodi, guide="none") +
  scale_x_discrete(labels=paste(DFbaixesDia_ymdTL$D, DFbaixesDia_ymdTL$M, DFbaixesDia_ymdTL$Y, sep="\n")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title = element_text(size=18, face="bold"), 
    axis.text = element_text(size=16),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size=16), 
    legend.text = element_text(size=15),
    legend.background = element_rect(linetype = "solid", size=0.1, colour="black"),
    legend.position = c(.93,.90)
  ) + 
  ylab("Baixes \n") +
  xlab("\n Ofensiva de Catalunya (en dies) - CTV Fletxes Blaves") + 
  ggtitle("Baixes entre morts, ferits i malalts")

# exportar
ggsave("img_baixesDia.svg", width = 18, height = 10)

# --- FI --- 
# --
# -
