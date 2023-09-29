

# ===================================================
# @fconesa - 2023

# Codi que acompanya l'article: 
# Conesa, F.C. (2023)  
# 'L’últim front al Maresme: la batalla del Turó del Balís a finals de gener de 1939'
# enviat per a publicació a la revista Ebre 38 -> https://revistes.ub.edu/index.php/ebre38

# FIGURA 11
# Presoners capturats entre oficials i tropa republicana
#
# Font: CAPS MONÉS 2-38, p. 14 (Full 1091)
# "Specchie dei prigionieri catturati"
# "Relazione sulla battaglia di Catalogna. Corpo Truppe Volontarie. Comando Divisione Legionaria Frecce Azzurre. Stato Maggiore [Itàlia]"
# Institut Cartogràfic i Geològic de Catalunya, Fons de la guerra civil (1936-1939) 
# [accés online 12/08/2023 a https://cartotecadigital.icgc.cat/digital/collection/fmones/id/1091/rec/360]


# ===================================================
# llibreries R 
require(ggplot2) # plot
require(tidyr) # gather
require(dplyr) # mutate, recode
require(lubridate) # lubridate
require(svglite) # exportar en svg
require(ggrepel) # etiquetes sense overlap


# ===================================================
# dataset

# importar taula .csv en un nou data frame (DF)
DFpresoners = read.csv("[directori_usuari]/1091_presoners.csv")

# freqüències de total en format numèric
DFpresoners$oficials = as.numeric(DFpresoners$oficials)
DFpresoners$tropa = as.numeric(DFpresoners$tropa)

# conversió en format "llarg" (wide to long) per reorganitzar les dades amb una variable "tipus" 
# dos grups: oficialitat i tropa
DFpresonersL = gather(DFpresoners, tipus, total, oficials:tropa, factor_key=TRUE)

# transformació "dies" en columnes YYYY-MM-D
DFpresonersL_ymd = DFpresonersL %>%
  mutate(year = lubridate::year(dies), 
         month = lubridate::month(dies), 
         day = lubridate::day(dies))

# afegir columnes per visualitzar només la primera i única freqüència per anys i mesos
DFpresonersL_ymd = DFpresonersL_ymd %>%
  mutate(
    D = ifelse(lag(day) != day |
                 is.na(lag(day)), day, ""),
    M = ifelse(lag(month) != month |
                 is.na(lag(month)), month, ""),
    Y = ifelse(lag(year) != year |
                 is.na(lag(year)), year, ""))

# nom del mes per a la visualització de l'eix x al gràfic
DFpresonersL_ymd$M = recode(DFpresonersL_ymd$M, "12"="Desembre", "1"="Gener", "2"="Febrer")


# ===================================================
# gràfic

# paleta de colors
colorOficials = "#d57dfb"
colorTropa = "#ffcbbf"
colorEpisodi = c("#919191", "#ff9400") # episodis/Balís

# selecció només "oficials"
DFpresonersLoficials = subset(DFpresonersL, tipus == "oficials")

# DF amb episodis destacats per les capture entre "oficials"
highL_presonersOficials = data.frame(name=c("riu Set-Castelldans", "Montalt-Balís"), 
                                     dies=c("1939-01-03", "1939-01-29"), 
                                     highlight=c(FALSE, TRUE))
highL_presonersToficials = left_join(highL_presonersOficials, DFpresonersLoficials, by="dies")

# ggplot per presoners"oficials "
oficials = ggplot(DFpresonersLoficials, aes(x=dies, y=total, fill=tipus)) +
  geom_bar(stat="identity", fill=colorOficials) +
  geom_label(data = highL_presonersToficials, aes(label=name, x=dies, y=total, color=highlight), inherit.aes=FALSE, hjust=1, size=6) +
  scale_color_manual(values=colorEpisodi, guide="none") +
  theme_minimal() +
  theme(
    plot.title=element_text(size=20, face="bold"),
    axis.title=element_text(size=18, face="bold"), 
    axis.text=element_text(size=16),
    axis.title.y=element_text(colour="#c54bfa"),
    axis.title.x = element_blank(), axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) + 
  ylab("Oficials \n") +
  ggtitle("Presoners capturats entre oficials i tropa republicana")
oficials

# selecció només "tropa"
DFpresonersLtropa = subset(DFpresonersL, tipus == "tropa")

# DF amb episodis destacats per les capture de "tropa"
highL_presoners = data.frame(name=c("riu Set-Castelldans", "Montalt-Balís", "Arenys-St.Pol", "riu Todera", "riu Ter", "riu Fluvià"), 
                             dies=c("1939-01-03", "1939-01-29", "1939-01-30", "1939-02-01", "1939-02-07", "1939-02-09"), 
                             highlight=c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
highL_presonersT = left_join(highL_presoners, DFpresonersLtropa, by="dies")

# ggplot per presoners "tropa"
tropa = ggplot(DFpresonersLtropa, aes(x=dies, y=total, fill=tipus)) +
  geom_bar(stat = "identity", fill=colorTropa) +
  geom_label_repel(data = highL_presonersT, aes(label=name, x=dies, y=total, color=highlight), inherit.aes=FALSE, hjust=1.1, size=6) +
  scale_color_manual(values=colorEpisodi, guide="none") +
  scale_x_discrete(labels=paste(DFpresonersL_ymd$D , DFpresonersL_ymd$M, DFpresonersL_ymd$Y, sep="\n")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title = element_text(size=18, face="bold"), 
    axis.text = element_text(size=16),
    axis.title.y=element_text(colour="#de6347"),
    panel.grid.major.x = element_blank(),
  ) + 
  ylab("Tropa \n") +
  xlab("\n Ofensiva de Catalunya (en dies) - CTV Fletxes Blaves")
tropa

# grid
plot_grid(oficials, tropa, ncol = 1, align = "v")

# exportar
ggsave("img_presoners.svg", width=18, height=10)
ggsave("img_presoners.jpg", width=18, height=10)
