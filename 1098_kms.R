

# ===================================================
# @fconesa - 2023

# Codi que acompanya l'article: 
# Conesa, F.C. (2023) 
# 'L’últim front al Maresme: la batalla del Turó del Balís a finals de gener de 1939'
# enviat per a publicació a la revista Ebre 38 -> https://revistes.ub.edu/index.php/ebre38

# FIGURA 9
# Moviment del CTV (en quilòmetres/dia)
#
# Font: CAPS MONÉS 2-38, p. 22 (Full 1098)
# "Grafice dei percorsi giornalieri e totali"
# "Relazione sulla battaglia di Catalogna. Corpo Truppe Volontarie. Comando Divisione Legionaria Frecce Azzurre. Stato Maggiore"
# Institut Cartogràfic i Geològic de Catalunya, Fons de la guerra civil (1936-1939) 
# [accés online 12/08/2023 a https://cartotecadigital.icgc.cat/digital/collection/fmones/id/1098/rec/360]


# ===================================================
# llibreries R 
require(ggplot2) # plot
require(lubridate) # lubridate
require(dplyr) # mutate, recode
require(svglite) # exportar en svg


# ===================================================
#dataset 

# importar taula .csv en un nou data frame (DF)
DFkms = read.csv("[directori_usuari]/1098_kms.csv")

# arrodonir valors kms extrets del ràfic original mitjançant WebPlotDigitizer
DFkms$kms = round(DFkms$kms, digits = 0)

# transformació "dies" en columnes YYYY-MM-D
DFkms_ymd = DFkms %>%
  mutate(year = lubridate::year(dies), 
         month = lubridate::month(dies), 
         day = lubridate::day(dies))

# afegir columnes per visualitzar només la primera i única freqüència per anys i mesos
DFkms_ymd = DFkms_ymd %>%
  mutate(
    D = ifelse(lag(day) != day |
                 is.na(lag(day)), day, ""),
    M = ifelse(lag(month) != month |
                 is.na(lag(month)), month, ""),
    Y = ifelse(lag(year) != year |
                 is.na(lag(year)), year, ""))

# nom del mes per a la visualització de l'eix x al gràfic
DFkms_ymd$M = recode(DFkms_ymd$M, "12"="Desembre", "1"="Gener", "2"="Febrer")


# ===================================================
# gràfic

# DF amb relació de fases de l'ofensiva segons original CTV
# visualització individualitzada per a la fase 6 (Alt Maresme-Empordà)
highL_fases = data.frame(name=c("Fase 1", "Fase 2", "Fase 3", "Fase 4"), 
                     description=c("Protezione fianco sinistro del CTV",
                                     "Aggiramento del cinturone di Catalogna", 
                                     "Rottura del Llobregat al Mare", 
                                     "Inseguimento a fondo"),
                     start_x=c(1, 11, 31, 37), 
                     end_x=c(10, 30, 36, 47), 
                     highlight=c(FALSE, FALSE, FALSE, TRUE))

# DF amb relació d'episodis destacats
# colors segons estil de columna; visualització individualitzada pel dia 1939-01-29 (Balís)
highL_kmsCol = places = data.frame(name=c("riu LLobregat", "Montalt-Balís", "riu Tordera", "riu Ridaura", "riu Ter"), 
                     dies = c("1939-01-24", "1939-01-29", "1939-02-01", "1939-02-04", "1939-02-07"), 
                     highlight = c("labelWhite", "labelBalís", "labelGrey", "labelWhite", "labelWhite"))

# ggplot
ggplot(DFkms_ymd, aes(x=dies, y=kms)) +
  geom_rect(data = highL_fases, aes(xmin=start_x -0.3, xmax=end_x +0.3, ymin=0, ymax=30, 
                                    fill=highlight), linewidth=0, inherit.aes=FALSE, alpha=0.3) +
  geom_bar(stat="identity") +
  geom_label(data = highL_fases, aes(label=name, x=(start_x+end_x)/2, y=29), hjust=0.5, size=6, label.size = NA) +
  geom_label(data = highL_fases, aes(label=description, x=(start_x+end_x)/2, y=28), hjust=0.5, size= 5, fontface="italic", label.size = NA) +
  geom_text(data=highL_kmsCol, aes(label=name, x=dies, y=1, color=highlight), inherit.aes=FALSE, hjust=0.1, size=5.5, angle=90) +
  scale_fill_manual(values=c("lightgrey", "goldenrod1"), guide=FALSE) +
  scale_color_manual(values = c("labelWhite"="white", "labelBalís"="#ff9400", "labelGrey"= "#6b6d6e"), guide = FALSE)+
  scale_x_discrete(labels=paste(DFkms_ymd$D , DFkms_ymd$M, DFkms_ymd$Y, sep="\n")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title = element_text(size=18, face="bold"), 
    axis.text = element_text(size=16),
    panel.grid.major.x = element_blank()
  ) + 
  ylab("Kms \n") +
  xlab("\n Ofensiva de Catalunya (en dies) - CTV Fletxes Blaves") + 
  ggtitle("Moviment i avanç del CTV en quilòmetres diaris")

# exportar
ggsave("img_kmsDiaris.svg", width=18, height=10)

# --- FI --- 
# --
# -


