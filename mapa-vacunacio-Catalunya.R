#Carreguem les llibreries
library(tidyverse)
library(sf)
library(viridis)
library(readxl)
library(patchwork)

#Creem una funció que calcularà els percentatges amb un decimal
percent <- function(first, second) {
  perc <- round(first / second * 100,1)
  return (perc)
}

# MAPA PROVINCIAL --------------------------------------------------

#Per crear el mapa de les províncies catalanes hem baixat els fitxers shapefile de
#https://analisi.transparenciacatalunya.cat/en/Urbanisme-infraestructures/L-mits-administratius-provincials-de-Catalunya/ghr8-wp3h
#Es important que tots els fitxers que acabem de descarregar estiguin a la mateixa carpeta

#Creem un dataframe amb el shapefile de les províncies
provincies <- st_read("../../dades/mapes/provincies-Catalunya/geo_export_418ccb84-c6f7-468b-8dd2-ab8f1b8ae2bb.shp")

#Dibuixem un mapa en blanc
ggplot(provincies) +
  geom_sf()

#Dibuixem un mapa amb els noms de les províncies i afegim un tema
ggplot(provincies) +
  geom_sf() +
  geom_sf_text(aes(label=nom_prov)) +
  theme_void()

#Pintem el mapa en funció d'una variable, que en aquest exemple és la superf?cie de cada província
ggplot(provincies) +
  geom_sf(aes(fill=sup_prov)) +
  geom_sf_text(aes(label=nom_prov)) +
  theme_void()

#Afegim el títol, subtítol, caption, llegenda i canviem alguns estils
ggplot(provincies) +
  geom_sf(aes(fill=sup_prov)) +
  geom_sf_text(aes(label=nom_prov), color="white") +
  theme_void() +
  scale_fill_gradient2(guide=guide_colorsteps(label.vjust=-1)) +
  labs(title="Superf?cie de les prov?ncies catalanes",
     subtitle="Exemple de mapa provincial",
     caption="Font de les dades: ",
     fill="Superf?cie en m2") 


# MAPA COMARCAL -----------------------------------------------------------

#Per al mapa de les comarques hem descarregat el shapefile de
#https://www.icgc.cat/Administracio-i-empresa/Descarregues/Capes-de-geoinformacio/Base-municipal
#Guardem tots els fitxers a la mateixa carpeta

#Creem un dataframe amb el shapefile de les comarques
comarques <- st_read("../../dades/mapes/comarques-Catalunya/bm5mv21sh0tpc1_20200601_0.shp")

#Dibuixem un mapa amb els noms de les comarques
ggplot(comarques) +
  geom_sf() +
  geom_sf_text(aes(label=NOMCOMAR)) +
  theme_void()

#Obtenim les dades de vacunació per comarques de
# https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-persones-vacunades-per-co/k7ta-zd3e
# La 'població diana' fa referència a la població amb dret a rebre assistència sanitària de finançament públic a Catalunya, 
#identificades en el Registre central de població del CatSalut (RCA), i calculada en la data de referència de les dades,
#pel que poden no coincidir exactament amb les dades de l'IDESCAT.

#Creem un dataframe amb les dades
df_vacunes <- read_csv("../../dades/vacunacio/Vacunaci__per_al_COVID-19__persones_vacunades_per_comarca.csv", 
                       col_types = cols(SEXE = col_factor(levels = c("Home", "Dona")), 
                                        DATA = col_date(format = "%d/%m/%Y")))

#Resumim les dades que utilitzarem al mapa
df_vacunes_resum <- df_vacunes %>%
  group_by(COMARCA_CODI) %>%
  summarize(Vacunacio_1dosi=sum(VACUNACIO_INICIADA),Vacunacio_completada=sum(VACUNACIO_COMPLETADA),
            Poblacio=sum(POBLACIO_DIANA)) 

#Creem columnes amb els percentatges
df_vacunes_resum$Perc_1dosi = percent(df_vacunes_resum$Vacunacio_1dosi, df_vacunes_resum$Poblacio)
df_vacunes_resum$Perc_completa = percent(df_vacunes_resum$Vacunacio_completada, df_vacunes_resum$Poblacio)

#Fusionem els dataframes per la columna codi comarca 
mapa_vacunes <- left_join(comarques, df_vacunes_resum, by=c("CODICOMAR"="COMARCA_CODI"))   

#Dibuixem el mapa de persones amb almenys una dosi (pot trigar a carregar)
p1 <- ggplot(mapa_vacunes, aes(fill=Perc_1dosi)) +
  geom_sf(color="white", size=.1) +
#Mostrem etiquetes amb les comarques que tenen menys del 34% i m?s del 46% de poblaci? vacunada   
  geom_sf_text(data=filter(mapa_vacunes, Perc_1dosi<34), aes(label=paste(NOMCOMAR,"\n", Perc_1dosi,"%")), size=2) +
  geom_sf_text(data=filter(mapa_vacunes, Perc_1dosi>46), aes(label=paste(NOMCOMAR, "\n", Perc_1dosi, "%")), size=2,
               color="#CCCCCC") +
  theme_void() +
  theme(plot.subtitle=element_text(hjust=.5)) +
  scale_fill_viridis(option="mako", direction=-1, breaks=c(30,35,40,45),
                       labels=function(x) paste0(x, "%"), guide=guide_colorsteps(show.limits=TRUE)) +
  labs(subtitle="Població que ha rebut almenys una dosi",
       caption="Font: Departament de Salut. Generalitat de Catalunya\nActualitzat: 3/06/2021",
       fill="") 

#Dibuixem el mapa de persones que han rebut la pauta completa
p2 <- ggplot(mapa_vacunes, aes(fill=Perc_completa)) +
  geom_sf(color="white", size=.1) +
  #Mostrem etiquetes amb les comarques que tenen menys del 18% i més del 28% de població vacunada   
  geom_sf_text(data=filter(mapa_vacunes, Perc_completa<18), aes(label=paste(NOMCOMAR,"\n",Perc_completa,"%")),size=2) +
  geom_sf_text(data=filter(mapa_vacunes, Perc_completa>28), aes(label=paste(NOMCOMAR, "\n", Perc_completa, "%")), 
               color="#CCCCCC", size=2) +
  theme_void() +
  theme(plot.subtitle=element_text(hjust=.5)) +
  scale_fill_viridis(option="mako",direction=-1,breaks=c(15,20,25,30), 
                       labels=function(x) paste0(x, "%"), guide=guide_colorsteps(show.limits=TRUE)) +
  labs(title="",
       subtitle="Població que ha rebut la pauta completa",
       caption="\nAutora: @evaac",
       fill="")

#Mostrem els dos mapes
mapes <- p1 + p2 + plot_annotation(title="Percentatge de població vacunada contra la COVID-19 a Catalunya", 
                                       theme = theme(plot.title = element_text(face="bold", size=18, hjust=.5)))

#Els guardem com a png
ggsave("../../mapes/mapa-vacunacio-comarques-Cat.png", mapes, height=5, width=9)



# MAPA MUNICIPAL ----------------------------------------------------------

# Utilitzem un altre dels shapefiles disponibles a
# #https://www.icgc.cat/Administracio-i-empresa/Descarregues/Capes-de-geoinformacio/Base-municipal

#Creem un dataframe amb el shapefile dels municipis
municipis <- st_read("../../dades/mapes/municipis-Catalunya/bm5mv21sh0tpm1_20200601_0.shp")

#Dibuixem un mapa en blanc
ggplot(municipis) +
  geom_sf() +
  theme_void()

#Els codis municipals del shapefile tenen 6 dígits, sobra l'últim
municipis$CODIMUN <- str_sub(municipis$CODIMUNI,1,5)

#Descarreguem les dades de vacunació per municipi
# https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-dosis-administrades-per-m/irki-p3c7

#Creem un dataframe de dosis de vacuna administrades
df_vacuna_mun <- read_csv("../../dades/vacunacio/Vacunaci__per_al_COVID-19__dosis_administrades_per_municipi.csv")

#Creem una columna amb les pautes de la vacuna completades, és a dir, que han rebut les dues dosis o una vacuna monodosi
df_vacuna_mun$PautaCompleta <- ifelse(df_vacuna_mun$FABRICANT == "J&J / Janssen" |
                                   (df_vacuna_mun$FABRICANT != "J&J / Janssen" & df_vacuna_mun$DOSI==2), 
                                   df_vacuna_mun$RECOMPTE, 0)
#Creem una columna condicional amb els que han rebut una dosi
df_vacuna_mun$Dosi1 <- ifelse(df_vacuna_mun$DOSI==1, df_vacuna_mun$RECOMPTE, 0)

#Creem un dataframe de resum amb els que han rebut almenys 1 una dosi i excloem els no vacunats
vacunes_mun_resum <- df_vacuna_mun %>%
  filter(is.na(NO_VACUNAT)) %>%
  group_by(MUNICIPI_CODI, MUNICIPI) %>%
  filter(!is.na(DOSI)) %>%
  summarize(PautaCompleta=sum(PautaCompleta), Dosi1=sum(Dosi1))

#Per calcular els percentatges, descarreguem les dades de població del CatSalut
#https://analisi.transparenciacatalunya.cat/Salut/Registre-central-de-poblaci-del-CatSalut-poblaci-p/7yq2-acdk

#Creem el dataframe de població
poblacio_mun <- read_csv("../../dades/poblacio/Registre_central_de_poblaci__del_CatSalut__poblaci__per_municipi.csv")

#Convertim els noms de les columnes a noms sintàcticament vàlids
colnames(poblacio_mun) <- make.names(colnames(poblacio_mun))

#Creem un dataframe de resum on filtrem els majors de 16 anys
poblacio_muni <- poblacio_mun %>%
  filter(edat>=16, any==2021) %>%
  group_by(codi.municipi, municipi) %>%
  summarize(Majors16=sum(població.oficial)) 

#Esborrem els dataframes que no necessitem
rm(poblacio_mun, df_vacuna_mun)

#Fusionem els dataframes
vacunes_mun_pob <- left_join(vacunes_mun_resum, poblacio_muni, by=c("MUNICIPI_CODI" = "codi.municipi"))

#Calculem els percentatges amb la funció que hem creat abans
vacunes_mun_pob$Perc_vacunat <- percent(vacunes_mun_pob$PautaCompleta, vacunes_mun_pob$Majors16)
vacunes_mun_pob$Perc_1dosi <- percent(vacunes_mun_pob$Dosi1, vacunes_mun_pob$Majors16)

#Fusionem els dataframes
mapa_mun <- left_join(municipis, vacunes_mun_pob, by=c("CODIMUN"="MUNICIPI_CODI"))

#Esborrem els dataframes sobrers
rm(municipis, poblacio_muni, vacunes_mun_resum)

#Dibuixem el mapa del percentatge que ha rebut almenys una dosi
p3 <- ggplot(mapa_mun, aes(fill=Perc_1dosi)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  theme(plot.subtitle=element_text(hjust=.3, margin=margin(10,0,10,0))) +
  scale_fill_viridis(option="mako", direction=-1, 
                     labels=function(x) paste0(x, "%"), guide=guide_colorsteps(show.limits=TRUE)) +
  labs(subtitle="Població que ha rebut almenys una dosi",
       caption="Font: Departament de Salut. Generalitat de Catalunya\nActualitzat: 3/06/2021",
       fill="") 

#Dibuixem el mapa del percentatge que ha rebut la pauta completa
p4 <- ggplot(mapa_mun, aes(fill=Perc_vacunat)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  theme(plot.subtitle=element_text(hjust=.5)) +
  scale_fill_viridis(option="mako", direction=-1, 
                     labels=function(x) paste0(x, "%"), guide=guide_colorsteps(show.limits=TRUE)) +
  labs(title="",
       subtitle="Població que ha rebut la pauta completa",
       caption="Autora: @evaac",
       fill="") 

#Mostrem els dos mapes
patchwork <- p3 + p4 + plot_annotation(title="Percentatge de població vacunada contra la COVID-19 a Catalunya", 
                            theme = theme(plot.title = element_text(face="bold", size=18, hjust=.5)))
                                
#Els guardem com a png
ggsave("../../mapes/mapa-vacunacio-municipis-Cat.png", patchwork, height=5, width=9)

