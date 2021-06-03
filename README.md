# mapes-Catalunya
<p>L'objectiu d'aquest script és compartir un petit tutorial per crear mapes de Catalunya a R amb GGplot.</p>
<p>És recomanable que tinguis coneixements previs de R, però si no és el cas, igualment et podrà ser útil. </p>
<p>Per crear els mapes he utilitzat el paquet sf, que permet que tot el procés sigui més senzill i comprensible que altres opcions. La part més complicada ha estat 
trobar els shapefiles de les províncies, les comarques i els municipis. Els hauràs de descarregar i deixar tots els fitxers en una mateixa carpeta:</p>
<ul><li> <a href="https://analisi.transparenciacatalunya.cat/en/Urbanisme-infraestructures/L-mits-administratius-provincials-de-Catalunya/ghr8-wp3h" target="_blank">
Shapefiles de les províncies de Catalunya</a></li>
<li> <a href="https://www.icgc.cat/Administracio-i-empresa/Descarregues/Capes-de-geoinformacio/Base-municipal">Shapefile dels municipis, comarques i províncies de Catalunya</a></li>
</ul>
<p>Comencem carregant els paquets. Ja saps, si no els tens instal·lats, utilitza la primera línia. Si no, passa directament a les següents: </p>
<pre>
install.packages("sf")
library(sf)
library(tidyverse)
library(viridis)
library(patchwork)
</pre>
<p>He utilitzat una funció per calcular els percentatges sobre la població</p>
<pre>
percent <- function(first, second) {
  perc <- round(first / second * 100,1)
  return (perc)
}
</pre>
<p>
## Mapa provincial
<p>Creem un dataframe amb el shapefile de les provincies.</p>
<pre>
provincies <- st_read("/dades/mapes/provincies-Catalunya/geo_export_418ccb84-c6f7-468b-8dd2-ab8f1b8ae2bb.shp")
</pre>
<p>Dibuixar un mapa en blanc amb els límits de les províncies és tan senzill com: </p>
<pre>
ggplot(provincies) +
geom_sf()
</pre>
<p>Ara ho compliquem una mica més i afegim el nom de les províncies, així com un tema que esborra les línies de fons.</p>
<pre>
ggplot(provincies) +
  geom_sf() +
  geom_sf_text(aes(label=nom_prov)) +
  theme_void()
  </pre>
<p>Pintem un mapa de coropletes en funció d'una variable.</p>
<pre>
ggplot(provincies) +
  geom_sf(aes(fill=sup_prov)) +
  geom_sf_text(aes(label=nom_prov)) +
  theme_void()
</pre>
<p>Per acabar, afegim un títol, subtítol, caption, llegenda i canviem alguns estils.</p>
<pre>
ggplot(provincies) +
  geom_sf(aes(fill=sup_prov)) +
  geom_sf_text(aes(label=nom_prov), color="white") +
  theme_void() +
  scale_fill_gradient2(guide=guide_colorsteps(label.vjust=-1)) +
  labs(title="Superfície de les províncies catalanes",
     subtitle="Exemple de mapa provincial",
     caption="Font de les dades: ",
     fill="Superfície en m2") 
</pre>
## Mapa comarcal
<p>Per crear un mapa comarcal, el funcionament és similar. L'única diferència és que com el mapa té més detall, trigarà més a carregar.</p>
<p>De tots els fitxers shapefiles descarregats, el que necessitaràs és el que s'anomena bm5mv21sh0tpc1_20200601_0.shp. </p>
<pre>
comarques <- st_read("dades/mapes/comarques-Catalunya/bm5mv21sh0tpc1_20200601_0.shp")
</pre>
<p>En l'exemple anterior, hem pintat el mapa en funció d'una variable continguda al mateix dataframe del mapa, però no és l'habitual. Normalmente tindràs un o diversos 
dataframes amb les dades, i un altre amb els shapefiles. Treballaràs les dades i quan ja les tinguis, fusionaràs el dataframe de les dades i el dels shapefiles. Per fer-ho, necessites que tots dos dataframes comparteixin una columna amb valors idèntics, habitualment el codi comarcal, el codi municipal, etc. (millor un valor numèric, ja que 
els textos admeten més variacions i si els camps no són exactament idèntics, obtindràs valors NA. </p>
<p>En el meu exemple he treballat amb les xifres de vacunació. He calculat el percentatge de població vacunada amb una dosi i amb la pauta completa, en relació a la població de la comarca. </p>
<pre>
df_vacunes <- read_csv("../../dades/vacunacio/Vacunaci__per_al_COVID-19__persones_vacunades_per_comarca.csv", 
                       col_types = cols(DATA = col_date(format = "%d/%m/%Y")))
</pre>
<p>T'estalvio els passos intermitjos, que pots consultar a l'Script, i vaig directament a com fusionar els dataframes finals. </p>
<pre>mapa_vacunes <- left_join(comarques, df_vacunes_resum, by=c("CODICOMAR"="COMARCA_CODI")) </pre>
<p>Fusionem el dataframe "comarques" i el dataframe "df_vacunes_resum", que és el que té les dades que vull pintar, per la columna que en un dataframe es diu "CODICOMAR" i en l'altre dataframe, "COMARCA_CODI", que conté valors idèntics. </p>
<p>Pinto el mapa en funció del valor de percentatge: 
<pre>
ggplot(mapa_vacunes, aes(fill=Perc_1dosi)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  theme(plot.subtitle=element_text(hjust=.5)) +
  scale_fill_viridis(option="mako", direction=-1, breaks=c(30,35,40,45), 
                       labels=function(x) paste0(x, "%"), gui=guide_colorsteps(show.limits=TRUE)) +
  labs(subtitle="Població que ha rebut almenys una dosi",
       caption="Font: Departament de Salut. Generalitat de Catalunya\nActualitzat: 3/06/2021",
       fill="") 
  </pre>
<p>En el meu script he creat un segon mapa amb el percentatge de població que ha rebut la pauta completa i he mostrat un mapa al costat de l'altre. No he utilitzat facet_wrap, ja que interessava que l'escala de la llegenda fos diferent en cadascun dels mapes. </p>

## Mapa municipal
<p>De tots els fitxers shapefiles descarregats, necessitarem el que es diu bm5mv21sh0tpm1_20200601_0.shp.</p>
<pre>
municipis <- st_read("dades/mapes/municipis-Catalunya/bm5mv21sh0tpm1_20200601_0.shp")
</pre>
<p>Com que els codis municipals d'aquest dataframe tenen un dígit de més, l'eliminem. </p>


