# mapes-Catalunya
<p>L'objectiu d'aquest script és compartir un petit tutorial per crear mapes de Catalunya a R amb GGplot.</p>
<p>És recomanable que tinguis coneixements previs de R, però si no és el cas, igualment et podrà ser útil. </p>
<p>Per crear els mapes he utilitzat el paquet sf, que permet que tot el procés sigui més senzill i comprensible que altres opcions. La part més complicada ha estat 
trobar els shapefiles de les províncies, les comarques i els municipis. Els hauràs de descarregar i deixar tots els fitxers en una mateixa carpeta:</p>
<ul><li><a href="https://analisi.transparenciacatalunya.cat/en/Urbanisme-infraestructures/L-mits-administratius-provincials-de-Catalunya/ghr8-wp3h" target="_blank">
Shapefiles de les províncies de Catalunya</a></li>
<li><a href="https://www.icgc.cat/Administracio-i-empresa/Descarregues/Capes-de-geoinformacio/Base-municipal">Shapefile dels municipis, comarques i províncies de Catalunya</a></li>
</ul>
<p>Comencem carregant els paquets. Ja saps, si no els tens instal·lats, utilitza la primera línia. Si no, passa directament a les següents: </p>
``` r
install.packages("sf")
library(sf)
library(tidyverse)
library(readxl)
library(viridis)
library(patchwork)
```