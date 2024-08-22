
# Geospatial-Data-Learning-process
This is a my learning process of Geospatial Data. In this tutorial, I´m following the codes, just changing some variables, provided by the book "Analises Ecologicas no R" (Da Silva FR, Gonçalves-Souza T, Paterno GB, Provete DB, Vancine MH. 2022. Análises ecológicas no R. Nupeea : Recife, PE, Canal 6 : São Paulo. 640 p. ISBN 978-85-7917-564-0.)


# Teste para estudo de Dados Geoespaciais #

# Primeiro os pacotes:
    install.packages(c("ade4", "adespatial", "ape", "bbmle", "betapart",
                   "BiodiversityR", "car", "cati", "datasauRus", "devtools", "DHARMa", "dplyr",
                   "ecolottery", "emmeans", "factoextra", "FactoMineR", "fasterize", "FD",
                   "forcats", "geobr", "generalhoslem", "GGally", "ggExtra", "ggforce",
                   "ggplot2", "ggpubr", "ggrepel", "ggspatial", "glmmTMB", "grid", "gridExtra",
                   "here", "hillR", "iNEXT", "janitor", "kableExtra", "knitr", "labdsv",
                   "lattice", "leaflet", "lmtest", "lsmeans", "lubridate", "mapview", "MASS",
                   "MuMIn", "naniar", "nlme", "ordinal", "palmerpenguins", "performance",
                   "pez", "phyloregion", "phytools", "picante", "piecewiseSEM", "purrr",
                   "pvclust", "raster", "readr", "reshape2", "rgdal" , "Rmisc", "rnaturalearth",
                   "RVAideMemoire", "sciplot", "sf", "sidrar", "sjPlot", "spData", "spdep",
                   "stringr", "SYNCSA", "tibble", "tidyr", "tidyverse", "tmap", "tmaptools",
                   "TPD", "vcd", "vegan", "viridis", "visdat", "mvabund", "rdist", "udunits2"),
                 dependencies = TRUE)
  
  
# Test with Vector type data 
  # dados vetoriais de polígonos do mundo
     data("world")  
    mundo= plot(world[1], col = viridis::viridis(100), main = "Mapa do mundo") 
  
# Raster: consistem em um matriz em que os elementos representam células, geralmente igualmente espaçadas;
    volcano[1:5, 1:5]
    raster_layer <- raster::raster(volcano)

    #class      : RasterLayer 
    #dimensions : 87, 61, 5307  (nrow, ncol, ncell)
    #resolution : 0.01639344, 0.01149425  (x, y)
    #extent     : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
    #crs        : NA 
    #source     : memory
    #names      : layer 
    #values     : 94, 195  (min, max)

# Básico com dados em Raster 
    raster_layer#esses dados são de topografia do vulcão Maunga na Nova zelândia
    plot(raster_layer, col= viridis::magma( n=100))
  
# Raster_Brick:
    raster_layer_1<- raster_layer
    raster_layer2<- raster_layer * raster_layer  
    raster_layer_3 <- sqrt(raster_layer)  
    raster_layer_4 <- log10(raster_layer)  

    raster_brick <- raster::brick(raster_layer_1, raster_layer2,
                              raster_layer_3, raster_layer_4)

    Gráfico_RasterBrick<- plot(raster_brick, col=viridis::magma(n=25), main = "RasterBrick Volcano")
  
#  Raster_Stack:
    raster_stack <- raster::stack(raster_layer_1, raster_layer2, raster_layer_3, raster_layer_4)

    Gráfico_RasterStack= plot(raster_stack, col= viridis::magma (n=25), main="RasterStack Vulcão")
  
  
#  Utilizando dados de vetor:
    dir.create(here::here("dados"))
    dir.create(here::here("dados","vetor"))

    options(timeout = 1e3)

  
    
#  Dados de nascentes em ITUPEVA
    for(i in c(".dbf",".prj",".shp",".shx")){
    download.file(url = paste0("https://geo.fbds.org.br/SP/ITUPEVA/HIDROGRAFIA/SP_3524006_NASCENTES", i),
                                     destfile= here::here("dados","vetor",
                                                           paste0("SP_3524006_NASCENTES", i)),
                                     mode="wb")
                                     
                                     }
 # Dados de Linhas de Hidrografia
    for(i in c(".dbf",".prj",".shp",".shx")){
    download.file(url = paste0("https://geo.fbds.org.br/SP/ITUPEVA/HIDROGRAFIA/SP_3524006_RIOS_SIMPLES", i),
                                     destfile= here::here("dados","vetor",
                                                          paste0("SP_3524006_RIOS_SIMPLES", i)),
                                     mode="wb")       
    }
 
 # Polígonos de cobertura de terra
    for(i in c(".dbf",".prj",".shp",".shx")){
    Cobertura_Terra<- download.file(url = paste0("https://geo.fbds.org.br/SP/ITUPEVA/USO/SP_3524006_USO", i),
                                     destfile= here::here("dados","vetor",
                                                          paste0("SP_3524006_USO", i)),
                                     mode="wb")


    }



# Nascentes-Itupeva
    geo_vetor_itupeva_nascentes<- sf::st_read(
    here::here("dados","vetor","SP_3524006_NASCENTES.shp"), quiet = TRUE)
    Nascentes_Itupeva<-plot(geo_vetor_itupeva_nascentes[1], pch=20, col="blue", main=NA,
     axes=TRUE, graticule=TRUE)
  
# Hidrografia-Itupeva
    vetor_itupeva_hidrografia <- sf::st_read (here::here("dados","vetor","SP_3524006_RIOS_SIMPLES.shp"),
                                        quiet=TRUE)
    Hidrografia_Itupeva<-plot(vetor_itupeva_hidrografia[1], col="steelblue", main="Hidrografia Itupeva",
     axes= TRUE, graticule= TRUE)

# Cobertura da terra-Itupeva
    vetor_itupeva_cobertura <- sf::st_read(here::here("dados","vetor","SP_3524006_USO.shp"),
                                      quiet = TRUE)
    unique_classes <- unique(vetor_itupeva_cobertura$CLASSE_USO)
    view(unique_classes)

    plot(vetor_itupeva_cobertura [6],
     col = c("blue", "orange", "white", "forestgreen", "green","red"),
     main = NA, axes = TRUE, graticule = TRUE)
    legend(x = .7, y = .3, pch = 15, cex = .7, pt.cex = 2.5,
       legend = (unique_classes),
       col = c("blue", "orange", "white", "forestgreen", "green","red"))

# Polígonos do limite do município de Itupeva:
    geo_vetor_Itupeva <- geobr::read_municipality(code_muni= 3524006, 
                                              year = 2022, showProgress = FALSE)

    plot(geo_vetor_Itupeva[1], col="blue", main=NA, axes=TRUE, graticule=TRUE)


# Polígonos do limite do Brasil:
    Brasil<-ecodados::geo_vetor_brasil_anos
    Brasil2<- ecodados::geo_vetor_brasil

    plot(Brasil[2], col="blue",main= NA, axes= TRUE, graticule=TRUE)
    plot(geo_vetor_brasil[1], col="blue", main=NA, axes= TRUE, graticule=TRUE)


# Utilizando dados de Raster
# Para exportar, podemos usar raster::raster(); raster::brick(); raster::stack().
    dir.create(here::here("dados","raster"))
    options(timeout = 1e3)
    setwd(~ Artigos Aleatórios)
    unzip(zipfile = ("srtm_27_17.zip"))
  

    download.file(url = "https://srtm.csi.cgiar.org/wp-content/uploads/files/
    srtm_5x5/TIFF/srtm_27_17.zip",
              destfile = here::here("dados", "raster", "srtm_27_17.zip"),
              mode = "wb")

# Realizando mapas de Bioma do Brasil####

    geo_vetor_amazonia <- geobr::read_amazon(year = 2012, simplified = TRUE, showProgress = TRUE) %>%
      dplyr::

    geo_vetor_biomas<- ecodados::geo_vetor_biomas



    plot(geo_vetor_biomas)

    plot(geo_vetor_biomas$geom,
     col= c("darkgreen", "orange1","orange3","lightgreen",
            "yellow","purple4"),
     main= "Brazilian Biomes", axes = TRUE, graticule = TRUE)

    legend(x= -37, y=-20, pch=15, cex=.7, pt.cex=2.5,
       legend =c("Amazon Rainforest","Caatinga","Cerrado","Atlantic Forest", "Pampa","Pantanal"),
       col = c("darkgreen", "orange1","orange3","lightgreen",
                 "yellow","purple4"))


    plot(geo_vetor_amazonia$geom,
     col= c("Darkgreen"),
     main= "Amazon Rainforest", axes =TRUE, graticule= TRUE)
    legend(x=-20, y=-30, cex=.7, pt.cex = 2.5,
       legend = c("Amazon Rainforest"),
       col = c("darkgreen"))


