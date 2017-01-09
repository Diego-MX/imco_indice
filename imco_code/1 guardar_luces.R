# Diego Villamil, OPI
# CDMX, 15 de diciembre de 2016

# 1. Importar SHAPEFILES del INEGI de Municipios y Localidades Urbanas. 
# 2. Importar RASTER de NOAA con la foto de luminosidad. 
# 3A La herramienta de RASTER CALCULATOR se usa para modificar los pixeles
#    de luminosidad >= 175.  
# 3B Utiliza ZONAL STATISTICS para resumir los 
#    datos del raster en los shapefiles.  
#    Si el Zonal Statistics aparece en blanco, se corrigen las 
#    proyecciones del shapefile y ráster para que coincidan. 
# 4. Continuar con este script. 


library(rgdal)

areas_shape <- function (shape) {  # Los applies se ven complicators.
  # Shape es una estructura, de listas de estructuras.
  areas_ <- shape@polygons %>% 
    lapply(. %>% slot("Polygons") %>% 
      sapply(. %>% slot("area")) ) %>% 
    sapply(sum)
  return (areas_) }

datos_shape <- function (shape, nivel = "municipio") {  # 
  # shape = locs_shp; nivel = "localidad"
  # shape = muns_shp; nivel = "municipio"
  datos_ <- shape@data %>% 
    mutate(area = areas_shape(shape)) %>% 
    rename(x175 = X175_sum)
  switch (nivel, 
    estado = {
      datos = datos_ %>% 
        mutate(CVEGEO = str_c(CVE_ENT)) %>% 
        select(-c(CVE_ENT)) %>% 
        rename(nombre = NOM_ENT)
    },
    municipio = { 
      datos = datos_ %>% 
        mutate(CVEGEO = str_c(CVE_ENT, CVE_MUN)) %>% 
        select(-c(CVE_ENT, CVE_MUN)) %>% 
        rename(nombre = NOM_MUN)
    },
    localidad = { 
      datos = datos_ %>% 
        mutate(CVEGEO = str_c(CVE_ENT, CVE_MUN, CVE_LOC)) %>% 
        select(-c(CVE_ENT, CVE_MUN, CVE_LOC)) %>% 
        rename(nombre = NOM_LOC)
    })
  return (datos)
}


# 1. Leer Shapefiles

edos_shp <- readOGR("../data/inegi/marco_geo/processed", 
  "datos_estados",     stringsAsFactors = FALSE)
muns_shp <- readOGR("../data/inegi/marco_geo/processed", 
  "datos_municipios",  stringsAsFactors = FALSE)
locs_shp <- readOGR("../data/inegi/marco_geo/processed", 
  "datos_localidades", stringsAsFactors = FALSE)


# 2. Sacar Datos

edos_data <- datos_shape(edos_shp, "estado") %>% 
  rename(CVEENT = CVEGEO) %>% 
  select(CVEENT, nombre, LUMEN = luz_sum, area, x175)

muns_data <- datos_shape(muns_shp, "municipio") %>% 
  rename(CVEMUN = CVEGEO) %>% 
  select(CVEMUN, nombre, LUMEN = luz_sum, area, x175)

locs_data <- datos_shape(locs_shp, "localidad") %>% 
  rename(CVELOC = CVEGEO) %>% 
  select(CVELOC, nombre, LUMEN = luz_sum, area, x175) 


# 3. Juntar datos. 

municipios_datos <- muns_data %>% 
  left_join(by = "CVEMUN", locs_data %>% 
    mutate(CVEMUN = str_sub(CVELOC, 1, 5)) %>% 
    group_by(CVEMUN) %>% 
    summarize_at(vars(LUMEN, area, x175), funs(loc = sum))
  ) 
    
estados_datos <- edos_data %>% 
  left_join(by = "CVEENT", locs_data %>% 
    mutate(CVEENT = str_sub(CVELOC, 1, 2)) %>% 
    group_by(CVEENT) %>% 
    summarize_at(vars(LUMEN, area, x175), funs(loc = sum))
  )


# 4. Escribir tablas

write_csv(locs_data, "../data/agrupadas/locs_luces_175.csv")

write_csv(municipios_datos, "../data/agrupadas/mun_luces_175.csv")

write_csv(estados_datos, "../data/agrupadas/edos_luces_175.csv")





