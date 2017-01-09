# Diego Villamil, OPI
# CDMX, a 16 de diciembre de 2016. 


filter <- dplyr::filter


aplicar_crecimiento <- function(base, crec, columnas) {
  # base <- metros_0; crec <- metros_crec; 
  # columnas <- c("CVEENT", "CVEMET", "zona_metro")
  # base <- muns_0; crec <- muns_crec; columnas <- c("CVEMUN")
  
  datos <- inner_join(base, crec, by=columnas) %>% 
    mutate(acteco = NA) %>% 
    arrange_(.dots = columnas %>% c("trimestre"))
  calc_crec <- datos %>% 
    filter(!is.na(crec_fit)) %>% 
    group_by_(.dots = columnas) %>% 
    mutate(es_2014 = (trimestre >= "2014-12-01") & 
                 (lag(trimestre) < "2014-12-01"), 
        crec_acum = cumprod(1 + crec_fit),
        crec_14 = sum(crec_acum[es_2014]) %>% replace(. == 0, 1), 
        acteco = ae_175*crec_acum/crec_14)

  datos[!is.na(datos$crec_fit), "acteco"] <- calc_crec$acteco
  
  return (datos)
}


## Por zonas metropolitanas. 

metros_0 <- read_csv("por_zonas_metro.csv" %>% 
      file.path("../data/resultados/acteco", .)) %>%
  select(CVEMET, CVEENT, zona_metro, ae_175)

metros_crec <- read_csv("por_zona_metro.csv" %>% 
  file.path("../data/resultados/crecimiento", .))

metros_eco <- aplicar_crecimiento(metros_0, metros_crec, 
  c("CVEENT", "CVEMET", "zona_metro"))

write_csv(metros_eco %>% select(-ae_175), 
  "../data/resultados/integrado/por_zonas_metro.csv")

metros_eco_2 <- metros_eco %>%
  group_by(trimestre, CVEMET, zona_metro) %>% 
  summarize(acteco = sum(acteco, na.rm = T))

write_csv(metros_eco_2, "../data/resultados/integrado/por_zonas_metro_.csv")



## Por municipios. 

muns_0 <- read_csv("por_municipio.csv" %>% 
      file.path("../data/resultados/acteco", .)) %>%
  select(CVEMUN, nombre, ae_175)

muns_crec <- read_csv("por_municipio.csv" %>% 
  file.path("../data/resultados/crecimiento", .), 
  col_types = "Dcdc") 

muns_eco <- aplicar_crecimiento(muns_0, muns_crec, "CVEMUN")

write_csv(muns_eco, "../data/resultados/integrado/por_municipio.csv")


## Por estado. 

edo_0 <- read_csv("../data/bie/processed/pibe.csv") %>% 
  filter(año == "2014-12-01") %>% rename(ae_175 = pibe)

edo_crec <- read_csv("../data/resultados/crecimiento/por_estado.csv")

edo_eco <- aplicar_crecimiento(edo_0, edo_crec, c("CVEENT", "Estado"))

write_csv(edo_eco, "../data/resultados/integrado/por_estado.csv")




