# Diego Villamil, OPI
# CDMX, 13 de diciembre de 2016
# modificado 4 de enero de 2017

# Este script es para hacer sanity checks de las estimaciones de 
# índice metropolitano. 
# Para cada estado, comparar:  
#   PIBE
#   ITAEE rescalado
#   act_eco del estado
#   act_eco de las ciudades del estado


metros_ref <- read_csv("zonas_metro_estado_ok.csv" %>% 
    file.path("../data/referencias", .)) %>% 
    select(CVEMET, zona_metro = nombre_corto, CVEMUN, CVEENT)


muns_ae <- read_csv("../data/resultados/integrado/por_municipio.csv") %>%
  select(trimestre, CVEMUN, acteco) %>% 
  filter(CVEMUN %in% metros_ref$CVEMUN)
  
metros_ae <- read_csv("por_zonas_metro.csv" %>% 
  file.path("../data/resultados/integrado", .)) %>% 
  select(trimestre, CVEENT, CVEMET, zona_metro, acteco)


pibe <- read_csv("../data/bie/processed/pibe.csv") %>% 
  rename(trimestre = año)
itaee <- read_csv("../data/bie/processed/itaee.csv") %>% 
  select(trimestre = fecha, Estado, CVEENT, itaee = original)

estado_ae <- read_csv("../data/resultados/integrado/por_estado.csv") %>% 
  select(trimestre, Estado, CVEENT, acteco)

junta_columnas <- c("trimestre", "CVEENT", "Estado")
estado_totales <- pibe %>% 
  right_join(itaee, by = junta_columnas) %>% 
  inner_join(estado_ae, by = junta_columnas) %>% 
  inner_join(by = c("trimestre", "CVEENT"), metros_ae %>% 
      group_by(trimestre, CVEENT) %>% 
      summarize(por_metros = sum(acteco, na.rm=T))) %>% 
  inner_join(by = c("trimestre", "CVEENT"), muns_ae %>% 
      mutate(CVEENT = str_sub(CVEMUN, 1, 2)) %>% 
      group_by(trimestre, CVEENT) %>% 
      summarize(por_muns = sum(acteco, na.rm=T))) %>% 
  group_by(CVEENT, Estado) %>% 
  mutate(es_2014 = (trimestre == "2014-12-01"), 
      itaee_al_pib = itaee/itaee[es_2014]*pibe[es_2014])
  

gg_sanity <- estado_totales %>% 
    gather("índice", "valor", factor_key = TRUE, 
        pibe, itaee_al_pib, por_metros) %>% 
    filter(!is.na(valor)) %>% 
  ggplot(aes(trimestre, valor, color = índice)) + 
    facet_wrap( ~ Estado, scales = "free_y", nrow = 5) + 
    geom_line() + 
    ggtitle("Actividad Económica") +
    scale_x_date("") +
    scale_y_continuous("", labels = . %>% divide_by(1e3)) + 
  # coord_cartesian(xlim = c("2013-10-01", "2015-10-01") %>% as.Date) +
    scale_color_brewer(palette = "Dark2") +
    expand_limits(y = 0) +
    theme(axis.text.x = element_text(angle=45, hjust=1), 
          legend.position = c(5/7, 1/10))

print(gg_sanity)
ggsave(plot = gg_sanity, "../visualization/comprobación_modelo_35.png",
  width = 16, height = 9, dpi = 100)
















