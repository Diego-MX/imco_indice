# Diego Villamil, OPI
# CDMX, 2 de enero de 2017

library(broom)

itaee <- read_csv("../data/bie/processed/itaee.csv") %>% 
  select(trimestre = fecha, CVEENT, Estado, itaee = original)
cnbv <- read_csv("../data/cnbv/processed/por_estados.csv") %>% 
  select(trimestre, CVEENT, atm_1)

datos_regresion <- inner_join(itaee, cnbv, 
    by=c("trimestre", "CVEENT")) %>% 
  group_by(CVEENT) %>% arrange(trimestre) %>% 
  mutate_at(vars(itaee, atm_1), 
    funs(crec = . %>% divide_by(lag(.)) %>% subtract(1))) %>% 
  filter(trimestre < "2016-01-01")

formula_crec <- "itaee_crec ~ CVEENT + CVEENT:atm_1_crec + " %>% 
  str_c("lag(atm_1_crec, 4) - 1")
  

## Una linda grafiquilla. 

gg_crecimiento <- datos_regresion %>%
    rename(cajeros = atm_1) %>% 
    gather("Serie", "valor", itaee, cajeros, 
        factor_key = TRUE) %>% 
    group_by(Estado, Serie) %>% 
    mutate(valor_1 = valor/mean(valor)*100) %>%
  ggplot(aes(trimestre, valor_1)) + 
    facet_wrap(~ Estado, nrow = 5) + 
    geom_line(aes(color = Serie)) + 
    ggtitle("Crecimiento de actividad") + 
    labs(x = "", y = "") +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(angle=45, hjust=1), 
          legend.position = c(5/7, 1/7))
print(gg_crecimiento)

ggsave(plot = gg_crecimiento, 
       "../visualization/figures/crecimiento_actividad.png", 
       width = 16, height = 9, dpi = 100)


# Seguimos el modelo 

modelo_crec <- glm(data = datos_regresion, na.action = na.omit, 
    family=gaussian(), as.formula(formula_crec))

saveRDS(modelo_crec, "../models/modelo_crecimiento.rds")


# Guardamos proyecciones

modelo_crec <- readRDS("../models/modelo_crecimiento.rds")

muns_data <- read_csv("../data/cnbv/processed/por_municipio.csv") %>% 
  group_by(CVEMUN) %>% arrange(trimestre) %>% 
  mutate(CVEENT = str_sub(CVEMUN, 1, 2),
         atm_1 = atm_1 %>% replace(is.na(.), 0),
         atm_1_crec = if_else(lag(atm_1) == 0, 1.0,
             atm_1/lag(atm_1) - 1))

muns_fit <- augment(modelo_crec, newdata = muns_data) %>% 
  select(trimestre, CVEMUN, crec_fit = .fitted) %>% 
  mutate(CVEENT = str_sub(CVEMUN, 1, 2))


estado_fit <- augment(modelo_crec, data = datos_regresion) %>% 
  select(trimestre, CVEENT, Estado, 
         itaee, itaee_crec, crec_fit = .fitted) 


metro_data <- read_csv("por_zonas_metro.csv" %>% 
      file.path("../data/cnbv/processed", .)) %>%
  group_by(CVEENT, CVEMET) %>% arrange(trimestre) %>% 
  mutate(atm_1_crec = atm_1/lag(atm_1) - 1)

metro_fit <- augment(modelo_crec, newdata = metro_data) %>% 
  select(trimestre, CVEENT, CVEMET, 
         zona_metro = nombre_corto, crec_fit = .fitted)


write_csv(muns_fit, 
  "../data/resultados/crecimiento/por_municipio.csv")

write_csv(metro_fit, 
  "../data/resultados/crecimiento/por_zona_metro.csv")

write_csv(estado_fit, 
  "../data/resultados/crecimiento/por_estado.csv")














