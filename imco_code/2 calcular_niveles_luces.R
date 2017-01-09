# Diego Villamil, OPI
# CDMX, 15 de diciembre de 2016


pibe_14 <- read_csv("../data/bie/processed/pibe.csv") %>% 
  filter(año == "2014-12-01") %>% select(CVEENT, pibe)

muns_acteco <- read_csv("mun_luces_175.csv" %>% 
      file.path("../data/viirs/processed_tables", .)) %>% 
  mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% 
  left_join(pibe_14, by="CVEENT") %>% 
  group_by(CVEENT) %>% 
  mutate(ae_loc = pibe*LUMEN_loc/sum(LUMEN_loc), 
         ae_175 = pibe*x175_loc/sum(x175_loc)) %>% 
  ungroup %>% select(-pibe, -CVEENT)

write_csv(muns_acteco, "../data/resultados/acteco/por_municipio.csv")



muns_metro <- read_csv("zonas_metro_estado_ok.csv" %>% 
    file.path("../data/referencias", .)) %>% 
  select(CVEMET, CVEENT, CVEMUN, zona_metro = nombre_corto)

acteco_metro <- muns_acteco %>% 
  right_join(muns_metro, by="CVEMUN") %>% 
  group_by(CVEENT, CVEMET, zona_metro) %>% 
  summarize_at(.funs = funs(sum), 
    .cols = vars(ae_loc, ae_175, LUMEN_loc, LUMEN, area, area_loc))

write_csv(acteco_metro, 
  "../data/resultados/acteco/por_zonas_metro.csv")














