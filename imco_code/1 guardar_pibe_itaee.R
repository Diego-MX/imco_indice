# Diego Villamil, OPI
# CDMX, 15 de diciembre de 2016

# 1. Bajar series del BIE:
#   - Indicadores económicos de coyuntura > ITAEE > 
#       Series desestacionalizadas > se limpian después
#   - Cuentas nacionales > PIBE > Act. Eco. y Ent. Federativa > 
#       Precios constantes 2008 > Total de Act. Eco. 


estados <- read_csv("../data/referencias/estados.csv") %>% 
  mutate(CVEENT = str_pad(cveent, 2, "left", "0")) %>% 
  select(BIE, Estado, CVEENT)

itaee <- read_csv("../data/bie/raw/itaee.csv", skip = 1, 
      locale = locale(encoding="latin1")) %>% head(-3) %>% 
  select(-contains("Variación")) %>% 
  set_names(names(.) %>% 
      str_replace_all("(.*ciclo > | Índice.*)", "")) %>% 
  gather("estado_efecto", "serie", 2:ncol(.)) %>% 
  separate(estado_efecto, c("estado", "efecto"), sep=" > ") %>% 
  mutate(efecto = efecto %>% str_replace_all(c(
      "Serie original.*" = "original", 
      "Serie desestacionalizada" = "desest", 
      "Tendencia-ciclo" = "ciclo")), 
    fecha = Periodo %>% str_replace_all(c(
      "/04"="/12", "/03"="/09", "/02"="/06", "/01" = "/03")) %>% 
          str_c("/01") %>% ymd) %>% 
  spread(efecto, serie) %>% 
  select(fecha, estado, original, desest, ciclo) %>% 
  left_join(estados, by = c("estado"="BIE")) %>% select(-estado)
  


pibe <- read_csv("../data/bie/raw/pibe.csv", 
      skip=3, locale=locale(encoding="latin3")) %>% 
  head(-3) %>% 
  set_names(names(.) %>% str_replace(" r1.*", "")) %>% 
  gather(Estado, pibe, Aguascalientes:Zacatecas) %>% 
  mutate(año = Periodo %>% str_c("1201") %>% ymd) %>% 
  select(año, Estado, pibe) %>% 
  left_join(estados, by=c("Estado"="BIE")) %>% select(-Estado) %>% 
  rename(Estado = Estado.y)


write_csv(itaee, "../data/bie/processed/itaee.csv")  

write_csv(pibe, "../data/bie/processed/pibe.csv")










