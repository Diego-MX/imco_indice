# Diego Villamil, OPI
# CDMX, 2 de enero de 2017


dir_cnbv <- "../data/cnbv"
fecha_ultima <- "2016-09-01" %>% as.Date


leer_multibanca <- function (pestagna_id, periodo, 
  output=FALSE, bancos = NULL) {
  require(zoo)
  require(readxl)
  # Lee archivo de Banca Múltiple
  nombre_ <- pestagna_id
  if (output) cat(sprintf("%-32s %s\n", nombre_, periodo))

  mb_ <- file.path(dir_cnbv, "raw", "BM_Operativa_%s.xls") %>% 
    sprintf(periodo) %>%
    read_excel(sheet = nombre_, skip=1) %>%
    { names(.)[1:4] <- c("entidad", "cvegeo", "colonia", "valor")
      as_data_frame(.)} %>%  # set_names[1:4]
    filter(!is.na(colonia)) %>%  # Los totales por estado
    slice(-1) %>%  # La primera fila es un total también
    mutate(
        entidad = na.locf(entidad),
        cvegeo  = na.locf(cvegeo) %>% str_sub(4, 13),
        fecha   = periodo %>% sprintf("%s01", .) %>% ymd,
        tipo    = nombre_)
  
  bancos_ <- intersect(bancos, names(mb_)) %>% sprintf("`%s`", .)
  mb <- select_(mb_, .dots = union(bancos_, 
      c("tipo","cvegeo","colonia","fecha","valor")))
  return (mb)
}


pestagnas_df <- read_csv("../data/referencias/pestañas_cnbv.csv", 
  locale = locale(encoding = "latin1")) %>% 
  filter(modelo_imco)
periodos <- seq(as.Date("2011-04-01"), fecha_ultima, 
  by="1 month") %>% 
  format("%Y%m")


MB_frame_ <- expand.grid(pestagnas_df$Nombre, periodos, 
      stringsAsFactors=FALSE) %>%
  apply(1, . %>% {leer_multibanca(.[1], .[2], output=TRUE, 
      bancos = c("BBVA Bancomer", "Santander") )}) 


MB_frame <- MB_frame_ %>% 
  bind_rows %>% 
  mutate(tipo = pestagnas_df %$% Alias[match(tipo, Nombre)])

write_csv(MB_frame, "../data/cnbv/processed/por_localidad.csv")


## Y después agregar por municipio

# Por si es el caso
MB_frame <- fread("../data/cnbv/processed/por_localidad.csv", 
  colClasses = c("integer", "integer", "character", 
          "character", "NULL", "Date", "integer")) %>% 
  mutate(CVEMUN = str_sub(cvegeo, 1, 5)) %>% 
  setkey(fecha, CVEMUN, tipo)

MB_muns <- MB_frame %>% 
  group_by(fecha, CVEMUN, tipo) %>% 
  summarize_at(vars(valor, `BBVA Bancomer`, Santander), 
      funs(. %>% sum(na.rm = TRUE))) %>% 
  mutate_at(vars(Santander, `BBVA Bancomer`), 
      funs(. %>% {.*(tipo == "transacciones_atm")})) %>% 
  mutate(valor = valor - Santander - `BBVA Bancomer`) %>% 
  select(-Santander, -`BBVA Bancomer`) %>% 
  spread(tipo, valor, fill = 0) %>% 
  rename(atm_1 = transacciones_atm) %>% 
  mutate(trimestre = floor_date(fecha %>% ymd, "quarter") %>% 
      add(2 %>% months)) %>% 
  group_by(trimestre, CVEMUN) %>% 
  summarize(atm_1 = sum(atm_1, na.rm = T), 
      n_atm = max(n_atm, na.rm = T)) %>% 
  filter(CVEMUN != "blanc")
  
write_csv(MB_muns, "../data/cnbv/processed/por_municipio.csv")  


# Por ciudad  

MB_metros <- read_csv("zonas_metro_estado_ok.csv" %>% 
    file.path("../data/referencias/", .)) %>%  
  left_join(MB_muns, by = "CVEMUN") %>% 
  group_by(trimestre, CVEENT, CVEMET, nombre_corto, zona_met) %>% 
  summarize_at(vars(atm_1, n_atm), funs(. %>% sum(na.rm = T)))
  
write_csv(MB_metros, "../data/cnbv/processed/por_zonas_metro.csv")


# Por estado

MB_estados <- MB_muns %>% 
  mutate(CVEENT = str_sub(CVEMUN, 1, 2)) %>% 
  group_by(trimestre, CVEENT) %>% 
  summarize_at(vars(atm_1, n_atm), funs(. %>% sum(na.rm = T)))

write_csv(MB_estados, "../data/cnbv/processed/por_estados.csv")




