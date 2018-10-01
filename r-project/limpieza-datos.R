
# paquetes ----------------------------------------------------------------

pacman::p_load(tidyverse, banxicoR, xts, zoo, lubridate)


# directory ---------------------------------------------------------------

carpeta <- here::here("data", "raw-data")

# datos -------------------------------------------------------------------

# Datos obtenidos
datos_busqueda <-
  list.files(carpeta, pattern = "^busqueda", full.names = T) %>% 
  set_names(nm = str_to_lower(
    str_remove_all(basename(.), "busqueda|.csv"))
  ) %>% 
  map(data.table::fread)

# Datos banxico
series_diarias <- 
  list(
    ops_spei = "SF316454",
    monto_spei = "SF316455",
    ops_tef = "SF316143",
    monto_tef = "SF316144",
    tiie_28 = "SF43783"
  )

series_trim <- 
  list(
    ops_tpv_deb = "SF62273",
    monto_tpv_deb = "SF62279",
    ops_tpv_cred = "SF62274",
    monto_tpv_cred = "SF62280",
    ops_domis = "SF62182",
    monto_domis = "SF62183",
    tar_ut_deb = "SF61873",
    tar_ut_cred = "SF61870",
    ops_ret_deb = "SF62270",
    monto_ret_deb = "SF62276",
    ops_ret_cred = "SF62271",
    monto_ret_cred = "SF62277",
    ops_cheques = "SF61604",
    monto_cheques = "SF61610",
    pib = "SR16643"
  )

series_men <- 
  list(
    inpc = "SP1"
  )

series_banxico <-
  list(
    diarias = series_diarias, 
    trimestrales = series_trim, 
    mensuales = series_men
  )

# functions ---------------------------------------------------------------

# Esta función es solo para los datos de Banxico
limpiar_datos_banxico <- function(lista_datos){
  datos <- 
    lista_datos %>% 
    map(~banxico_series(.)) %>% 
    reduce(left_join, "Dates") %>% 
    mutate_at(vars(Dates), as.Date)
  
  
  colnames(datos) <- c("fecha", names(lista_datos))
  
  ts <- select(datos, -fecha)
  fecha <- pull(datos, fecha)
  
  ts <- xts(ts, order.by = fecha)
  
  return(ts)
  
}

# Esta función es para los datos de búsquedas
limpiar_datos_busqueda <- function(lista_datos){
  datos <-
    lista_datos %>% 
    reduce(left_join, "Mes") %>% 
    mutate_at(vars(Mes), str_c, "-01") %>%  
    mutate_at(vars(Mes), as.Date) 
  
  colnames(datos) <- c("fecha", names(lista_datos))
  
  ts <- select(datos, -fecha)
  fecha <- pull(datos, fecha)

  ts <- xts(ts, order.by = fecha)
  
  return(ts)
  
}

limpiar_datos_ahora <- function(file_name){
  series <- 
    series_banxico %>% 
    map(limpiar_datos_banxico)
  
  datos_busqueda <-
    limpiar_datos_busqueda(datos_busqueda)
  
  series$busqueda <-datos_busqueda
  
  
  series %>% 
    saveRDS(file_name)
  
  beepr::beep(5)
}

# Limpiando

limpiar_datos_ahora(here::here("data", "series.RDS"))

# finally -----------------------------------------------------------------

rm(list = ls())
