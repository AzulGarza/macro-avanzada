# Variables dependientes e independientes en una sola base
pacman::p_load(tidyverse, xts, zoo, lubridate)


# carpeta -----------------------------------------------------------------

carpeta_datos <- here::here("data")

# datos -------------------------------------------------------------------

series <- readRDS(file.path(carpeta_datos, "series.RDS"))


# variables ---------------------------------------------------------------

# Variables dependientes

group_by_date <- function(matrix_xts, period = 'quarters', fun = sum){
  ep <- endpoints(matrix_xts, period)
  ts <- period.apply(matrix_xts, ep, fun)
  index(ts) <- yq(quarter(index(ts), with_year = T))
  
  ts
}

vars_dep <- 
  series$diarias$ops_spei %>%
  group_by_date() %>% 
  merge.xts(
    series$diarias$monto_spei %>% 
      group_by_date()
  ) %>% 
  merge.xts(
    series$trimestrales$ops_tpv_deb %>% 
      group_by_date()
  ) %>% 
  merge.xts(
    series$trimestrales$monto_tpv_deb %>% 
      group_by_date()
  ) %>% 
  merge.xts(
    series$trimestrales$ops_tpv_cred %>% 
      group_by_date()
  ) %>% 
  merge.xts(
    series$trimestrales$monto_tpv_cred %>% 
      group_by_date()
  ) %>% 
  merge.xts(
    series$trimestrales$ops_cheques %>% 
      group_by_date()
  ) %>% 
  merge.xts(
    series$trimestrales$monto_cheques %>% 
      group_by_date()
  ) %>% 
  na.omit()


# Vatriables independientes


vars_indep <- 
  series$mensuales$inpc %>% 
  group_by_date(fun = mean) %>% 
  merge.xts(
    series$delictivo %>% 
      group_by_date(fun = mean)
  ) %>% 
  merge.xts(
    series$diarias$tiie_28 %>% 
      group_by_date(fun = mean)
  ) %>% 
  merge.xts(
    series$trimestrales$pib
  ) %>% 
  merge.xts(
    busqueda = series$busqueda %>% 
      group_by_date()
  ) %>% 
  na.omit()

vars <- 
  vars_indep %>% 
  merge.xts(vars_dep) %>% 
  na.omit()

name_vars_indep <- colnames(vars_indep)

name_vars_dep <- colnames(vars_dep)


save(
  vars, 
  name_vars_indep, 
  name_vars_dep, 
  file = file.path(carpeta_datos, "vars.RData")
)


# remove ------------------------------------------------------------------

rm(list = ls())
