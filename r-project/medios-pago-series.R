
# Seires banxico ----------------------------------------------------------


# paquetes ----------------------------------------------------------------

pacman::p_load(tidyverse, banxicoR, skimr, lubridate)


# data --------------------------------------------------------------------

series_diarias <- 
  list(
    ops_spei = "SF316454",
    monto_spei = "SF316455",
    ops_tef = "SF316143",
    monto_tef = "SF316144"
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
    monto_cheques = "SF61610"
  )


# functions ---------------------------------------------------------------

limpiar_datos <- function(lista_datos){
  datos <- 
    lista_datos %>% 
    map(~banxico_series(.)) %>% 
    reduce(left_join, "Dates")
  
  colnames(datos) <- c("fecha", names(lista_datos))
  
  return(datos)
  
}

resumen_sin_fecha <- function(datos, nombre_variables, nombre_cols){
  datos %>% 
    filter(! variable %in% c("fecha")) %>% 
    select(
      variable, mean, sd
    ) %>% 
    mutate(
      variable = nombre_variables
    ) %>% 
    mutate_at(vars(-variable), as.numeric) %>% 
    knitr::kable(
      format = "latex", 
      col.names = nombre_cols, 
      align = 'lrr', escape = T, 
      format.args = list(big.mark = ","),
      caption = "Elaboración propia con datos de Banco de México"
    )
}

resumen_fecha <- function(datos, leyenda, periodicidad){
  
  datos %>% 
    filter(variable == "fecha") %>% 
    select(min, max, n) %>% 
    gather() %>% 
    mutate(
      key = case_when(
        key == "min" ~ "Fecha inicial",
        key == "max" ~ "Fecha final",
        T ~ "Número de observaciones"
      )
    ) %>% 
    add_row(
      key = "Periodicidad de información", 
      value = periodicidad, .before = T
    ) %>% 
    knitr::kable(
      format = "latex", 
      col.names = NULL,
      align = 'lr',
      caption = leyenda
    )
}

col_names_sin_fecha <- 
  c(
    "Variable", "Media", "Desviación estándar"
  )


# datos diarios -----------------------------------------------------------


datos_diarios <- limpiar_datos(series_diarias)

skim_diarios <- 
  datos_diarios %>% 
  skim_to_wide()  

variable_diarios <- 
  c(
    "Monto SPEI", "Monto TEF", 
    "Operaciones SPEI", "Operaciones TEF"
  )
  
resumen_sin_fecha(skim_diarios, variable_diarios, col_names_sin_fecha)

resumen_fecha(
  skim_diarios, 
  "Descripción de la información diaria",
  "diaria"
)

datos_diarios %>% 
  gather(variable, valor, -fecha) %>% 
  ggplot(aes(fecha, valor, group = variable)) + 
  geom_line() + 
  facet_wrap(~ variable, ncol = 2, scales = "free") + 
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(), 
    panel.background = element_blank()
  )

crisis <- 
  datos_diarios %>% 
  mutate(
    after_crisis = if_else(
      year(fecha) >= 2008, 
      1, 
      0
    )
  )

crisis %>%
  gather(variable, value, -fecha, -after_crisis) %>% 
  filter(variable == "monto_spei", after_crisis > 0) %>% 
  mutate(
    before_2010 = if_else(year(fecha) < 2010, 1, 0)
  ) %>% 
  ggplot(aes(fecha, value/1e6, color = factor(before_2010))) +
  geom_line() +
  expand_limits(x = ymd("2020-01-01")) +
  geom_text(
    data = . %>% tail(1),
    aes(fecha, value/1e6, label = "Monto de las\ntransferencias", vjust = 0.5),
    hjust = -0.02,
    size = 3,
    color = "gray"
  )+
  labs(
    title = "Se observa una depresión del monto transaccionado\ntras la crisis de 2008 y hasta finales de 2009",
    x = "Año",
    y = "Millones de\npesos\nmensuales",
    caption = "Fuente: Banco de México"
  )+
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.title = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.97, hjust = 1, size = 8, color = "#b2b2b2"),
    axis.line = element_line(size = 0.5, color = "gray"),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "#b2b2b2", size = 8),
    axis.line.x = element_line(color = "#b2b2b2"),
    axis.line.y = element_blank(),
    plot.caption = element_text(color = "#b2b2b2", size = 6)
  )+ 
  scale_color_manual(values = c("gray", "#619CFF"))+
  scale_x_date(
    #breaks = 2004:2020,
    date_breaks = "1 year",
    labels = scales::date_format("%Y")
  ) + 
  scale_y_continuous(
    breaks = seq(0, 2, length.out = 6), limits = c(0, 2)
    #labels = scales::dollar_format(prefix = "$")
  )

ggsave("monto-spei-post-crisis.png", width = 20, height = 0.618*20, units = "cm")

crisis %>%
  gather(variable, value, -fecha, -after_crisis) %>% 
  filter(variable == "monto_spei", after_crisis < 1) %>% 
  mutate(
    after_mid_2007 = if_else(ymd(fecha) > "2007-09-01", 1, 0)
  ) %>% 
  ggplot(aes(fecha, value/1e6, color = factor(after_mid_2007))) +
  geom_line() +
  expand_limits(x = ymd("2008-07-01")) +
  geom_text(
    data = . %>% tail(1),
    aes(fecha, value/1e6, label = "Monto de las\ntransferencias", vjust = 0.5),
    hjust = -0.02,
    size = 3,
    color = "gray"
  ) + 
  labs(
    title = "La serie proviene de una tendencia creciente",
    x = "Año",
    y = "Millones de\npesos\nmensuales",
    caption = "Fuente: Banco de México"
  )+
  scale_color_manual(values = c("gray", "#619CFF")) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.title = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.97, hjust = 1, size = 8, color = "#b2b2b2"),
    axis.line = element_line(size = 0.5, color = "gray"),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "#b2b2b2", size = 8),
    axis.line.x = element_line(color = "#b2b2b2"),
    axis.line.y = element_blank(),
    plot.caption = element_text(color = "#b2b2b2", size = 6)
  )+ 
  #scale_color_manual(values = c("gray", "red"))+
  scale_x_date(
    #breaks = 2004:2020,
    date_breaks = "1 year",
    labels = scales::date_format("%Y")
  ) + 
  scale_y_continuous(
    breaks = seq(0, 2, length.out = 6), limits = c(0, 2)
    #labels = scales::dollar_format(prefix = "$")
  )

ggsave("monto-spei-pre-crisis.png", width = 20, height = 0.618*20, units = "cm")

# datos trimestrales ------------------------------------------------------


datos_trim <- limpiar_datos(series_trim)

skim_trim <- 
  datos_trim %>% 
  skim_to_wide()
 
variable_trim <- 
  c(
    "Monto cheques", "Monto domiciliaciones", "Monto de retiros crédito",
    "Monto retiros débito", "Monto TPV crédito", "Monto TPV débito",
    "Número cheques", "Número domiciliaciones", 
    "Número de retiros crédito", "Número de retiros débito",
    "Operaciones TPV crédito", "Operaciones TPV débito",
    "Tarjetas utilizadas crédito", "Tarjetas utilizadas débito"
  )

resumen_sin_fecha(skim_trim, variable_trim, col_names_sin_fecha)

resumen_fecha(
  skim_trim, 
  "Descripción de la información trimestral",
  "trimestral"
)


# gráficas ----------------------------------------------------------------

datos_trim %>% 
  gather(variable, valor, -fecha) %>% 
  ggplot(aes(fecha, valor, group = variable)) + 
  geom_line() +
  facet_wrap(~variable, scales = "free") + 
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(), 
    panel.background = element_blank()
  )
  


