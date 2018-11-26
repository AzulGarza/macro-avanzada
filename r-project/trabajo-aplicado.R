
# Trabajo aplicado macroeconometría avanzada ------------------------------
# Este script contiene todo el trabajo aplicado para la clase 
# de macroeconometría avanzada, ITAM, otoño 2018


# Paquetes ----------------------------------------------------------------

pacman::p_load(tidyverse, xts, zoo, tseries, forecast, beepr)

source(here::here("load-data.R")) # Esto carga las bases de datos


# Segunda presentación, estabilización ------------------------------------

# Función para estabilizar

estabilizar <- function(serie, retraso_adf){
  # Dicky Fuller
  adf_list <- adf.test(na.omit(serie), alternative = "s", k = retraso_adf)
  p_value_adf <- adf_list$p.value
  
  # Tendencia determinística
  time <- 1:nrow(serie)
  summary_time <- summary(lm(serie ~ time))
  p_value_det <- summary_time$coefficients[2, 4]
  
  # Variables de utilidad
  ts <- serie
  n_diffs <- 0
  while(p_value_adf > 0.01 | p_value_det < 0.01){
    # Si existe alguna tendencia, diferenciamos
    ts <- diff.xts(ts, differences = 1)
    n_diffs <- n_diffs + 1
    
    # DF
    adf_list <- adf.test(na.omit(ts), alternative = "s", k = retraso_adf)
    p_value_adf <- adf_list$p.value
    
    #Determinista
    time <- 1:nrow(ts)
    summary_time <- summary(lm(ts ~ time))
    p_value_det <- summary_time$coefficients[2, 4]
  }
  
  resumen <- 
    tibble(
      diferencias = n_diffs, 
      retrasos = retraso_adf,
      p_val_adf = p_value_adf,
      p_val_det = p_value_det,
      media = mean(ts, na.rm = T),
      desv_est = sd(ts, na.rm = T)
    )
  
  list_r <- 
    list(
      serie_original = serie,
      serie_diferenciada = ts,
      resumen = resumen
    )
  
  return(list_r)
}

grafica_tabla <- function(serie, retraso_adf, main_plot){
  file_plot_no_est <- here::here("2-estabilizacion", paste0(main_plot, "-no-est.png"))
  png(filename = file_plot_no_est, width = 900, height = 0.68*900)
  print(plot(serie, main = main_plot))
  dev.off()
  estable <- estabilizar(serie, retraso_adf)
  # Plot
  file_plot <- here::here("2-estabilizacion", paste0(main_plot, ".png"))
  png(filename = file_plot, width = 900, height = 0.68*900)
  print(plot(estable$serie_diferenciada, main = main_plot))
  dev.off()
  # Tabla
  file_tabla <- here::here("2-estabilizacion", "tablas.txt")
  cat(paste0("tabla ", main_plot, ":"), file = file_tabla, append = T)
  cat(knitr::kable(estable$resumen, "latex", digits = 2), file = file_tabla, append = T)
  cat("\n", file = file_tabla, append = T)
  cat("\n", file = file_tabla, append = T)
  
  return(estable)
}

estabilizar_series <- function(){
  dir <- here::here("2-estabilizacion")
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  file.create(file.path(dir, "tablas.txt"))
  lista_datos %>% 
    map2(
      c(30, 4, 12, 12, 12),
      function(datos, periodo_diff){
        
        index <- index(datos)
        map2(
            as_tibble(datos),
            names(datos),
            ~grafica_tabla(as.xts(log(.x + 0.01), index), periodo_diff, .y)
          ) 
        
      }
    ) %>% 
    saveRDS(here::here("data", "series-estables.RDS"))
  
  beepr::beep(5)
}

# análisis 

beep_on_error(estabilizar_series())




# Modelos ARMA ------------------------------------------------------------

dir.create(here::here("3-arma"))

# Cargamos las series
series_estables <- 
  readRDS(here::here("data", "series-estables.RDS")) %>% 
  flatten()

names_series <- names(series_estables)

# Funcíón para crear los arma
crear_arma <- function(nombre_serie){
  print(nombre_serie)
  # Directorios donde se guardará el trabajo y nombres
  dir_serie <- 
    str_c("3-arma", nombre_serie, sep = "/") %>% 
    here::here()
  
  if(!dir.exists(dir_serie)){
    dir.create(dir_serie)
  }
  
  file_plot <- 
    nombre_serie %>% 
    str_c(".png", collapse = "") %>% 
    str_c(dir_serie, ., sep ="/")
  
  file_plot_residuals <- 
    file_plot %>% 
    str_replace(".png", "-residuals.png")
  
  file_table <- 
    file_plot %>% 
    str_replace(".png", "-resumen.txt")
  
  file_ajustada <- 
    file_plot %>% 
    str_replace(".png", "-ajustada.png")
  
  file_pred <- 
    file_plot %>% 
    str_replace(".png", "-prediccion.png")
  
  main_plot <- 
    str_c(
      "Diagnóstico serie estacionaria (logs): ",
      nombre_serie,
      sep = ""
    )
  
  # Serie a estudiar
  
  serie <- series_estables[[nombre_serie]][["serie_diferenciada"]]
  
  
  # Gráficas de los diagnósticos
  
  png(filename = file_plot, width = 900, height = 0.68*900)
  
  print(
    tsdisplay(
      serie,
      main = main_plot
    )
  )
  
  dev.off()
  
  # Modelo ARMA
  arma <- 
    serie %>% 
    auto.arima(max.p = 10, max.q = 10, d = 0)
  
  arma_order <- arimaorder(arma)
  
  main_model <- 
    str_c(
      "ARMA(", 
      arma_order["p"],
      ",",
      arma_order["q"],
      ") ",
      "de ",
      nombre_serie,
      sep = ""
    )
  
  main_residuals  <- 
    "Residuos de " %>% 
    str_c(., main_model, sep = "")
  
  if(sum(arma_order) > 1){
    arma %>% 
      broom::tidy() %>% 
      knitr::kable(
        format = "latex",
        col.names = c("Término", "Coeficiente", "Error estándar")
      ) %>% 
      cat(
        file = file_table
      )
  }
  # Gráficas de los residuos
  
  png(filename = file_plot_residuals, width = 900, height = 0.68*900)
  
  print(
    checkresiduals(
      arma,
      main = main_residuals
    )
  )
  
  dev.off()
  
    # Si el modelo es 0 entonces se acaba
  if(sum(arma_order) < 1){
    return(NULL)
  }
  
  # Gráfica de ajustada vs real
  
  ajustada <- 
    arma$fitted %>%
    xts(order.by = index(serie))
  
  plt_a <- 
    plot(
      serie, 
      main = nombre_serie
    )
  
  plt_a <- addSeries(ajustada, main = main_model, col = "blue")
  
  png(filename = file_ajustada, width = 900, height = 0.68*900)
  
  print(
    plt_a
  )
  
  dev.off()
  
  # Gráfica de la predicción
  
  main_pred <- 
    "Predicción de " %>% 
    str_c(., main_model, sep = "")
  
  plt_pred <- 
    autoplot(
      forecast(arma, h = 10),
      include = 100, ylab = "", main = main_pred
    )
  
  png(filename = file_pred, width = 900, height = 0.68*900)
  
  print(
    plt_pred
  )
  
  dev.off()
}

crear_modelos_arma <- function(){
  names_series %>% 
    walk(crear_arma)
  
  beep(5)
}

# Creamos los modelos

beep_on_error(crear_modelos_arma())



