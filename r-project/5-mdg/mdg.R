# paquetes ----------------------------------------------------------------
pacman::p_load(tidyverse, xts, broom, forecast, lmtest, data.table, knitr)
library(ggplot2)
library(tsr)
library(formula.tools)

source("G:/projects/tsr/R/tsr.R")
source("G:/projects/tsr/R/drop-terms.R")
source("G:/projects/tsr/R/dynamic-mult.R")


# carpeta datos -----------------------------------------------------------

carpeta_datos <- here::here("data")

# datos -------------------------------------------------------------------

load(file.path(carpeta_datos, "vars.RData"))
load(file.path(carpeta_datos, "exogeneidad.RData"))
data <- read_rds(file.path(carpeta_datos, "series.RDS"))


l_vars <- log(vars)
n_l_vars <- names(l_vars)
n_l_vars[str_detect(n_l_vars, "tiie_28")] <- "tiie"

names(l_vars) <- n_l_vars

mdg_models <- 
  exogeneidad %>% 
    map(
      function(modelo){
        espec <- modelo[["espec"]]
        vars <- str_split(espec, "~|\\+")[[1]]
        dep_var <- vars[1]
        indep_var <- 
          setdiff(vars, dep_var) %>% 
          str_replace_all("_28", "")
        model <- 
          auto_mdg(
            l_vars[,dep_var],
            l_vars[,indep_var],
            0, 3, 3
          )
        if(model == "error"){
          model <- NULL
        }
        
        res <- list()
        res[["model"]] <- model
        res[["espec"]] <- espec
        return(res)
      }
    )

mdg_final_models <- 
  mdg_models %>% 
    discard(~is.null(.x[["model"]]))

# Results

carpeta_mdg <- here::here("5-mdg")
resultados <- file.path(carpeta_mdg, "resultados")

if(!dir.exists(resultados)){
  dir.create(resultados)
}


mdg_final_models %>% 
  walk(
    function(model){
      
      # Especificacion del modelo
      espec <- model[["espec"]]
      # Carpeta del modelo
      carpeta_modelo <- file.path(resultados, espec)
      if(!dir.exists(carpeta_modelo)){
        dir.create(carpeta_modelo)
      }
      
      # Resultados de la especificacion
      resultados_model <- file.path(carpeta_modelo, "resultados.txt")
      
      if(file.exists(resultados_model)){
        file.remove(resultados_model)
      } else {
        file.create(resultados_model)
      }
      
      modelo <- model[["model"]]
      
      cat("Especificación:", "\n", "\n", file = resultados_model, append = T)
      cat(espec, "\n", "\n", file = resultados_model, append = T)
      cat("Resultados:", "\n", "\n", file = resultados_model, append = T)
      cat(
        kable(tidy(coeftest(modelo)), format = "latex"), 
        file = resultados_model, 
        append = T
      )
      
      ## Residuals
      file_plot_residuals <- file.path(carpeta_modelo, "residuals.png")
      png(filename = file_plot_residuals, width = 900, height = 0.68*900)
      print(
        checkresiduals(
          modelo,
          main = espec
        )
      )
      dev.off()
      
      vars <- str_split(espec, "~")
      var_dep <- vars[[1]][1]
      #print(var_dep)
      dy_mult <- dynamic_mult(modelo, var_dep)
      
      if(dy_mult == "error"){
        return()
      } 
      
      
      # Multiplicadores
      # Acumulado
      file_plot_acumulado <- file.path(carpeta_modelo, "acumulado.png")
      plot_mult_acumulado(dy_mult)
      ggsave(file_plot_acumulado, width = 10, height = 0.68*10)
      
      # Acumulado
      file_plot_aislado <- file.path(carpeta_modelo, "aislado.png")
      plot_mult_aislado(dy_mult)
      ggsave(file_plot_aislado, width = 10, height = 0.68*10)
      
      # Acumulado
      file_plot_shocks <- file.path(carpeta_modelo, "shocks.png")
      plot_mult_shock(dy_mult)
      ggsave(file_plot_shocks, width = 10, height = 0.68*10)
      
      
      
    }
  )

