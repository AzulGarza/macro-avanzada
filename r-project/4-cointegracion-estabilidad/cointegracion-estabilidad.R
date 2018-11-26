# Cointegración y estabilidad
pacman::p_load(tidyverse, xts, zoo, lubridate, urca, aTSA, AER, magrittr)
pacman::p_load(knitr, broom)

# carpeta -----------------------------------------------------------------

carpeta_datos <- here::here("data")

# datos -------------------------------------------------------------------

load(file.path(carpeta_datos, "vars.RData"))

l_vars <- log(vars)


# Johansen ----------------------------------------------------------------

combn_indep <- 
  1:length(name_vars_indep) %>% 
  map(~combn(name_vars_indep, ., simplify = F)) %>% 
  flatten()

all_combn <- 
  name_vars_dep %>% 
  map(
    function(dep_var){
      combn_indep %>% 
        map(~c(., dep_var))
    }
  ) %>% 
  flatten()

estabilidad <- 
  all_combn %>% 
  map(
    function(combination){
      vars_test <- l_vars[, combination]
      dep_var <-intersect(combination, name_vars_dep)
      indep_var <- intersect(combination, name_vars_indep)

      # Johansen test
      jo_test <- ca.jo(vars_test, K = 4, type = "trace", spec = "longrun")
      jo_stat <- as.matrix(jo_test@teststat, nrow = length(combination))
      jo_cval <- jo_test@cval
      colnames(jo_stat) <- "jo_stat"
      rownames(jo_stat) <- rownames(jo_cval)
      jo_result <- merge(jo_stat, jo_cval, by = "row.names")
      
      # Engle Granger test
      en_gran_test <-
        coint.test(
          as.matrix(vars_test[,dep_var]),
          as.matrix(vars_test[,indep_var]), 
          output = F, 
          nlag = 4
        )
      
      results <- list()
      
      results$espec <- 
        list(
          dep_var = dep_var,
          indep_var = indep_var
        )
      
      #results$estabilidad_jo <- any(jo_test@teststat > )
      
      results$johansen <- jo_result
      results$eng_gran <- en_gran_test
      
      return(results)
    }
  )

# Mantener relaciones estables por johansen y por
# Engle y Granger
estables <- 
  estabilidad %>% 
  keep(
    function(relacion){
      est_johansen <- any(relacion$johansen[,"jo_stat"] > relacion$johansen[, "10pct"])
      est_eng_gran <- relacion$eng_gran[1, "p.value"] < 0.1
      
      return(est_johansen | est_eng_gran)
    }
  )
  
# Prueba de Hausman

# Exogeneidad
exogeneidad <- 
  estables %>% 
    map(
      function(relacion){
        
        dep_var <- relacion$espec$dep_var
        indep_var <- relacion$espec$indep_var
        
        form <- 
          str_c(
            dep_var,
            "~",
            str_c(indep_var, collapse = "+")
          )
        
        
        lags_iv <- 0
        r_squared <- 0
        iv_form <- ""
        for(number in 1:4){ 
          new_form <- 
            str_c(
              "lag(l_vars$",
              indep_var, 
              ",",
              number,
              ")",
              collapse = "+"
            )
          
          
          new_iv_form <- str_c(c(form, new_form), collapse = "|")
          
          r_squared_new <- 
            ivreg(
              new_iv_form,
              data = l_vars
            ) %>% 
            summary() %$%
            r.squared
          
          if(r_squared_new > r_squared){
            lags_iv <- number
            iv_form <- new_iv_form
          }
        }
        
        ols <- lm(form, data = l_vars)
        
        if(lags_iv < 1){
          new_new_form <- 
            str_c(
              "lag(l_vars$",
              indep_var, 
              ",",
              4,
              ")",
              collapse = "+"
            )
          
          
          iv_form <- str_c(c(form, new_new_form), collapse = "|")
        } 
        
        iv <- ivreg(
          iv_form, 
          data = l_vars
        )
        
        #Test
        
        cf_diff <- coef(iv) - coef(ols)
        vc_diff <- vcov(iv) - vcov(ols)
      
        x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
        
        result <- list()
        
        result$espec <- form
        result$johansen <- relacion$johansen
        result$eng_gran <- relacion$eng_gran
        result$lags_iv <- lags_iv
        result$ols <- tidy(ols)
        result$iv <- tidy(iv)
        result$chi <- 
          list(
              stat = x2_diff,
              prob = pchisq(
                x2_diff, 
                df = length(indep_var)+1, 
                lower.tail = FALSE
              )
            )
        
        result
      }
    ) %>% 
    keep(~.$chi$prob > 0.1 & .$chi$stat > 0)


# Reporte de datos --------------------------------------------------------

carpeta_co_est <- here::here("4-cointegracion-estabilidad", "resultados")
resultados <- file.path(carpeta_co_est, "resultados.txt")


if(file.exists(resultados)){
  file.remove(resultados)
} else {
  file.create(resultados)
}

exogeneidad %>% 
  walk(
    function(relacion){
      cat("Especificación: ", relacion$espec, "\n", file = resultados, append = T)
      cat("Prueba de Johansen:", file = resultados, "\n", append = T)
      
      joh <- relacion$johansen
      colnames(joh) <- c("Prueba", "Estadístico", "10pct", "5pct", "1pct")
      
      cat(kable(joh, "latex"), "\n", "\n", file = resultados, append = T)
      
      cat("Prueba de Engle y Granger:", "\n", file = resultados, append = T)
      
      eng_gran <- relacion$eng_gran
      
      colnames(eng_gran) <- c("Retrasos", "Estadístico", "Pvalue")
      
      cat(kable(as.matrix(eng_gran[1, c(2,3)], ncol = 2), "latex"), "\n", "\n", file = resultados, append = T)
      
      cat("Prueba de exogeneidad:", "\n", file = resultados, append = T)
      cat("Resultados ols:", "\n", "\n", file = resultados, append = T)
      cat(kable(relacion$ols, "latex"), "\n", "\n", file = resultados, append = T)
      cat("Número de rezagos para iv:", relacion$lags_iv, "\n", "\n", file = resultados, append = T)
      cat("Resultados iv:", "\n", "\n", file = resultados, append = T)
      cat(kable(relacion$iv, "latex"), "\n", "\n", file = resultados, append = T)
      cat("Estadístico:", relacion$chi$stat, "\n", file = resultados, append = T)
      cat("P-value:", relacion$chi$prob, "\n", file = resultados, append = T)
      cat("--------", "\n", "\n", file = resultados, append = T)
    }
  )

save(exogeneidad, file = file.path(carpeta_datos, "exogeneidad.RData"))

