library(tidyverse)
library(lubridate)

datos_2 <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/",
                                  token = "0D53115172B94333E7EC10F5FAC63E48",  
                                  report_id = 767, raw_or_label = "label",guess_type = FALSE)$data

# Lectura de datos actuales en sheet de resultados
entrega_res_actual_2 <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1G1-erEgp3brQaGydKSJcEzJMRhuFkNtheWSeDPPixrU/edit?gid=0#gid=0", sheet = "Resultados")


# Entrega de resultados del lab
results_lab_1 <- datos_2 %>% 
  filter(
    enrol_1 == "Si"
  ) %>%
  # View()
  mutate(
    # Edad = lubridate::ymd(enrol_12)
    # Edad = lubridate::as_date(col_muestra_4)
    # Edad_años = as.period(interval(lubridate::ymd(enrol_12), lubridate::as_date(col_muestra_4)), unit = "year")$year,
    # Edad_meses = as.period(interval(lubridate::ymd(enrol_12), lubridate::as_date(col_muestra_4)), unit = "year")$month,
    # Edad_dias = as.period(interval(lubridate::ymd(enrol_12), lubridate::as_date(col_muestra_4)), unit = "year")$day,
    enrol_12 = lubridate::as_date(enrol_12),
    enrol_99 = lubridate::as_date(enrol_99),
    Edad = as.period(interval(enrol_12, enrol_99), unit = "year")$year,
    
    Servicio = case_when(
      pre_16 == "HOSPITAL CAYETANO HEREDIA" ~ pre_17,
      pre_16 == "HOSPITAL SERGIO BERNALES" ~ pre_18,
      T ~ NA_character_
    ),
    Fecha = as.Date(col_muestra_4)
  ) %>% 
  # arrange(Edad_años, Edad_meses, Edad_dias) %>% 
  # View()
  select(
    Codigo = enrol_8,
    Centro = pre_16,
    Iniciales = col_muestra_3,
    Genero = enrol_14,
    # Edad = enrol_13,
    Edad,
    # Servicio = pre_17, # Servicio de Cayetano
    # Servicio = pre_18, # Servicio de Sergio Bernales
    Servicio,
    Telf1 = enrol_22,
    Telf2 = enrol_23,
    Correo = enrol_100,
    `Via Preferente` = enrol_101,
    Fecha
  ) %>% 
  # View()
  add_column(`Fecha de entrega de resultados Ag. S. Pneumoniae` = NA_character_) %>% 
  add_column(`Fecha de envío resultado a Encargado de servicio` = NA_character_) %>% 
  add_column(`Observaciones Servicio` = NA_character_) %>% 
  # View()
  filter(
    Fecha == lubridate::today()
  ) %>%
  arrange(Codigo) %>% 
  anti_join(
    entrega_res_actual_2,
    by = "Codigo"
  )