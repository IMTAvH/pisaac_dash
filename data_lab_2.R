library(tidyverse)
library(lubridate)

datos_2 <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/",
                                  token = Sys.getenv("token_pisaac_2"),  
                                  report_id = 767, raw_or_label = "label", guess_type = FALSE)$data

data_lab_2 <- datos_2 %>% 
  filter(
    enrol_1 == "Si", 
    # lubridate::as_date(enrol_99) == lubridate::today()
  ) %>%
  # View()
  select(
    Codigo = enrol_8,
    Codigomuestra = col_muestra_5,
    Iniciales = col_muestra_3,
    Tipo = col_muestra_18,
    Genero = enrol_14,
    Años = enrol_13,
    Meses = enrol_163,
    Fechahora = col_muestra_4,
    Responsable = col_muestra_7,
    
    enrol_99,
    
    Centro = pre_16,
    pre_17,
    pre_18,
    # Servicio = pre_17, # Servicios de Cayetano
    # Servicio = pre_18, # Servicio de Sergio Bernales
    
    # contains("col_muestra")
    Acepto = col_muestra_1,
    Razon = col_muestra_2,
    
    
    
    `Fecha envio` = col_muestra_6,
    
  ) %>% 
  # View()
  # add_column(Tipo = "Hisopado", .after = "Iniciales") %>%
  bind_rows(
    datos_2 %>% 
      filter(
        enrol_1 == "Si", 
        # lubridate::as_date(enrol_99) == lubridate::today()
      ) %>%
      select(
        Codigo = enrol_8,
        Codigomuestra = col_muestra_13,
        Iniciales = col_muestra_11,
        Centro = pre_16,
        Fechahora = col_muestra_12,
        Responsable = col_muestra_15,
        
        enrol_99,
        Genero = enrol_14,
        Años = enrol_13,
        Meses = enrol_163,
        pre_17,
        pre_18,
        # Servicio = pre_17, # Servicio de Cayetano
        # Servicio = pre_18, # Servicio de Sergio Bernales
        
        # contains("col_muestra")
        Acepto = col_muestra_8,
        Razon = col_muestra_9,
        # col_muestra_10,
        
        # Tipo = "Orina",
        
        `Fecha envio` = col_muestra_14,
        
      ) %>% 
      add_column(Tipo = "Orina", .after = "Iniciales")
  ) %>% 
  arrange(enrol_99, Codigo) %>%
  # View()
  # filter(
  #   lubridate::as_date(Fechahora) == lubridate::today()
  # ) %>%
  mutate(
    `Fecha colecta` = lubridate::ymd(str_sub(Fechahora,1,10)),
    `Hora colecta` = as.character(hms::parse_hm(str_sub(Fechahora,11))),
    Servicio = case_when(
      Centro == "HOSPITAL CAYETANO HEREDIA" ~ pre_17,
      Centro == "HOSPITAL SERGIO BERNALES" ~ pre_18,
      T ~ NA_character_
    ),
    `Fecha envio` = lubridate::ymd(`Fecha envio`),
    across(c(Años, Meses), ~as.numeric(.))
  ) %>% 
  relocate(`Fecha colecta`, `Hora colecta`, .before = Responsable) %>%
  relocate(Servicio, .after = Centro) %>%
  select(-c(enrol_99, Fechahora, pre_17, pre_18)) %>% 
  filter(
    !is.na(Acepto)
  )
