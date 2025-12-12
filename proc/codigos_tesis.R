#Codigos de bases de tesis 

##librerias

pacman::p_load(dplyr, summarytools, sjPlot, readr, purrr, sjmisc, ggplot2, 
               networkD3, tidyr, ggalluvial, highcharter, htmlwidgets, 
               survey, calidad, DT, zoo, glue, forcats, haven, car, corrplot, 
               stargazer, lme4, gt, tibble, janitor, psych, tidyverse, survival,
               survminer, knitr, kableExtra, naniar, corrr, lattice, forcats,
               networkD3, htmlwidgets, modelsummary, scales)


##base de datos demre

base_matriculado_corta <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/base_matriculado_corta.csv",delim = ",")
base_panel_enriquecida_final_r <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/base_panel_enriquecida_final_r.csv",delim = ",")


#### Paleta personalizada
colores_matricula <- c("Sí" = "#698B69", "No" = "#CD3333")

colores_preferencias <- c("1° preferencia" = "#63B8FF",
                          "2° preferencia" = "#5CACEE",
                          "3° preferencia" = "#4594CD",    
                          "4°-5° preferencia" = "#4682B4",
                          "6° o más" = "#36648B")

###codigos

####saco proporciones por genero y por matricula, la idea es ver las diferencias
####frecuencia no me sirve porque hombres son muy muy pocos

proporciones <- base_matriculado_corta %>%
  group_by(SEXO, matriculado) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEXO) %>%
  mutate(proporcion = n / sum(n) * 100)

proporciones <- proporciones %>%
  mutate(matriculado = ifelse(matriculado == TRUE, "Sí", "No"))

ggplot(proporciones, aes(x = SEXO, y = proporcion, fill = matriculado)) +
  geom_col(position = "stack") +
  labs(
    title = "Proporción de Matriculados y No Matriculados por Género",
    x = "Género",
    y = "Proporción (%)",
    fill = "¿Matriculado?"
  ) +
  scale_fill_manual(values = colores_matricula) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

####quiero ver factor tiempo, saco tasa de matricula hombres y mujeres
####annado promedio para que se note que los hombres tienen peor matricula

tasas_por_sexo <- base_matriculado_corta %>% group_by(anno, SEXO) %>%
  summarise(tasa = mean(matriculado) * 100, .groups = "drop")

tasas_promedio <- tasas_por_sexo %>% group_by(anno) %>%
  summarise(SEXO = "Promedio", tasa = mean(tasa), .groups = "drop")

tasas_todas <- bind_rows(tasas_por_sexo, tasas_promedio) %>%
  mutate(SEXO = factor(SEXO, levels = c("Hombre", "Mujer", "Promedio")))

ggplot(tasas_todas, aes(x = anno, y = tasa, color = SEXO, linetype = SEXO)) +
  geom_line(size = 1) + geom_point() +
  geom_text(aes(label = round(tasa, 1)), vjust = -0.5, size = 3.5) +
  scale_color_manual(
    values = c(
      "Hombre" = "#6E7B8B",
      "Mujer" = "#B0C4DE",
      "Promedio" = "#CDCDC1")) +
  scale_linetype_manual(
    values = c(
      "Hombre" = "solid",
      "Mujer" = "solid",
      "Promedio" = "dashed")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Tasa de matrícula por cohorte y género",
       x = "Cohorte",
       y = "Tasa de matrícula (%)",
       color = "Género",
       linetype = "Género") +
  theme_minimal(base_size = 13)

####quiero ver ahora las preferencias de aquellos que ingresan
####uso otra base para esto
tablaaa <-base_panel_enriquecida_final_r %>%
  group_by(anno, matriculado) %>%
  summarise(frecuencia = n(), .groups = "drop")

base_panel_enriquecida_final_r <- base_panel_enriquecida_final_r %>%
  mutate(PREFERENCIA_RANGO = case_when(
    is.na(PREFERENCIA) ~ NA_character_,
    PREFERENCIA == 1 ~ "1° preferencia",
    PREFERENCIA == 2 ~ "2° preferencia",
    PREFERENCIA == 3 ~ "3° preferencia",
    PREFERENCIA %in% 4:5 ~ "4°-5° preferencia",
    PREFERENCIA > 6 ~ "6° o más",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(PREFERENCIA_RANGO))

matriculados <- base_panel_enriquecida_final_r %>%
  filter(matriculado == TRUE)

tabla_preferencias <- matriculados %>%
  count(PREFERENCIA_RANGO) %>%
  mutate(prop = round(n / sum(n) * 100, 1))

tabla_preferencias_sin_na <- tabla_preferencias %>%
  filter(!is.na(PREFERENCIA_RANGO))

ggplot(tabla_preferencias, aes(x = PREFERENCIA_RANGO, y = prop, fill = PREFERENCIA_RANGO)) +
  geom_col() +
  geom_text(aes(label = paste0(prop, "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = colores_preferencias) +
  labs(
    title = "Distribución de preferencia de matrícula",
    x = "Rango de preferencia",
    y = "Proporción (%)",
    fill = "Preferencia"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

####aca apilado y separado por genero

tabla_prop <- base_panel_enriquecida_final_r %>%
  group_by(SEXO, PREFERENCIA_RANGO) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEXO) %>%
  mutate(prop = n / sum(n) * 100) %>%
  # Aquí agregamos las etiquetas
  mutate(SEXO = factor(SEXO,
                       levels = c(1, 2),
                       labels = c("Hombres", "Mujeres")))


##### Gráfico proporcional apilado (barra 100%)
ggplot(tabla_prop, aes(x = SEXO, y = prop, fill = PREFERENCIA_RANGO)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Distribución proporcional de preferencia por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Rango de preferencia"
  ) +
  theme_minimal(base_size = 13)

###rendciones/inscripciones
#### Filtrar personas matriculadas en carreras de pedagogía

rendiciones <- base_matriculado_corta %>%
  count(rendiciones) %>%
  mutate(prop = round(n / sum(n) * 100, 1))

base_matriculado_corta %>%
  count(rendiciones, SEXO) %>%
  mutate(prop = round(n / sum(n) * 100, 1))

base_matriculado_corta %>%
  count(SEXO, rendiciones) %>%
  group_by(SEXO) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = rendiciones, y = prop, fill = SEXO)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribución de rendiciones por género",
    x = "Cantidad de rendiciones",
    y = "Porcentaje (%)"
  ) +
  scale_fill_manual(values = c("Mujer" = "#B0C4DE", "Hombre" = "#6E7B8B")) +
  theme_minimal(base_size = 14)

base_matriculado_corta %>%
  mutate(
    rendiciones_grupo = case_when(
      rendiciones %in% 1:5 ~ as.character(rendiciones),
      rendiciones >= 6 ~ "6 o más",
      TRUE ~ NA_character_
    ),
    rendiciones_grupo = factor(
      rendiciones_grupo,
      levels = c("1", "2", "3", "4", "5", "6 o más")
    )
  ) %>%
  count(SEXO, rendiciones_grupo) %>%
  group_by(SEXO) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ggplot(aes(x = SEXO, y = prop, fill = rendiciones_grupo)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = ifelse(prop > 3, paste0(round(prop, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Blues", name = "Rendiciones") +
  labs(
    title = "Distribución proporcional de rendiciones por género",
    x = "Género",
    y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 14)



matriculados <- base_panel_enriquecida_final_r %>%
  filter(matriculado == TRUE, !is.na(rendiciones), !is.na(SEXO))

#### Calcular estadísticos descriptivos
resumen_rendiciones <- matriculados %>%
  group_by(SEXO) %>%
  summarise(
    promedio_rendiciones = mean(rendiciones),
    mediana_rendiciones = median(rendiciones),
    maximo = max(rendiciones),
    minimo = min(rendiciones),
    n = n()
    )

print(resumen_rendiciones)

####Rama tecnico vs humanista/cientifico

tabla_rama_sexo <- base_matriculado_corta %>%
  filter(!is.na(rama), !is.na(SEXO)) %>%
  group_by(SEXO, rama) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEXO) %>%
  mutate(prop = n / sum(n) * 100)

ggplot(tabla_rama_sexo, aes(x = SEXO, y = prop, fill = rama)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_fill_manual(values = c("H" = "#B0C4DE", "T" = "#6E7B8B")) +
  labs(
    title = "Distribución proporcional de ramas por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Rama"
  ) +
  theme_minimal(base_size = 13)

####lo mismo pero para ver si hay diferencia entre no matriculado y matriculado

tabla_rama_sexo_mat <- base_matriculado_corta %>%
  filter(!is.na(rama), !is.na(SEXO), !is.na(matriculado)) %>%
  group_by(SEXO, matriculado, rama) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEXO, matriculado) %>%
  mutate(prop = n / sum(n) * 100)

ggplot(tabla_rama_sexo_mat, aes(x = SEXO, y = prop, fill = rama)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  facet_wrap(~ matriculado, labeller = labeller(matriculado = c("TRUE" = "Matriculado", "FALSE" = "No matriculado"))) +
  scale_fill_manual(values = c("H" = "#B0C4DE", "T" = "#6E7B8B")) +
  labs(
    title = "Distribución proporcional de ramas por género y matrícula",
    x = "Género",
    y = "Proporción (%)",
    fill = "Rama"
  ) +
  theme_minimal(base_size = 13)

####Ver dependencias de donde vienen pero por municipal, particular pagado, particular subvencionado

tabla_dependencia <- base_matriculado_corta %>%
  filter(!is.na(matriculado), !is.na(SEXO)) %>%
  group_by(SEXO, matriculado, dependencia_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEXO, matriculado) %>%
  mutate(prop = n / sum(n) * 100)

ggplot(tabla_dependencia, aes(x = SEXO, y = prop, fill = dependencia_label)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  facet_wrap(~ matriculado, labeller = labeller(matriculado = c("TRUE" = "Matriculado", "FALSE" = "No matriculado"))) +
  scale_fill_manual(values = c(
    "Municipal" = "#6E7B8B",
    "Particular subvencionado" = "#B0C4DE",
    "Particular pagado" = "#EEE8AA"
  )) +
  labs(
    title = "Distribución Tipo de dependencia, género y matrícula",
    x = "Género",
    y = "Proporción (%)",
    fill = "Dependencia"
  ) +
  theme_minimal(base_size = 13)

####ver correlacion

vars_continuas <- base_matriculado_corta %>%
  select(edad, rendiciones, PTJE_NEM, anno) %>%
  filter(if_all(everything(), ~ !is.na(.)))  # Elimina NA

# Matriz de correlación
correlacion <- cor(vars_continuas, use = "complete.obs", method = "pearson")
print(round(correlacion, 2))

corrplot(correlacion, method = "number", type = "lower", tl.col = "black")

####ver nem

resumen_nem_genero <- base_matriculado_corta %>%
  filter(!is.na(SEXO), !is.na(PTJE_NEM)) %>%
  group_by(SEXO) %>%
  summarise(
    promedio_nem = round(mean(PTJE_NEM), 1),
    desviacion_nem = round(sd(PTJE_NEM), 1),
    n = n()
  )

print(resumen_nem_genero)

####ordenar base

base_matriculado_corta <- base_matriculado_corta %>%
  left_join(
    base_panel_enriquecida_final_r %>% select(ID_aux, anno, PREFERENCIA),
    by = c("ID_aux", "anno"))

base_matriculado_corta %>%
  count(ID_aux, anno) %>%
  filter(n > 1)

##### Filtrar casos con NA en la variable PREFERENCIA
base_na_preferencia <- base_matriculado_corta %>%
  filter(is.na(PREFERENCIA))

base_filtrada_na <- base_panel_enriquecida_final_r %>%
  filter(ID_aux %in% base_na_preferencia$ID_aux)

#### Ver variables de interes
variables_interes <- base_matriculado_corta %>%
  select(SEXO, rendiciones, REGIMEN, GRUPO_DEPENDENCIA, RAMA, anno, matriculado, PTJE_NEM)

summary(variables_interes)
unique(variables_interes$SEXO)

variables_interes <- variables_interes %>%
  mutate(
    SEXO = case_when(
      SEXO == "Hombre" ~ 0,
      SEXO == "Mujer" ~ 1,
      TRUE ~ NA_real_))

variables_interes <- variables_interes %>%
  mutate(
    RAMA = case_when(
      RAMA %in% c("H1", "H2", "H3", "H4") ~ 2,
      RAMA %in% c("T1", "T2", "T3", "T4", "T5") ~ 1,
      TRUE ~ NA_real_))

unique(variables_interes$matriculado)

variables_interes <- variables_interes %>%
  mutate(matriculado = as.numeric(matriculado))

descriptivos <- psych::describe(variables_interes)

tabla_desc <- descriptivos %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  select(Variable, n, mean, sd, min, max) %>%
  rename(
    `N` = n,
    `Media` = mean,
    `Desv.Est.` = sd,
    `Mín.` = min,
    `Máx.` = max)

tabla_desc

base_matriculado_corta %>%
  count(ID_aux) %>%
  summarise(
    min = min(n),
    max = max(n),
    promedio = mean(n),
    median = median(n))

cor(base_matriculado_corta %>% select_if(is.numeric), use = "complete.obs")

vars_interes <- base_matriculado_corta %>%
  select(PTJE_NEM,
         edad,
         rendiciones,
         PROM_NOTAS)

variables_predictoras<- base_matriculado_corta %>%
  select(SEXO, rendiciones, REGIMEN, GRUPO_DEPENDENCIA, RAMA, PTJE_NEM)

variables_predictoras <- variables_predictoras %>%
  mutate(
    SEXO = case_when(
      SEXO == "Hombre" ~ 0,
      SEXO == "Mujer" ~ 1,
      TRUE ~ NA_real_))

variables_predictoras <- variables_predictoras %>%
  mutate(
    RAMA = case_when(
      RAMA %in% c("H1", "H2", "H3", "H4") ~ 2,
      RAMA %in% c("T1", "T2", "T3", "T4", "T5") ~ 1,
      TRUE ~ NA_real_))

matriz <- variables_predictoras %>%
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs")

tabla_cor <- as.data.frame(round(matriz, 2)) %>%
  rownames_to_column(var = "Variable") %>%
  as_tibble()

tabla_cor %>%
  gt() %>%
  tab_header(
    title = "Matriz de correlaciones entre variables numéricas")

cor_matrix <- cor(vars_interes, use = "complete.obs", method = "pearson")

summary(base_matriculado_corta)

####modelos y preparacion

base_matriculado_corta %>% 
  select(matriculado, SEXO, edad, anno, rendiciones, PTJE_NEM, rama, GRUPO_DEPENDENCIA) %>% 
  haven::zap_labels() %>% 
  sjmisc::descr(
    show = c("range", "mean", "sd", "NA.prc", "n")
  ) %>% 
  sjPlot::tab_df(title = "Variables modelo 1")

base_matriculado_corta <- base_matriculado_corta %>%
  mutate(
    RAMA = case_when(
      RAMA %in% c("H1", "H2", "H3", "H4") ~ 2,
      RAMA %in% c("T1", "T2", "T3", "T4", "T5") ~ 1,
      TRUE ~ NA_real_))

###modelo simple 

modelo_1 <- glm(matriculado ~ SEXO,
                data = base_matriculado_corta,
                family = binomial)
modelo_2 <- glm(matriculado ~ SEXO + edad, 
                data = base_matriculado_corta, 
                family = binomial)
modelo_3 <- glm(matriculado ~ rendiciones + PTJE_NEM + rama + dependencia_label,
                data = base_matriculado_corta,
                family = binomial)
modelo_4 <- glm(matriculado ~ SEXO + edad + rendiciones + PTJE_NEM + rama + dependencia_label,
                data = base_matriculado_corta,
                family = binomial)

tab_model(modelo_1, modelo_2, modelo_3, modelo_4,
          transform = "exp",  # odds ratios
          show.icc = FALSE,  
          show.aic = TRUE,
          show.obs = TRUE,
          dv.labels = c("Genero",
                        "Variables personales",
                        "Variables academicas",
                        "Modelo total"),
          title = "Modelos Logísticos paso a paso (Odds Ratios)")

library(modelsummary)

models <- list(
  "Genero" = modelo_1,
  "Variables personales" = modelo_2,
  "Variables academicas" = modelo_3,
  "Modelo total" = modelo_4)

modelsummary(
  models,
  statistic = "({std.error})",   # SE debajo entre paréntesis
  estimate = "{estimate}",       # coef logit
  title = "Modelos Logísticos (logits con SE debajo)")



##base de datos abierta 1/2

base_unica <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/base_unica.csv",delim = ",")
base_propu_trayec_r <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/base_propu_trayec_r.csv",delim = ",")
base_para_modelo <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/base_para_modelo.csv",delim = ",")
base_trayectorias_SC_r <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/base_trayectorias_SC_r.csv",delim = ",")

###codigos

#### Paleta personalizada

colores_egreso <- c("Sí" = "#698B69", "No" = "#CD3333")  

####ver duracion de los estudios

duracion_promedio_categoria <- base_propu_trayec_r %>% group_by(categoria_peda) %>%
  summarise(promedio_duracion = mean(dur_estudio_carr, na.rm = TRUE))

####ver base 

glimpse(base_propu_trayec_r)
table(base_unica$gen_alu)
table(base_unica$egreso)
table(base_unica$egreso, base_unica$sexo)

####calcular duración individual (por cantidad de años con presencia en la carrera)

duracion_individual <- base_propu_trayec_r %>%
  filter(!is.na(cat_periodo)) %>%  # asegurarse que anno exista
  group_by(mrun) %>%
  summarise(
    duracion_carrera = n_distinct(cat_periodo),
    sexo = first(gen_alu),   
    egresado = first(egresa),
    categoria_peda = first(categoria_peda))

duracion_genero_categoria <- duracion_individual %>%
  group_by(sexo, categoria_peda) %>%
  summarise(
    promedio_anios = mean(duracion_carrera, na.rm = TRUE),
    mediana_anios = median(duracion_carrera, na.rm = TRUE),
    n = n())

duracion_genero_egreso_categoria <- duracion_individual %>%
  group_by(sexo, egresado, categoria_peda) %>%
  summarise(
    promedio_anios = mean(duracion_carrera, na.rm = TRUE),
    mediana_anios = median(duracion_carrera, na.rm = TRUE),
    n = n())

duracion_individual <- base_propu_trayec_r %>%
  filter(!is.na(cat_periodo)) %>%
  group_by(mrun) %>%
  summarise(
    duracion_carrera = n_distinct(cat_periodo),
    sexo = first(gen_alu),
    egresado = first(egresa),
    categoria_peda = first(categoria_peda)
  ) %>%
  mutate(
    egresado = ifelse(egresado == 1, "Egresó", "No egresó"),
    sexo = ifelse(sexo == 1, "Hombre", "Mujer"))

#### grafico de velas no reportar pero si ver 
ggplot(duracion_individual, aes(
  x = ifelse(egresado == "Egresó", duracion_carrera, -duracion_carrera),
  y = categoria_peda,
  fill = sexo
)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Hombre" = "#6E7B8B", "Mujer" = "#B0C4DE")) +
  labs(
    title = "Duración en la carrera según egreso, categoría pedagógica y género",
    x = "Duración en años (negativo: no egresó / positivo: egresó)",
    y = "Categoría pedagógica",
    fill = "Género"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  facet_wrap(~categoria_peda, scales = "free_y") +
  theme(strip.text = element_text(face = "bold")) +
  coord_flip()

####ver base

glimpse(base_trayectorias_SC_r)
summary(base_trayectorias_SC_r)

base_trayectorias_SC_ultimo_dato <- base_trayectorias_SC_r %>%
  group_by(mrun) %>%
  filter(cat_periodo == max(cat_periodo, na.rm = TRUE)) %>%
  ungroup()

tabla_frecuencias <- table(base_trayectorias_SC_r$gen_alu, base_trayectorias_SC_r$categoria_peda)

prop_fila <- prop.table(tabla_frecuencias, margin = 1)
round(prop_fila, 3)

prop_total <- prop.table(tabla_frecuencias)
round(prop_total, 3)

####Proporcion de egreso por carrera y genero

proporciones_egreso <- base_propu_trayec_r %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    ),
    egresa = ifelse(egresa == 1, "Sí", "No"),
    categoria_peda = factor(categoria_peda, levels = c(1, 2, 3),
                            labels = c("Parvularia", "Básica", "Mixta"))
  ) %>%
  group_by(sexo, categoria_peda, egresa) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo, categoria_peda) %>%
  mutate(proporcion = n / sum(n) * 100)

#####con base unica

proporciones_egreso <- base_unica %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    ),
    egresa = ifelse(egresa == 1, "Sí", "No")
  ) %>%
  group_by(sexo, egresa) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(proporcion = n / sum(n) * 100)

ggplot(proporciones_egreso, aes(x = sexo, y = proporcion, fill = egresa)) +
  geom_col(position = "stack", color = "white") +
  labs(
    title = "Proporción de egreso por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "¿Egresó?"
  ) +
  scale_fill_manual(values = colores_egreso) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 13)





proporciones_egreso <- base_unica %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    ),
    egresa = ifelse(egresa == 1, "Sí", "No")) %>%
  group_by(sexo, categoria_peda, egresa) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo, categoria_peda) %>%
  mutate(proporcion = n / sum(n) * 100)

ggplot(proporciones_egreso, aes(x = categoria_peda, y = proporcion, fill = egresa)) +
  geom_col(position = "stack") +
  facet_wrap(~ sexo) +
  labs(
    title = "Proporción de egreso por carrera y género",
    x = "Categoría Pedagógica",
    y = "Proporción (%)",
    fill = "¿Egresó?"
  ) +
  scale_fill_manual(values = colores_egreso) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 13)

####evolucion por anno

tasas_por_egreso <- base_trayectorias_SC_ultimo_dato %>%
  mutate(
    SEXO = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    )
  ) %>%
  group_by(cat_periodo, SEXO) %>%
  summarise(tasa = mean(egresa, na.rm = TRUE) * 100, .groups = "drop")

##### Promedio general por año
tasas_promedio <- tasas_por_egreso %>%
  group_by(cat_periodo) %>%
  summarise(SEXO = "Promedio", tasa = mean(tasa), .groups = "drop")

##### Unir
tasas_todas <- bind_rows(tasas_por_egreso, tasas_promedio) %>%
  mutate(SEXO = factor(SEXO, levels = c("Hombre", "Mujer", "Promedio")))

tasas_todas_filtradas <- tasas_todas %>%
  filter(cat_periodo <= 2022)

##### Gráfico veste no lo reporto
ggplot(tasas_todas_filtradas, aes(x = cat_periodo, y = tasa, color = SEXO, linetype = SEXO)) +
  geom_line(size = 1) +
  geom_point() +
  geom_text(aes(label = round(tasa, 1)), vjust = -0.5, size = 3.5) +
  scale_color_manual(
    values = c(
      "Hombre" = "#6E7B8B",
      "Mujer" = "#B0C4DE",
      "Promedio" = "#CDCDC1"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Hombre" = "solid",
      "Mujer" = "solid",
      "Promedio" = "dashed"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Tasa de egreso observada por año y género",
    x = "Año observado (último año por persona)",
    y = "Tasa de egreso (%)",
    color = "Sexo",
    linetype = "Sexo"
  ) +
  theme_minimal(base_size = 13)

####proporciones instituciones

base_propu_trayec_r <- base_propu_trayec_r %>%
  filter(tipo_inst_3 != "Universidades (* Carrera en Convenio)") %>%
  mutate(
    tipo_inst_recod = case_when(
      tipo_inst_3 %in% c("Universidades Privadas", "Universidades Privadas CRUCH") ~ "Universidad privada",
      tipo_inst_3 == "Universidades Estatales CRUCH" ~ "Universidad estatal",
      tipo_inst_3 == "Institutos Profesionales" ~ "Instituto profesional",
      TRUE ~ NA_character_
    ),
    tipo_inst_recod = factor(tipo_inst_recod, levels = c("Universidad estatal", "Universidad privada", "Instituto profesional"))
  )

proporciones_inst <- base_propu_trayec_r %>%
  filter(!is.na(tipo_inst_recod)) %>%
  group_by(cat_periodo, tipo_inst_recod) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cat_periodo) %>%
  mutate(proporcion = n / sum(n))

ggplot(proporciones_inst, aes(x = cat_periodo, y = proporcion, fill = tipo_inst_recod)) +
  geom_area(color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Universidad estatal" = "#6E7B8B",
      "Universidad privada" = "#B0C4DE",
      "Instituto profesional" = "#CDCDC1"
    )
  ) +
  labs(
    title = "Evolución proporcional de matrícula por tipo de institución",
    x = "Año",
    y = "Proporción del total",
    fill = "Tipo de institución"
  ) +
  theme_minimal(base_size = 13)

proporciones_inst_genero <- base_propu_trayec_r %>%
  filter(!is.na(tipo_inst_recod), gen_alu %in% c(1, 2)) %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    )
  ) %>%
  group_by(cat_periodo, tipo_inst_recod, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cat_periodo, sexo) %>%
  mutate(proporcion = n / sum(n))

#####grafico comparativo entre hombres y mujeres segun institucion
ggplot(proporciones_inst_genero, aes(x = cat_periodo, y = proporcion, fill = tipo_inst_recod)) +
  geom_area(color = "white") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Universidad estatal" = "#6E7B8B",
      "Universidad privada" = "#B0C4DE",
      "Instituto profesional" = "#CDCDC1"
    )
  ) +
  labs(
    title = "Evolución de matrícula por institución y género",
    x = "Año",
    y = "Proporción del total",
    fill = "Tipo de institución"
  ) +
  facet_wrap(~ sexo) +
  theme_minimal(base_size = 13)

####ver por género apilado

proporciones_inst_genero <- base_para_modelo %>%
  filter(!is.na(tipo_inst_recod), gen_alu %in% c(1, 2)) %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    )
  ) %>%
  group_by(sexo, tipo_inst_recod) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(proporcion = n / sum(n))

proporciones_inst_genero <- proporciones_inst_genero %>%
  mutate(
    tipo_inst_recod = factor(
      tipo_inst_recod,
      levels = c(
        "Universidad estatal",
        "Universidad privada",
        "Instituto profesional")))

ggplot(proporciones_inst_genero,
       aes(x = sexo, y = proporcion, fill = tipo_inst_recod)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = scales::percent(proporcion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Universidad estatal"   = "#6E7B8B",
      "Universidad privada"   = "#B0C4DE",
      "Instituto profesional" = "#CDCDC1"
    )
  ) +
  labs(
    title = "Distribución de matrícula por tipo de institución y género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Tipo de institución"
  ) +
  theme_minimal(base_size = 13)

#####apilado por tipo de pedagogía

proporciones_inst_genero <- base_para_modelo %>%
  filter(!is.na(tipo_inst_recod), gen_alu %in% c(1, 2)) %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    )
  ) %>%
  group_by(sexo, tipo_inst_recod,categoria_peda) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo,categoria_peda) %>%
  mutate(proporcion = n / sum(n))

proporciones_inst_genero <- proporciones_inst_genero %>%
  mutate(
    tipo_inst_recod = factor(
      tipo_inst_recod,
      levels = c(
        "Universidad estatal",
        "Universidad privada",
        "Instituto profesional")))

ggplot(proporciones_inst_genero,
       aes(x = sexo, y = proporcion, fill = tipo_inst_recod)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = scales::percent(proporcion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Universidad estatal"   = "#6E7B8B",
      "Universidad privada"   = "#B0C4DE",
      "Instituto profesional" = "#CDCDC1"
    )
  ) +
  
  labs(
    title = "Distribución por género y categoría pedagógica",
    x = "Género",
    y = "Proporción (%)",
    fill = "Tipo de institución"
  ) +
  
  facet_wrap(~ categoria_peda, nrow = 1) +
  theme_minimal(base_size = 13)




####ver casa de estudios 

unique(base_para_modelo$cod_inst)

####preparacion y modelos

# Conteo de NA por variable
na_count <- sapply(base_para_modelo, function(x) sum(is.na(x)))
na_prop  <- sapply(base_para_modelo, function(x) mean(is.na(x)))

na_table <- data.frame(
  variable = names(na_count),
  n_NA     = na_count,
  prop_NA  = round(na_prop * 100, 2)
)

View(na_table)   # tabla ordenada


base_para_modelo <- base_para_modelo %>%
  mutate(egreso = factor(egresa, levels = c(0,1), labels = c("No","Sí")),
         sexo = factor(gen_alu, levels = c(1,2), labels = c("Hombre","Mujer")))


num_vars <- base_para_modelo %>%
  select(where(is.numeric))

correlaciones<- correlate(num_vars)
correlaciones


modelo_test <- glm(egreso ~ sexo + categoria_peda + tipo_inst_recod,
                   data = base_para_modelo,
                   family = binomial)

modelo_test

tab_model(modelo_test,
          transform = "exp",  # muestra OR en vez de log-odds
          show.ci = TRUE,
          show.aic = TRUE,
          show.obs = TRUE,
          digits = 3,
          pred.labels = c("Intercepto",
                          "Sexo (Mujer)",
                          "Pedagogía Mixta",
                          "Educación Parvularia",
                          "Univ. Privada",
                          "Inst. profesional"),
          dv.labels = "Probabilidad de Egreso")
####base unica

base_unica <- base_para_modelo %>%
  arrange(mrun, cod_inst, cat_periodo) %>%   # ordenar por persona, carrera e inicio
  group_by(mrun) %>%               # agrupar por persona y carrera
  slice_head(n = 1) %>%                      
  ungroup()

proporciones_egreso <- base_unica %>%
  mutate(
    sexo = case_when(
      gen_alu == 1 ~ "Hombre",
      gen_alu == 2 ~ "Mujer"
    ),
    egresa = ifelse(egresa == 1, "Sí", "No")) %>%
  group_by(sexo, categoria_peda, egresa) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo, categoria_peda) %>%
  mutate(proporcion = n / sum(n) * 100)

ggplot(proporciones_egreso, aes(x = categoria_peda, y = proporcion, fill = egresa)) +
  geom_col(position = "stack") +
  facet_wrap(~ sexo) +
  labs(
    title = "Proporción de egreso por carrera y género",
    x = "Categoría Pedagógica",
    y = "Proporción (%)",
    fill = "¿Egresó?"
  ) +
  scale_fill_manual(values = colores_egreso) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 13)

##### Revisar resultado
nrow(base_unica)                 
n_distinct(base_unica$mrun)      


modelo_reg_1 <- glm(egresa ~ sexo, 
                    data = base_unica, 
                    family = binomial)
modelo_reg_2 <- glm(egresa ~ categoria_peda + tipo_inst_recod, 
                    data = base_unica, 
                    family = binomial)
modelo_reg_3 <- glm(egresa ~ sexo + tipo_inst_recod + categoria_peda, 
                    data = base_unica, 
                    family = binomial)

tab_model(
  modelo_reg_1, modelo_reg_2, modelo_reg_3,
  transform = "exp",        # OR en vez de log-odds
  show.icc = TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  dv.labels = c("Género", "Educativas", "Modelo completo"),
  title = "Modelos de regresión logística sobre probabilidad de egreso")

models_1 <- list(
  "Genero" = modelo_reg_1,
  "Educativas" = modelo_reg_2,
  "Modelo completo" = modelo_reg_3)

modelsummary(
  models_1,
  statistic = "({std.error})",   # SE debajo entre paréntesis
  estimate = "{estimate}",       # coef logit
  title = "Modelos Logísticos (logits con SE debajo)")

######multinivel

modelo_multinivel_1 <- glmer(egresa ~ sexo + (1 |cod_inst),
                             data = base_unica,
                             family = binomial)
modelo_multinivel_2 <- glmer(egresa ~ categoria_peda + tipo_inst_recod + (1 |cod_inst),
                             data = base_unica,
                             family = binomial)
modelo_multinivel_3 <- glmer(egresa ~ sexo + categoria_peda + tipo_inst_recod + (1 |cod_inst),
                             data = base_unica,
                             family = binomial)

tab_model(
  modelo_multinivel_1,modelo_multinivel_2,modelo_multinivel_3,
  transform = "exp",   # odds ratios
  show.icc =  TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  dv.labels = c("Género", "Educativas", "Modelo completo"),
  title = "Modelo multinivel: probabilidad de egreso")

models_2 <- list(
  "Genero" = modelo_multinivel_1,
  "Educativas" = modelo_multinivel_2,
  "Modelo completo" = modelo_multinivel_3)

modelsummary(
  models_2,
  statistic = "({std.error})",   # SE debajo entre paréntesis
  estimate = "{estimate}",       # coef logit
  title = "Modelos Logísticos (logits con SE debajo)")

##base de datos abierta 2/2

docente_fil_r <- read_delim("https://raw.githubusercontent.com/sophkar/tesis/main/docente_fil_r.csv",delim = ",")

####ver base 

glimpse(docente_fil_r )
table(docente_fil_r $DOC_GENERO)

#### Paleta personalizada

colores_syn <- c("Sí" = "#698B69", "No" = "#CD3333")

colores_preferencias <- c(
  "1–19"   = "#63B8FF",
  "20–29"   = "#5CACEE",
  "30–39"   = "#4594CD",
  "40–45"= "#4682B4",
  "46+"         = "#36648B")

colores_rur <- c("Urbano" = "#CD3333", "Rural" = "#698B69")

colores_genero <- c("Hombre" = "#6E7B8B", "Mujer" = "#B0C4DE")

###codigos

####1.generar jerarquía - generar ascenso 

docente_fil_r <- docente_fil_r %>% mutate(docente_aula = ifelse(ID_IFP %in% c(1, 17), 1, 0))
frq(docente_fil_r$docente_aula)

docente_fil_r <- docente_fil_r %>% mutate(directivo = ifelse(ID_IFP %in% c(4, 16), 1, 0))

tabla_dir_genero <- docente_fil_r %>%
  group_by(genero_lbl, directivo_lbl) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    prop_dentro_genero = round(100 * n / sum(n), 1)  # proporción en %
  ) %>%
  ungroup()

tabla_dir_genero

docente_fil_r <- docente_fil_r %>%
  arrange(mrun, RBD, AGNO) %>%
  group_by(mrun, RBD) %>%
  mutate(
    ascenso = ifelse(!is.na(lag(docente_aula)) & lag(docente_aula == 1) & directivo == 1, 1, 0)
  ) %>%
  ungroup()

frq(docente_fil_r$ascenso)

docente_fil_r <- docente_fil_r %>% mutate(genero_lbl = factor(DOC_GENERO,
                                                          levels = c(1, 2),
                                                          labels = c("Hombre", "Mujer")),
                                      ascenso_lbl = factor(ascenso,
                                                           levels = c(0, 1),
                                                           labels = c("No", "Sí")))

proporciones_ascenso <- docente_fil_r %>%
  filter(!is.na(genero_lbl), !is.na(ascenso_lbl)) %>%
  group_by(genero_lbl, ascenso_lbl) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(proporcion = n / sum(n)) %>%
  ungroup()

ggplot(proporciones_ascenso,
       aes(x = genero_lbl, y = proporcion, fill = ascenso_lbl)) +
  geom_col(position = "stack") +
  labs(
    title = "Proporción de ascenso por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "¿Ascenso?"
  ) +
  scale_fill_manual(values = colores_syn) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_minimal(base_size = 13)    

docente_fil_r <- docente_fil_r %>%
  arrange(mrun, RBD, AGNO) %>%
  group_by(mrun, RBD) %>%
  mutate(
    # contador acumulado de años "docente_aula==1" hasta el año anterior
    docente_acum_prev = lag(cumsum(docente_aula == 1), default = 0L),
    
    # tiempo hasta el ascenso: SOLO se informa en el año donde ascenso==1
    tiempo_ascenso_doc = ifelse(ascenso == 1, docente_acum_prev, NA_integer_)
  ) %>%
  ungroup()

ascenso_tiempos <- docente_fil_r %>%
  filter(ascenso == 1, !is.na(tiempo_ascenso_doc)) %>%
  group_by(DOC_GENERO) %>%
  summarise(
    n = n(),
    promedio_anios = mean(tiempo_ascenso_doc, na.rm = TRUE),
    mediana_anios  = median(tiempo_ascenso_doc, na.rm = TRUE),
    minimo_anios   = min(tiempo_ascenso_doc, na.rm = TRUE),
    maximo_anios   = max(tiempo_ascenso_doc, na.rm = TRUE))

ascenso_tiempos

ascenso_tiempos <- docente_fil_r %>%
  filter(ascenso == 1, !is.na(tiempo_ascenso_doc)) %>%
  mutate(
    genero_lbl = factor(DOC_GENERO, levels = c(1,2),
                        labels = c("Hombre", "Mujer")),
    anios_asc = tiempo_ascenso_doc)

proporciones_asc <- ascenso_tiempos %>%
  group_by(genero_lbl, anios_asc) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

##### Gráfico en proporciones
ggplot(proporciones_asc, aes(x = genero_lbl, y = prop, fill = factor(anios_asc))) +
  geom_col(position = "stack") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(
    title = "Tiempo hasta ascenso por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Años hasta ascender"
  ) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_minimal(base_size = 13)

###2.comprobar diferencias en horas hvsm 

frq(docente_fil_r$HORAS_CONTRATO)
frq(docente_fil_r$DOC_GENERO)
frq(docente_fil_r$horas_grupo_)

tabla_horas_genero <- docente_fil_r %>%
  filter(!is.na(horas_grupo_), !is.na(DOC_GENERO)) %>%
  count(DOC_GENERO, horas_grupo_, name = "n") %>%
  group_by(DOC_GENERO) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(tabla_horas_genero,
       aes(x = DOC_GENERO, y = prop, fill = horas_grupo_)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = colores_preferencias) +
  labs(
    title = "Distribución de horas de contrato por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Grupo de horas"
  ) +
  theme_minimal(base_size = 13)

ggplot(
  tabla_horas_genero %>% filter(horas_grupo_ != "46+"),
  aes(x = DOC_GENERO, y = prop, fill = horas_grupo_)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = colores_preferencias) +
  labs(
    title = "Distribución de horas de contrato por género",
    x = "Género",
    y = "Proporporción (%)",
    fill = "Grupo de horas"
  ) +
  theme_minimal(base_size = 13)


base_filtrada <- docente_fil_r %>%
  filter(!is.na(horas_grupo_),
         horas_grupo_ != 0)

ggplot(tabla_horas_genero %>% filter(horas_grupo_ != "46+"),
       aes(x = DOC_GENERO, y = prop, fill = horas_grupo_)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Hombre", "Mujer")) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = colores_preferencias) +
  labs(
    title = "Distribución de horas de contrato por género",
    x = "Género", y = "Proporción (%)", fill = "Grupo de horas"
  ) +
  theme_minimal(base_size = 13)

###5.comprobar ruralidad hvsm 

tabla_rural_genero <- docente_fil_r %>%
  filter(!is.na(RURAL_RBD), !is.na(DOC_GENERO)) %>%
  group_by(DOC_GENERO, RURAL_RBD) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop_dentro_genero = n / sum(n))

tabla_rural_genero

#### Tabla: ruralidad dentro de cada género
tabla_rural_genero <- docente_fil_r %>%
  filter(!is.na(RURAL_RBD), !is.na(DOC_GENERO)) %>%
  group_by(DOC_GENERO, RURAL_RBD) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    prop_dentro_genero = n / sum(n),
    rural_lbl = factor(RURAL_RBD, levels = c(0,1), labels = c("Urbano","Rural")),
    genero_lbl = factor(DOC_GENERO, levels = c(1,2), labels = c("Hombre","Mujer"))
  )

tabla_rural_genero

ggplot(tabla_rural_genero,
       aes(x = genero_lbl, y = prop_dentro_genero, fill = rural_lbl)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop_dentro_genero, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = colores_rur) +
  labs(
    title = "Proporción de ruralidad dentro de cada género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Tipo de establecimiento"
  ) +
  theme_minimal(base_size = 13)

###6.ver tipos de establecimientos hvsm 

docente_fil_r <- docente_fil_r %>%
  mutate(
    dependencia = case_when(
      COD_DEPE %in% c(1, 2, 5, 6) ~ "Municipal",
      COD_DEPE == 3 ~ "Particular Subvencionado",
      COD_DEPE == 4 ~ "Particular Pagado",
      TRUE ~ NA_character_
    ),
    dependencia = factor(
      dependencia,
      levels = c("Municipal", "Particular Subvencionado", "Particular Pagado")))

tabla_dep_genero <- docente_fil_r %>%
  filter(!is.na(DOC_GENERO), !is.na(dependencia)) %>%
  group_by(DOC_GENERO, dependencia) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

ggplot(tabla_dep_genero,
       aes(x = factor(DOC_GENERO, levels = c(1,2), labels = c("Hombre","Mujer")),
           y = prop,
           fill = dependencia)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = c(
    "Municipal" = "#6E7B8B",
    "Particular Subvencionado" = "#B0C4DE",
    "Particular Pagado" = "#CDCDC1"
  )) +
  labs(
    title = "Distribución dependencia por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Dependencia"
  ) +
  theme_minimal(base_size = 13)

###7.ver abandono - noveles - rotacion

docente_fil_r <- docente_fil_r %>%
  mutate(
    anio_nac = as.integer(substr(DOC_FEC_NAC, 1, 4)),
    edad = AGNO - anio_nac)

docente_fil_r <- docente_fil_r %>%
  arrange(mrun, AGNO) %>%
  group_by(mrun) %>%
  mutate(
    # diferencia con el año anterior
    gap = AGNO - lag(AGNO),
    
    # edad en el año previo (para evaluar la condición de jubilación)
    edad_prev = lag(edad),
    genero_prev = lag(DOC_GENERO),
    
    # abandono si hay 5+ años de ausencia entre apariciones
    abandono_interno = case_when(
      genero_prev == 2 & edad_prev < 60 & gap >= 5 ~ 1,   # mujer
      genero_prev == 1 & edad_prev < 65 & gap >= 5 ~ 1,   # hombre
      TRUE ~ 0
    )
  ) %>%
  ungroup()

abandono_final <- docente_fil_r %>%
  group_by(mrun) %>%
  summarise(
    ultimo_agno = max(AGNO, na.rm = TRUE),
    genero = last(DOC_GENERO),
    edad_ultimo = last(edad)
  ) %>%
  mutate(
    abandono_tope = case_when(
      genero == 2 & edad_ultimo < 60 & (2024 - ultimo_agno) >= 5 ~ 1,
      genero == 1 & edad_ultimo < 65 & (2024 - ultimo_agno) >= 5 ~ 1,
      TRUE ~ 0))

docente_fil_r<- docente_fil_r %>%
  arrange(mrun, AGNO) %>%
  group_by(mrun) %>%
  mutate(
    anio_nac = as.integer(substr(DOC_FEC_NAC, 1, 4)),
    edad     = AGNO - anio_nac,
    
    # --- Abandono por brecha interna (cuando reaparece tras ≥5 años) ---
    gap       = AGNO - lag(AGNO),
    edad_prev = lag(edad),
    gen_prev  = lag(DOC_GENERO),
    abandono_interno = as.integer(
      (!is.na(gap) & gap >= 5) &
        (
          (gen_prev == 2 & !is.na(edad_prev) & edad_prev < 60) |   # mujer
            (gen_prev == 1 & !is.na(edad_prev) & edad_prev < 65)     # hombre
        )
    ),
    
    # --- Extensión al tope 2024 (si NO vuelve) ---
    ultimo_agno   = max(AGNO, na.rm = TRUE),
    # tomar género y edad del último año observado (sin joins)
    edad_ultimo   = { idx <- match(ultimo_agno, AGNO); edad[idx[1]] },
    genero_ultimo = { idx <- match(ultimo_agno, AGNO); DOC_GENERO[idx[1]] },
    
    abandono_tope = as.integer(
      (2024 - ultimo_agno) >= 5 &
        (
          (genero_ultimo == 2 & !is.na(edad_ultimo) & edad_ultimo < 60) |  # mujer
            (genero_ultimo == 1 & !is.na(edad_ultimo) & edad_ultimo < 65)    # hombre
        )
    ),
    
    # >>> Re-codificación final de *la misma* variable
    abandono_interno = pmax(abandono_interno, abandono_tope)
  ) %>%
  ungroup()

tabla_abandono_genero <- docente_fil_r%>%
  filter(!is.na(DOC_GENERO), !is.na(abandono_interno)) %>%
  group_by(DOC_GENERO, abandono_interno) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(
  tabla_abandono_genero,
  aes(
    x = factor(DOC_GENERO, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    y = prop,
    fill = factor(abandono_interno, levels = c(0, 1), labels = c("No", "Sí"))
  )
) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = colores_syn) +
  labs(
    title = "Abandono por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Abandono"
  ) +
  theme_minimal(base_size = 13)

###8.noveles

titulos_por_mrun <- base_propu_trayec_r %>%
  mutate(cat_periodo = as.integer(cat_periodo)) %>%         # por si viene como carácter
  group_by(mrun) %>%
  summarise(anio_titulacion = max(cat_periodo, na.rm = TRUE),
            .groups = "drop")

docente_fil_r<- docente_fil_r%>%
  left_join(titulos_por_mrun, by = "mrun")

docente_fil_r<- docente_fil_r%>%
  group_by(mrun) %>%
  mutate(anio_ingreso_lab = suppressWarnings(min(AGNO, na.rm = TRUE))) %>%
  ungroup()

docente_fil_r<- docente_fil_r%>%
  mutate(
    novel = as.integer(!is.na(anio_titulacion) & anio_ingreso_lab == anio_titulacion))

tabla_noveles_genero <- docente_fil_r%>%
  filter(!is.na(DOC_GENERO), !is.na(novel)) %>%            # noveles debe ser 0/1
  group_by(genero = factor(DOC_GENERO, levels = c(1, 2),
                           labels = c("Hombre", "Mujer")),
           novel) %>%
  summarise(N = n(), .groups = "drop_last") %>%
  group_by(genero) %>%
  mutate(prop = N / sum(N),
         prop_pct = percent(prop, accuracy = 0.1),
         novel_lbl = factor(novel, levels = c(1, 0),
                            labels = c("Sí", "No"))) %>%
  ungroup()

##### ver tabla
tabla_noveles_genero

ggplot(tabla_noveles_genero,
       aes(x = genero, y = prop, fill = novel_lbl)) +
  geom_col() +  # usamos prop ya calculada
  geom_text(aes(label = prop_pct),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = colores_syn) +
  labs(
    title = "Proporción de docentes noveles por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "Novel"
  ) +
  theme_minimal(base_size = 13)

docente_fil_r<- docente_fil_r%>%
  group_by(mrun) %>%
  mutate(
    # RBD distintos en los que ha estado la persona (ignorando NA)
    rbd_unicos = n_distinct(RBD[!is.na(RBD)]),
    # Rotación simple: cuántos cambios como mínimo implica haber pasado por rbd_unicos
    rotacion_total = pmax(rbd_unicos - 1L, 0L)
  ) %>%
  ungroup()

tabla_resumen <- docente_fil_r%>%
  distinct(mrun, genero_lbl, rotacion_total) %>%   # 1 fila por persona
  group_by(genero_lbl) %>%
  summarise(
    media_rotacion = mean(rotacion_total, na.rm = TRUE),
    sd_rotacion    = sd(rotacion_total, na.rm = TRUE),
    n_personas     = n(),
    .groups = "drop")

print(tabla_resumen)

base_personas <- docente_fil_r %>%
  distinct(mrun, DOC_GENERO, rotacion_total) %>%
  mutate(genero_lbl = factor(DOC_GENERO, levels = c(1, 2),
                             labels = c("Hombre", "Mujer")))

##### Proporciones DENTRO de cada género
##### (para hombres: cada proporción se divide por el total de hombres; idem mujeres)

tabla_rotacion_genero <- base_personas %>%
  group_by(genero_lbl, rotacion_total) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%     # <- clave: normaliza por género
  ungroup()

tabla_rotacion_genero %>%
  mutate(prop_pct = percent(prop, accuracy = 0.1)) %>%
  arrange(genero_lbl, rotacion_total)

tabla_rotacion_genero %>% print(n=30)

###9.ver idoneos 

base_unica_simplificada <- base_unica %>%
  select(mrun, categoria_peda) %>% 
  distinct(mrun, .keep_all = TRUE)

docente_fil_r <- docente_fil_r %>%
  left_join(base_unica_simplificada, by = "mrun")

docente_fil_r <- docente_fil_r %>%
  mutate(
    ejercicio_area = case_when(
      docente_aula == 1 & categoria_peda == "Parvularia" & COD_ENS_1 == 10 ~ 1,
      docente_aula == 1 & categoria_peda == "Basica"     & COD_ENS_1 == 110 ~ 1,
      docente_aula == 1 & categoria_peda == "Mixta"     & COD_ENS_1 %in% c(10, 110) ~ 1,
      TRUE ~ 0))

frq(docente_fil_r$ejercicio_area)
frq(docente_fil_r$categoria_peda)

tabla_ejercicio_genero_cat <- docente_fil_r %>%
  filter(!is.na(DOC_GENERO),
         !is.na(categoria_peda),
         !is.na(ejercicio_area)) %>%
  mutate(
    genero_lbl     = factor(DOC_GENERO, levels = c(1, 2),
                            labels = c("Hombre", "Mujer")),
    ejercicio_lbl  = factor(ifelse(ejercicio_area == 1, "Sí", "No"),
                            levels = c("Sí", "No")),
    categoria_lbl  = factor(categoria_peda,
                            levels = c("Parvularia", "Basica", "Mixta"))
  ) %>%
  group_by(categoria_lbl, genero_lbl, ejercicio_lbl) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(categoria_lbl, genero_lbl) %>%
  mutate(prop_dentro_genero = n / sum(n)) %>%
  ungroup()

tabla_ejercicio_genero <- docente_fil_r %>%
  filter(!is.na(DOC_GENERO),
         !is.na(ejercicio_area)) %>%   
  mutate(
    genero_lbl     = factor(DOC_GENERO, levels = c(1, 2),
                            labels = c("Hombre", "Mujer")),
    ejercicio_lbl  = factor(ifelse(ejercicio_area == 1, "Sí", "No"),
                            levels = c("Sí", "No"))
  ) %>%
  group_by(genero_lbl, ejercicio_lbl) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(genero_lbl) %>%
  mutate(prop_dentro_genero = n / sum(n)) %>%
  ungroup()

tabla_ejercicio_genero_cat <- tabla_ejercicio_genero_cat %>%
  mutate(ejercicio_lbl = fct_relevel(ejercicio_lbl, "Sí", "No"))

#### separado por categoría pedagógica
ggplot(tabla_ejercicio_genero_cat,
       aes(x = genero_lbl, y = prop_dentro_genero, fill = ejercicio_lbl)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop_dentro_genero, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = colores_syn,
                    name = "Ejercicio en el área titulada") +
  labs(
    title = "Proporción de ejercicio en el área titulada",
    x = "Género",
    y = "Proporción dentro del género"
  ) +
  facet_wrap(~ categoria_lbl, nrow = 1) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

####no ejercen

no_ejercen <- docente_fil_r %>%
  filter(ejercicio_area == 0 & !is.na(COD_ENS_1) & !is.na(ID_IFP))

tabla_destino <- no_ejercen %>%
  group_by(COD_ENS_1,ID_IFP) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))

tabla_destino_gen <- no_ejercen %>%
  mutate(
    genero_lbl = factor(DOC_GENERO,
                        levels = c(1,2),
                        labels = c("Hombre","Mujer"))
  ) %>%
  group_by(genero_lbl, COD_ENS_1,ID_IFP) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(genero_lbl) %>%
  mutate(prop_dentro = n / sum(n))

tabla_destino_gen

####posible sankey

base_sankey1 <- docente_fil_r %>% filter(ejercicio_area == 0)

base_sankey1 <- docente_fil_base_sankey1 <- base_sankey1 %>% select(DOC_GENERO, categoria_peda, ID_IFP, NIVEL1)

base_sankey1 <- base_sankey1 %>%
  ##### Crear etiquetas nuevas (NO sobreescribir originales)
  mutate(
    genero_lbl = case_when(
      DOC_GENERO == 1 ~ "Hombre",
      DOC_GENERO == 2 ~ "Mujer",
      is.na(DOC_GENERO) ~ "Sin dato",
      TRUE ~ "Otro/NA"
    ),
    categoria_lbl = case_when(
      categoria_peda %in% c("Basica","Básica") ~ "Básica",
      categoria_peda == "Parvularia"          ~ "Parvularia",
      categoria_peda == "Mixta"               ~ "Mixta",
      is.na(categoria_peda)                   ~ "Sin dato",
      TRUE ~ "Otro/NA"
    ),
    ifp_lbl = case_when(
      ID_IFP %in% c(1,17)         ~ "Docencia aula",
      ID_IFP %in% c(2,7,11,15)    ~ "Planta Técnico-pedagógica",
      ID_IFP %in% c(3,10,12,13,14,16) ~ "Planta Directiva",
      ID_IFP == 4                 ~ "Director(a)",
      ID_IFP == 8                 ~ "Inspector(a) General",
      ID_IFP == 9                 ~ "Orientador(a)",
      is.na(ID_IFP)              ~ "Sin dato",
      TRUE ~ "Otra/NA"
    ),
    nivel1_lbl = case_when(
      NIVEL1 == 0  ~ "No hace clases",
      NIVEL1 == 1  ~ "Parvularia",
      NIVEL1 == 2  ~ "Básica niños/as y jóvenes",
      NIVEL1 == 6  ~ "Básica adultos",
      NIVEL1 == 3  ~ "Especial",
      NIVEL1 == 4  ~ "Media H-C (N/J)",
      NIVEL1 == 7  ~ "Media H-C (Adultos)",
      NIVEL1 == 5  ~ "Media TP (N/J)",
      NIVEL1 == 8  ~ "Media TP (Adultos)",
      NIVEL1 == 9  ~ "Otro nivel",
      is.na(NIVEL1) ~ "Sin dato",
      TRUE ~ "Otra/NA"
    )
  ) %>%
  mutate(
    genero_lbl    = factor(genero_lbl,    levels = c("Hombre","Mujer","Sin dato","Otro/NA")),
    categoria_lbl = factor(categoria_lbl, levels = c("Parvularia","Básica","Mixta","Sin dato","Otro/NA")),
    ifp_lbl       = factor(ifp_lbl,       levels = c("Docencia aula","Planta Técnico-pedagógica","Planta Directiva",
                                                     "Director(a)","Inspector(a) General","Orientador(a)","Sin dato","Otra/NA")),
    nivel1_lbl    = factor(nivel1_lbl,    levels = c("No hace clases","Parvularia","Básica niños/as y jóvenes","Básica adultos",
                                                     "Especial","Media H-C (N/J)","Media H-C (Adultos)",
                                                     "Media TP (N/J)","Media TP (Adultos)","Otro nivel","Sin dato","Otra/NA"))
  )

###### Flujos (ya con *_lbl creadas y ejercicio_area == 0)
flows <- base_sankey1 %>%
  count(genero_lbl, categoria_lbl, ifp_lbl, nivel1_lbl, name = "value")

###### Nombres únicos por etapa (prefijos)
flows_uni <- flows %>%
  transmute(
    g = paste0("G: ", as.character(genero_lbl)),
    c = paste0("C: ", as.character(categoria_lbl)),
    f = paste0("F: ", as.character(ifp_lbl)),
    n = paste0("N: ", as.character(nivel1_lbl)),
    value
  )

###### Tabla de nodos (únicos globales)
nodes <- data.frame(
  name = unique(c(flows_uni$g, flows_uni$c, flows_uni$f, flows_uni$n)),
  stringsAsFactors = FALSE
)

###### Helper para mapear a índices 0-based
idx <- function(x) match(x, nodes$name) - 1

###### Enlaces por etapa (G->C, C->F, F->N)
links_gc <- flows_uni %>%
  group_by(g, c) %>% summarise(value = sum(value), .groups = "drop") %>%
  transmute(source = idx(g), target = idx(c), value)

links_cf <- flows_uni %>%
  group_by(c, f) %>% summarise(value = sum(value), .groups = "drop") %>%
  transmute(source = idx(c), target = idx(f), value)

links_fn <- flows_uni %>%
  group_by(f, n) %>% summarise(value = sum(value), .groups = "drop") %>%
  transmute(source = idx(f), target = idx(n), value)

links <- bind_rows(links_gc, links_cf, links_fn)

###### Sankey (prefijos garantizan unicidad)
p <- sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target", Value = "value", NodeID = "name",
  fontSize = 12, nodeWidth = 28, sinksRight = FALSE
)

###### (Opcional) Ocultar prefijos en las etiquetas visibles
p <- onRender(p, '
  function(el,x){
    d3.select(el).selectAll(".node text").each(function(){
      var t = d3.select(this).text();
      t = t.replace(/^G: /,"").replace(/^C: /,"").replace(/^F: /,"").replace(/^N: /,"");
      d3.select(this).text(t);
    });
  }
')

p

###10.preparar modelos

docente_fil_r <- docente_fil_r %>%
  mutate(optimo = case_when(
    ejercicio_area == 1 & horas_grupo_ == 40-45 ~ 1,
    TRUE ~ 0))

docente_fil_r <- docente_fil_r %>%
  mutate(
    optimo = as.integer(
      ejercicio_area == 1L &
        str_replace_all(str_squish(as.character(horas_grupo_)), "[−–—]", "-") == "40-45"
    ))

tabla_optimo_genero <- docente_fil_r %>%
  filter(!is.na(DOC_GENERO),
         !is.na(optimo)) %>%   # usamos optimo en lugar de ejercicio_area
  mutate(
    genero_lbl = factor(DOC_GENERO, levels = c(1, 2),
                        labels = c("Hombre", "Mujer")),
    optimo_lbl = factor(ifelse(optimo == 1, "Sí", "No"),
                        levels = c("Sí", "No"))
  ) %>%
  group_by(genero_lbl, optimo_lbl) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(genero_lbl) %>%
  mutate(prop_dentro_genero = n / sum(n)) %>%
  ungroup()

tabla_optimo_genero_cat <- docente_fil_r %>%
  filter(!is.na(DOC_GENERO),
         !is.na(categoria_peda),
         !is.na(optimo)) %>%
  mutate(
    genero_lbl = factor(DOC_GENERO, levels = c(1, 2),
                        labels = c("Hombre", "Mujer")),
    optimo_lbl = factor(ifelse(optimo == 1, "Sí", "No"),
                        levels = c("Sí", "No")),
    categoria_lbl = factor(categoria_peda,
                           levels = c("Parvularia", "Basica", "Mixta"))
  ) %>%
  group_by(categoria_lbl, genero_lbl, optimo_lbl) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(categoria_lbl, genero_lbl) %>%
  mutate(prop_dentro_genero = n / sum(n)) %>%
  ungroup()

ggplot(tabla_ejercicio_genero_cat,
       aes(x = genero_lbl, y = prop_dentro_genero, fill = ejercicio_lbl)) +
  geom_col(position = "fill") +
  geom_text(aes(label = percent(prop_dentro_genero, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = colores_syn,
    name = "Ejercicio área titulada"
  ) +
  labs(
    title = "Proporción de ejercicio área",
    x = "Género",
    y = "Proporción dentro del género"
  ) +
  facet_wrap(~ categoria_lbl, nrow = 1) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

table(docente_fil_r $ejercicio_area)
table(docente_fil_r $ejercicio_area, docente_fil_r $DOC_GENERO)
##### chequeo rápido
docente_fil_r %>% count(optimo) %>% mutate(prop = n / sum(n))

docente_fil_r <- docente_fil_r %>% mutate(optimo = as.integer(optimo))

vars_pull <- c("mrun", "tipo_inst_recod", "cod_inst", "nomb_inst")

base_princ <- docente_fil_r %>%
  transmute(
    optimo         = as.integer(optimo),           # DV 0/1
    DOC_GENERO     = as.factor(DOC_GENERO),
    categoria_peda = as.factor(categoria_peda),
    dependencia    = as.factor(dependencia),
    mrun           = as.factor(mrun),
    RBD            = as.factor(RBD) ) %>%
  tidyr::drop_na(optimo) %>%
  droplevels()

base_princ %>% summarise(n = n(), n_mrun = n_distinct(mrun), n_RBD = n_distinct(RBD))
frq(base_princ$DOC_GENERO)

system.time({
  fit_null_mrun <- glmer(optimo ~ 1 + (1 | mrun),
                         family = binomial, data = base_princ,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
})
icc(fit_null_mrun); isSingular(fit_null_mrun)

###11.generar modelos

base_princ <- docente_fil_r %>%
  transmute(
    optimo        = as.integer(optimo),     # DV 0/1
    DOC_GENERO    = as.factor(DOC_GENERO),
    categoria_peda= as.factor(categoria_peda),
    dependencia   = as.factor(dependencia),
    mrun          = as.factor(mrun),
    RBD           = as.factor(RBD)
  ) %>%
  tidyr::drop_na(optimo) %>%
  droplevels()

modelo_multinivel_1_ <- glmer(optimo ~ DOC_GENERO + (1 | mrun),
                             data = base_princ,
                             family = binomial)
modelo_multinivel_2_ <- glmer(optimo ~ categoria_peda + (1 | mrun),
                             data = base_princ,
                             family = binomial)
modelo_multinivel_3_ <- glmer(optimo ~  dependencia + (1 | mrun),
                             data = base_princ,
                             family = binomial)
modelo_multinivel_4_ <- glmer(optimo ~ DOC_GENERO + categoria_peda + dependencia + (1 | mrun),
                             data = base_princ,
                             family = binomial)

tab_model(
  modelo_multinivel_1_,modelo_multinivel_2_,modelo_multinivel_3_,modelo_multinivel_4_,
  transform = "exp",   # odds ratios
  show.icc =  TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  dv.labels = c("Género", "Educativas", "Dependencia laboral","Modelo completo"),
  title = "Modelo multinivel ejercicio óptimo")

### modelo doble

modelo_multinivel_1_1 <- glmer(optimo ~ DOC_GENERO + (1 | mrun)  + (1 | RBD),
                              data = base_princ,
                              family = binomial)
modelo_multinivel_2_1 <- glmer(optimo ~ categoria_peda + (1 | mrun) + (1 | RBD),
                              data = base_princ,
                              family = binomial)
modelo_multinivel_3_1 <- glmer(optimo ~  dependencia + (1 | mrun) + (1 | RBD),
                              data = base_princ,
                              family = binomial)
modelo_multinivel_4_1 <- glmer(optimo ~ DOC_GENERO + categoria_peda + dependencia + (1 | mrun) + (1 | RBD),
                              data = base_princ,
                              family = binomial)

tab_model(
  modelo_multinivel_1_1,modelo_multinivel_2_1,modelo_multinivel_3_1,modelo_multinivel_4_1,
  transform = "exp",   # odds ratios
  show.icc =  TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  dv.labels = c("Género", "Educativas", "Dependencia laboral","Modelo completo"),
  title = "Modelo multinivel ejercicio óptimo")

table(docente_fil_r $optimo, docente_fil_r $DOC_GENERO)

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(scales)

# Definir los colores para las categorías "Sí" y "No"
colores_syn <- c("Sí" = "#698B69", "No" = "#CD3333")

# Calcular las proporciones de 'optimo' (0 y 1) por 'DOC_GENERO'
proporciones <- base_princ %>%
  group_by(DOC_GENERO, optimo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(DOC_GENERO) %>%
  mutate(proporcion = n / sum(n) * 100)

proporciones <- base_princ %>%
  # Recodificar DOC_GENERO con mutate
  mutate(
    genero_lbl = case_when(
      DOC_GENERO == 1 ~ "Hombre",
      DOC_GENERO == 2 ~ "Mujer",
      TRUE ~ as.character(DOC_GENERO)  # Para mantener otros valores si existen
    )
  ) %>%
  group_by(genero_lbl, optimo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(genero_lbl) %>%
  mutate(proporcion = n / sum(n) * 100)

# Crear el gráfico con ggplot
ggplot(proporciones, aes(x = genero_lbl, y = proporcion, fill = factor(optimo, labels = c("No", "Sí")))) +
  geom_col(position = "stack") +
  labs(
    title = "Proporción de ejercicio óptimo por género",
    x = "Género",
    y = "Proporción (%)",
    fill = "¿Ejercicio óptimo?"
  ) +
  scale_fill_manual(values = colores_syn) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 13)

# Calcular las proporciones de 'optimo' (0 y 1) por 'DOC_GENERO'
proporciones <- base_princ %>%
  group_by(DOC_GENERO, optimo, categoria_peda) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(DOC_GENERO, categoria_peda) %>%
  mutate(proporcion = n / sum(n) * 100)

# Crear el gráfico con ggplot
ggplot(proporciones, aes(x = DOC_GENERO, y = proporcion, fill = factor(optimo, labels = c("No", "Sí")))) +
  geom_col(position = "stack") +
  facet_wrap(~categoria_peda, scales = "free_y") +  # Crear subgráficos por 'categoria_peda'
  labs(
    title = "Proporción de ejercicio óptimo por Género y Categoría Pedagógica",
    x = "Género",
    y = "Proporción (%)",
    fill = "¿Ejercicio óptimo?"
  ) +
  scale_fill_manual(values = colores_syn) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Formatear eje Y como porcentaje
  theme_minimal(base_size = 13)

proporciones <- base_princ %>%
  group_by(DOC_GENERO, optimo, categoria_peda) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(DOC_GENERO, categoria_peda) %>%
  mutate(
    proporcion = n / sum(n) * 100,
    DOC_GENERO = dplyr::recode_factor(DOC_GENERO,
                                      `1` = "Hombre",
                                      `2` = "Mujer"))

ggplot(proporciones, aes(x = DOC_GENERO, y = proporcion,
                         fill = factor(optimo, labels = c("No", "Sí")))) +
  geom_col(position = "stack") +
  facet_wrap(~categoria_peda) +   # <--- ahora todos comparten el mismo eje Y
  labs(
    title = "Proporción de ejercicio óptimo por Género y Categoría Pedagógica",
    x = "Género",
    y = "Proporción (%)",
    fill = "¿Ejercicio óptimo?"
  ) +
  scale_fill_manual(values = colores_syn) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 13)

