# Etapa IV - Analisis

## Previsualizacion de los datos
head (master_table_clean)
colnames (master_table_clean)
str (master_table_clean)
summary (master_table_clean)

## Estadisticas descriptivas -- Media, Mediana y Desvio estandar

### Tabla de medias por usuario
mean_users <- master_table_clean %>% 
  group_by (Id) %>% 
  summarise (
    mean_steps = mean (TotalSteps),
    mean_distance = mean (TotalDistance),
    mean_calories = mean (Calories)
  )
View (mean_users)

### Tabla de medianas por usuario
median_users <- master_table_clean %>% 
  group_by (Id) %>% 
  summarise (
    median_steps = median (TotalSteps),
    median_distance = median (TotalDistance),
    median_calories = median (Calories)
  )
View (median_users)

### Tabla de desvios por usuario 
sd_users <- master_table_clean %>% 
  group_by (Id) %>% 
  summarise (
    sd_steps = sd (TotalSteps),
    sd_distance = sd (TotalDistance),
    sd_calories = sd (Calories)
  )
View (sd_users)

## Graficos para entender distribuciones de datos --> todas son de cola derecha, es decir, concentran la mayor cantidad de datos a la izquierda
### Distribucion de pasos diarios
ggplot (data = master_table_clean) +
  geom_histogram (mapping = aes (x = TotalSteps), fill = "green") +
  labs (title = "Distribucion de pasos diarios")

### Distribucion de calorias diarias
ggplot (data = master_table_clean) +
  geom_histogram (mapping = aes (x = Calories), fill = "orange") +
  labs (title = "Distribucion de calorias diarias")

### Distribucion de la distancia total recorrida
ggplot (data = master_table_clean) +
  geom_histogram (mapping = aes (x = TotalDistance), fill = "blue") +
  labs (title = "Distribucion de distancia diaria recorrida")

## Graficos para relacionar variables

### Matriz de correlaciones
var_table <- master_table_clean %>% 
  dplyr::select ("TotalSteps", "TotalDistance", "Calories")

cor_matriz <- cor (var_table, use = "complete.obs")
corrplot (cor_matriz, method = "number", type = "upper")

### Dispersion 
ggplot (data = master_table_clean, aes (x = TotalSteps, y = Calories)) +
  geom_point (alpha = 0.5, color = "darkgreen") +
  geom_smooth (method = "lm", color = "red") +
  labs (title = "Relacion pasos-calorias")

## Agrupamos usuarios por nivel de actividad

### Tabla segun nivel de actividad
activity_table <- master_table_clean %>% 
  group_by (Id) %>% 
  summarise (mean_steps = mean (TotalSteps)) %>% 
  mutate (activity_level = case_when (
    mean_steps < 5000 ~ "Baja",
    mean_steps < 10000 ~ "Moderada",
    TRUE ~ "Alta"
  ))

### Grafico segun nivel de actividad
ggplot (data = activity_table, aes (x = activity_level, fill = activity_level)) +
  geom_bar () +
  labs (title = "Usuarios por nivel de actividad",
        x = "Nivel de actividad",
        y = "Cantidad de usuarios")

## Actividad segun dia de la semana

### Convertimos ActivityDate a formato fecha
master_table_clean <- master_table_clean %>% 
  mutate (ActivityDate = as.Date(ActivityDate))

### Tabla segun dias
weekday_table <- master_table_clean %>% 
  mutate (weekday = weekdays(ActivityDate)) %>% 
  group_by (weekday) %>% 
  summarise (avg_steps = mean (TotalSteps))

weekday_table <- weekday_table %>% 
  mutate (weekday = factor (weekday,
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ))) %>% 
  arrange (weekday)

### Grafico segun dias
ggplot (data = weekday_table, aes (x = weekday, y = avg_steps)) +
  geom_col (fill = "steelblue") +
  labs (title = "Promedio de pasos por dia de la semana",
        x = "Dia",
        y = "Pasos promedio")

## Promedio diario de pasos por usuario, en general, y su evolucion temporal

### Tabla 
daily_trend <- master_table_clean %>% 
  group_by (ActivityDate) %>% 
  summarise (avg_steps = mean (TotalSteps))

#### El dia con menos pasos totales realizados por los 35 usuarios en su conjunto fue el 2016-03-18, con tan solo 658 pasos registrados por cada uno de ellos, en promedio.
#### El dia con mas pasos totales realizados por el conjunto de usuarios que compartieron sus datos fue el 2016-04-12 con 10.780 para cada uno de ellos, en promedio.

### Grafico
ggplot (data = daily_trend, aes (x = ActivityDate, y = avg_steps)) +
  geom_line (color = "darkblue") +
  labs (title = "Tendencia del promedio diario de pasos",
        x = "Fecha",
        y = "Pasos promedio")

## Analisis relacion distancia - calorias
ggplot (data = master_table_clean, aes (x = TotalDistance, y = Calories)) +
  geom_point (color = "purple") +
  geom_smooth (method = "lm", color = "red") +
  labs (title = "Relacion distancia - calorias",
        x = "Distancia (km)",
        y = "Calorias")

## Nivel de actividad - dias de la semana
### Tabla que distingue dentro de cada dia la cantidad de pasos promedio que realiza cada grupo de actividad
activity_weekday <- master_table_clean %>%
  mutate (weekday = weekdays(ActivityDate)) %>%
  left_join (activity_table, by = "Id") %>% 
  group_by (activity_level, weekday) %>% 
  summarise (avg_steps = mean (TotalSteps), .groups = "drop" ) %>% 
  mutate (weekday = factor (weekday,
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ))) %>% 
  arrange (weekday)

### Grafico
ggplot (data = activity_weekday, aes (x = weekday, y = avg_steps, fill = activity_level)) +
  geom_col () +
  facet_wrap (~ activity_level) +
  theme (axis.text.x = element_text(angle = 90)) +
  labs (title = "Promedio de pasos por dia y nivel de activida",
        x = "Dias de la semana",
        y = "Pasos promedio")

## Activity_level vs Calories
### Tabla
calories_activity <- master_table_clean %>% 
  group_by(Id) %>% 
  summarise( 
    mean_steps = mean (TotalSteps),
    mean_calories = mean (Calories)) %>% 
  mutate (activity_level = case_when (
    mean_steps < 5000 ~ "Baja",
    mean_steps < 10000 ~ "Moderada",
    TRUE ~ "Alta"
  ))

### Grafico
ggplot(data = calories_activity, aes (x = activity_level, y = mean_calories, fill = activity_level)) +
  geom_boxplot() +
  labs (title = "Consumo calorico promedio segun nivel de actividad",
        x = "Nivel de actividad",
        y = "Calorias promedio consumidas")

## Reconocimiento de perfiles extremos (5 mas y menos activos)
### Pasos y calorias por usuario
user_calories_activity <- master_table_clean %>% 
  group_by(Id) %>% 
  summarise( 
    mean_steps = mean (TotalSteps),
    mean_calories = mean (Calories),
    active_days = n ())

### Top 5 mas activos
top5 <- user_calories_activity %>% 
  arrange (desc(mean_steps)) %>% 
  slice (1:5) %>% 
  mutate (group = "Top 5 activos")

### Top 5 menos activos
bottom5 <- user_calories_activity %>% 
  arrange (mean_steps) %>% 
  slice (1:5) %>% 
  mutate (group = "Top 5 menos activos")

### Unimos y graficamos
extreme_users <- bind_rows(top5, bottom5)

ggplot (extreme_users, aes (x = reorder(Id, mean_steps), y = mean_calories, fill = group)) +
  geom_col() +
  theme (axis.text.x = element_text(angle = 90)) +
  labs (title = "Top 5 vs Bottom 5 usuarios",
        x = "Usuario",
        y = "Calorias promedio")


############################################################################

#No incorporados en el analisis, fueron ideas de Chat GPT

## Analisis de correlacion entre variables usando valor-p

### Variables de interes
var_table <- master_table_clean %>% 
  dplyr::select ("TotalSteps", "TotalDistance", "Calories")

### Promedios por usuario
var_user <- master_table_clean %>% 
  group_by(Id) %>% 
  summarise(
    TotalSteps = mean(TotalSteps),
    TotalDistance = mean (TotalDistance),
    Calories = mean (Calories),
    .groups = "drop"
  ) %>% 
  dplyr::select (-Id)

### Correlacion y valor-p 
#### Crea funcion matematica 
cor_pmat <- function(mat) {
  m <- as.matrix(mat)
  n <- ncol(m)
  p <- matrix(NA_real_, n, n)
  diag(p) <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      ct <- cor.test(m[, i], m[, j], use = "complete.obs")
      p[i, j] <- p[j, i] <- ct$p.value
    }
  }
  colnames(p) <- colnames(m); rownames(p) <- colnames(m)
  p
}

#### Usando los datos diarios
cor_matriz <- cor (var_table, use = "complete.obs")
valor_p   <- cor_pmat(var_table)
corrplot(cor_matriz, method = "number", type = "upper",
         p.mat = valor_p, sig.level = 0.05, insig = "blank",
         tl.col = "black")

#### Usando los promedios por usuario
cor_matriz1 <- cor (var_user, use = "complete.obs")
valor_p1 <- cor_pmat(var_user)
corrplot (cor_matriz1, method = "number", type = "upper",
         p.mat = valor_p, sig.level = 0.05, insig = "blank",
         tl.col = "black")

## Grafico boxplot con violines
ggplot(calories_activity, aes(x = activity_level, y = mean_calories, fill = activity_level)) +
  geom_violin(alpha = 0.3, show.legend = FALSE) +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Consumo calórico promedio según nivel de actividad", x = "Nivel de actividad", y = "Calorías promedio")