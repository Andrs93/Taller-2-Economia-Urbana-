
required_pkgs <- c("sf","ggplot2","dplyr","tidyr","viridis","MASS","geosphere","gridExtra")
to_install <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(MASS)       # kde2d
library(geosphere)  # distHaversine
library(gridExtra)  # grid.arrange

# Crear carpeta de salida
dir.create("outputs1")

# ----------------------------
# Cargar datos
# ----------------------------
load("Taller2_Ejercicio1.Rdata") 
ls()
# ----------------------------
# 1) Réplica Figura 1
#    (Mapas comparativos: ubicaciones restaurantes 2004 vs 2012)
# ----------------------------
# ---------------------------------------------------------
# 2. Convertir restaurantes en sf (puntos)
# ---------------------------------------------------------
rest2004 <- restaurants %>%
  filter(!is.na(long2004), !is.na(lat2004)) %>% 
  st_as_sf(coords = c("long2004", "lat2004"), crs = 4326)

rest2012 <- restaurants %>%
  filter(!is.na(long2012), !is.na(lat2012)) %>% 
  st_as_sf(coords = c("long2012", "lat2012"), crs = 4326)

# ---------------------------------------------------------
# 3. Conteos por barrio
# ---------------------------------------------------------
#Cambiamos nombre para hacer llave 
df_barrios <- barrios %>% rename(zona180 = ZONA180)
#unimos barrios con población 
df <- df_barrios %>% 
  left_join(poblacion, by = "zona180")

#Creamos variables de conteo
count2004 <- rest2004 %>% 
  st_drop_geometry() %>% 
  count(zona180, name = "rest_2004")

count2012 <- rest2012 %>% 
  st_drop_geometry() %>% 
  count(zona180, name = "rest_2012")

# ---------------------------------------------------------
# 4. Unir a shapefile de barrios
# ---------------------------------------------------------

##Unimos conteo de restaurantes con la database otiginal
df <-df %>% 
  left_join(count2004, by = "zona180") %>% 
  left_join(count2012, by = "zona180") %>%  
  mutate(
    rest_2004 = replace_na(rest_2004, 0),
    rest_2012 = replace_na(rest_2012, 0)
  )

# ---------------------------------------------------------
# 5. Restaurantes per cápita
# ---------------------------------------------------------
df <- df %>%
  mutate(
    pc2004 = (rest_2004 * 1000) / day_pop,
    pc2012 = (rest_2012 * 1000) / day_pop
  )

# 2. Restar la media — CLAVE
df <- df %>%
  mutate(
    mpc2004 = pc2004 - mean(pc2004, na.rm = TRUE),
    mpc2012 = pc2012 - mean(pc2012, na.rm = TRUE)
  )

# 3. Crecimiento
df <- df %>%
  mutate(
    growth = (pc2012 - pc2004) / abs(pc2004)
  )

# 4. Cortes
breaks_pc <- c(-4,-2,0,2,4,6,8,10,12,14)
breaks_growth <- c(-0.6,-0.3,-0.2,0,0.2,0.3,0.6)

df <- df %>%
  mutate(
    cut2004 = cut(mpc2004, breaks_pc, include.lowest = TRUE),
    cut2012 = cut(mpc2012, breaks_pc, include.lowest = TRUE),
    cutgrowth = cut(growth, breaks_growth, include.lowest = TRUE)
  )

# 5. Paleta del paper
pal_rest <- c(
  "#fff7ec","#fee8c8","#fdd49e",
  "#fdbb84","#fc8d59","#ef6548",
  "#d7301f","#b30000","#7f0000"
)

pal_growth <- c(
  "#deebf7","#c6dbef","#9ecae1",
  "#6baed6","#4292c6","#2171b5"
)

# 6. Mapas en ggplot
g2004 <- ggplot(df) +
  geom_sf(aes(fill = cut2004), color = "black", size = 0.2) +
  scale_fill_manual(values = pal_rest) +
  labs(title = "Per capita number of restaurants in 2004") +
  theme_void()

g2012 <- ggplot(df) +
  geom_sf(aes(fill = cut2012), color = "black", size = 0.2) +
  scale_fill_manual(values = pal_rest) +
  labs(title = "Per capita number of restaurants in 2012") +
  theme_void()

ggrowth <- ggplot(df) +
  geom_sf(aes(fill = cutgrowth), color = "black", size = 0.2) +
  scale_fill_manual(values = pal_growth) +
  labs(title = "Percent growth in the number of per capita restaurants") +
  theme_void()

# 7. Figura final
library(cowplot)
figura1<-plot_grid(
  plot_grid(g2004, g2012, ncol = 2),
  ggrowth,
  ncol = 1
)

ggsave("outputs1/figura1.png", figura1, width=8, height=5)
# Comentario breve (guardar en txt)
cat("Figura 1 replicada: outputs/Figura1_replicacion.png\n",
    file = "outputs1/README_replica.txt", append = FALSE)
# ----------------------------
# 2) Distribución de precios (no paramétrica)
# ----------------------------
# Tomamos precios válidos
barrios      <- barrios      %>% rename(zona180 = ZONA180)
poblacion    <- poblacion    %>% rename(zona180 = zona180)
restaurants <- restaurants %>% rename(zona180 = zona180)

df2<- barrios %>%
  left_join(poblacion,    by = "zona180") %>%
  left_join(restaurants, by = "zona180")

prez2004 <- df2$prezzo2004[!is.na(df2$prezzo2004)]
prez2012 <- df2$prezzo2012[!is.na(df2$prezzo2012)]

# Rule-of-thumb bw en R para 1D: bw.nrd0
bw2004 <- bw.nrd0(prez2004)
bw2012 <- bw.nrd0(prez2012)

# 2.a) Estimaciones para dos kernels: Epanechnikov y Gaussian, usando rule-of-thumb
#creamos funcion que hace las graficas
dens_plot <- function(x, bw, kernels = c("epanechnikov","gaussian"), xlab="Precio (EUR)", main=""){
  dens_list <- lapply(kernels, function(k){
    d <- density(x, bw = bw, kernel = k, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE), n = 512)
    data.frame(x = d$x, y = d$y, kernel = k)
  })
  df_comb <- bind_rows(dens_list)
  ggplot(df_comb, aes(x=x, y=y, color = kernel)) +
    geom_line(size=1) + labs(x=xlab, y="Densidad", title = main) + theme_minimal()
}
# hacemos la realizacion para el BW encontrado
p_d2004 <- dens_plot(prez2004, bw2004, kernels=c("epanechnikov","gaussian"), main="Distribución precios 2004 (bw rule-of-thumb)")
p_d2012 <- dens_plot(prez2012, bw2012, kernels=c("epanechnikov","gaussian"), main="Distribución precios 2012 (bw rule-of-thumb)")

#Exportamos resultados
ggsave("outputs1/density_prices_2004_kernels.png", p_d2004, width=8, height=5)
ggsave("outputs1/density_prices_2012_kernels.png", p_d2012, width=8, height=5)

# 2.b) Epanechnikov con 3 anchos: rule, half, double (para cada año)
#Creamos funcion, que establezca el BW para cada serie de precios
bw_variants <- function(x){
  bw <- bw.nrd0(x)
  list(double = 2*bw, rule = bw, half = 0.5*bw)
}
#Creamos funcion, que establezca haga las estimaciones.
plot_bw_variants <- function(x, year){
  bws <- bw_variants(x)
  dens_list <- lapply(names(bws), function(nm){
    d <- density(x, bw = bws[[nm]], kernel = "epanechnikov", from = min(x), to = max(x), n=512)
    data.frame(x = d$x, y = d$y, bw = nm)
  })
  dfc <- bind_rows(dens_list)
  ggplot(dfc, aes(x=x, y=y, color=bw)) + geom_line(size=1) +
    labs(title = paste0("Epanechnikov: anchos (half, rule, double) - ", year),
         x="Precio (EUR)", y="Densidad") + theme_minimal()
}

# hacemos la realizacion
p_bw2004 <- plot_bw_variants(prez2004, 2004)
p_bw2012 <- plot_bw_variants(prez2012, 2012)

ggsave("outputs1/epanechnikov_bw_variants_2004.png", p_bw2004, width=8, height=5)
ggsave("outputs1/epanechnikov_bw_variants_2012.png", p_bw2012, width=8, height=5)

# ----------------------------
# 3) Test de Duranton & Overman (2005) para 5 barrios con mayor crecimiento restaurantes per cápita
#    Distancias 0-1 km, nsim = 999
library(sf)
library(dplyr)
library(ggplot2)

# parámetros
maxdist <- 1000         # 1 km
nsim <- 999
set.seed(12345)

# Asegurar CRS proyectado (metros) para muestreos y distance calculations
if(st_is_longlat(df)){
  df <- st_transform(df, 3857)
}
if(st_is_longlat(rest2004)) rest2004 <- st_transform(rest2004, 3857)
if(st_is_longlat(rest2012)) rest2012 <- st_transform(rest2012, 3857)

# seleccionar 5 zonas con mayor crecimiento (usar la variable 'growth' calculada anteriormente)
top_zones <- df %>%
  st_drop_geometry() %>%
  arrange(desc(growth)) %>%
  slice_head(n = 5) %>%
  pull(zona180)

message("Top 5 zonas por crecimiento (zona180): ", paste(top_zones, collapse = ", "))

# function: calcula densidad de distancias (pares) hasta maxdist, devuelve densidad estimada en grid
pairwise_dist_density <- function(coords_matrix, from = 0, to = maxdist, n = 512, bw = NULL){
  # coords_matrix: matrix n x 2 (x,y) en metros
  if(nrow(coords_matrix) < 2){
    # Devuelve vector de zeros si no hay pares
    grid <- seq(from, to, length.out = n)
    return(data.frame(x = grid, y = rep(0, length(grid))))
  }
  # todas las distancias pareja-pareja (upper triangle)
  dmat <- as.vector(dist(coords_matrix)) # Euclidean distances in meters
  dsel <- dmat[dmat <= to]  # limitar al rango
  if(length(dsel) < 2){
    grid <- seq(from, to, length.out = n)
    return(data.frame(x = grid, y = rep(0, length(grid))))
  }
  # density estimate (1D) sobre distancias
  if(is.null(bw)) bw <- bw.nrd0(dsel)
  dens <- density(dsel, bw = bw, from = from, to = to, n = n)
  data.frame(x = dens$x, y = dens$y)
}

# utilidad: area bajo curva (trapezoid)
auc_trapz <- function(x, y){
  # requiere x ordenado
  dx <- diff(x)
  midy <- (y[-1] + y[-length(y)])/2
  sum(dx * midy)
}

# Carpeta de salida para DO tests
dir.create("outputs1/do_tests", recursive = TRUE, showWarnings = FALSE)

results_list <- list()
summary_tbl <- data.frame(zona180 = character(),
                          n_restaurants = integer(),
                          auc_obs = numeric(),
                          mean_auc_sim = numeric(),
                          p_value = numeric(),
                          stringsAsFactors = FALSE)

# loop por cada zona top
for(z in top_zones){
  message("Procesando zona ", z, " ...")
  poly_z <- df %>% filter(zona180 == z)
  if(nrow(poly_z) == 0){
    warning("Zona ", z, " no encontrada en 'shp' - salto.")
    next
  }
  # restaurantes 2012 dentro de la zona (usar rest2012 para la prueba)
  rests_in_zone <- st_join(rest2012, poly_z, left = FALSE, join = st_within)
  n_points <- nrow(rests_in_zone)
  message(" - restaurantes 2012 en zona: ", n_points)
  
  if(n_points < 2){
    warning("Pocos restaurantes en zona ", z, " (n < 2). Se salta.")
    next
  }
  
  # coords observadas
  coords_obs <- st_coordinates(rests_in_zone)
  dens_obs_df <- pairwise_dist_density(coords_obs, from = 0, to = maxdist, n = 512)
  auc_obs <- auc_trapz(dens_obs_df$x, dens_obs_df$y)
  
  # simulaciones: sample uniforme dentro del polígono de la zona
  sim_densities <- matrix(0, nrow = nsim, ncol = nrow(dens_obs_df))
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  for(i in seq_len(nsim)){
    # sample n_points uniformly inside polygon
    samp_pts <- st_sample(poly_z, size = n_points, type = "random")
    # st_sample may return POINTS geometry list; convert to matrix
    samp_coords <- st_coordinates(samp_pts)
    # compute pairwise density
    dens_sim <- pairwise_dist_density(samp_coords, from = 0, to = maxdist, n = nrow(dens_obs_df))
    sim_densities[i, ] <- dens_sim$y
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # calcular envelope (2.5% - 97.5%) y mean
  env_lower <- apply(sim_densities, 2, quantile, probs = 0.025, na.rm = TRUE)
  env_upper <- apply(sim_densities, 2, quantile, probs = 0.975, na.rm = TRUE)
  env_mean  <- apply(sim_densities, 2, mean, na.rm = TRUE)
  
  # calcular AUC de simulaciones -> p-valor aproximado: proporción sims con AUC >= auc_obs
  auc_sims <- apply(sim_densities, 1, function(y) auc_trapz(dens_obs_df$x, y))
  pval <- mean(auc_sims >= auc_obs)
  
  # guardar resultados en tabla
  summary_tbl <- rbind(summary_tbl,
                       data.frame(zona180 = z,
                                  n_restaurants = n_points,
                                  auc_obs = auc_obs,
                                  mean_auc_sim = mean(auc_sims, na.rm = TRUE),
                                  p_value = pval,
                                  stringsAsFactors = FALSE))
  
  # plot: densidad observada + envelope
  plot_df <- data.frame(x = dens_obs_df$x,
                        obs = dens_obs_df$y,
                        lower = env_lower,
                        upper = env_upper,
                        mean_sim = env_mean)
  
  p <- ggplot(plot_df, aes(x = x)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.6) +
    geom_line(aes(y = mean_sim), linetype = "dashed", size = 0.8, color = "darkgrey") +
    geom_line(aes(y = obs), size = 1.2, color = "red") +
    labs(title = paste0("Duranton & Overman style test - Zona ", z),
         subtitle = paste0("n=", n_points, " | AUC_obs=", round(auc_obs,5), " | p=", signif(pval,3)),
         x = "Distance (m)", y = "Density of pairwise distances") +
    theme_minimal()
  
  ggsave(filename = paste0("outputs1/do_tests/DO_test_zone_", z, ".png"),
         plot = p, width = 8, height = 5, dpi = 300)
  
  results_list[[as.character(z)]] <- list(
    dens_obs = dens_obs_df,
    env_lower = env_lower,
    env_upper = env_upper,
    mean_sim = env_mean,
    auc_obs = auc_obs,
    auc_sims = auc_sims,
    p_value = pval,
    plot = p
  )
  
  message(" -> zona ", z, ": p-value (AUC sims >= obs) = ", signif(pval,3))
}

# guardar resumen
write.csv(summary_tbl, "outputs1/do_tests_summary.csv", row.names = FALSE)

# mostrar tabla resumen en pantalla
print(summary_tbl)

# combinar las figuras en un pdf (opcional)
png("outputs1/DO_tests_combined.png", width = 12*300, height = 8*300, res = 300)
grid.arrange(grobs = lapply(results_list, function(x) x$plot), ncol = 2)
dev.off()

# ----------------------------
# Fin del script
# ----------------------------
cat("En la carpeta 'outputs1/' esta la figura y graficas \n")
