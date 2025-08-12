## =============================================================
## 01_pipeline.R — UMass Theses (base para múltiples análisis)
## Estructura de salida:
##   C:/textdata/post/figuras/
##   C:/textdata/post/tablas/
## Requiere columnas:
##   n, name, title, link, year, bp_categories, abstract,
##   adv1, adv2, adv3, adv4, adv5, pdfhtml, rcit
## =============================================================

## 0) Rutas -----------------------------------------------------
BASE <- "C:/textdata/post"
DATA_RDATA <- file.path(BASE, "df_bp3.RData")
DIR_FIG <- file.path(BASE, "figuras")
DIR_TAB <- file.path(BASE, "tablas")

dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_TAB, showWarnings = FALSE, recursive = TRUE)

## 1) Paquetes --------------------------------------------------
packs <- c("tidyverse","tidytext","stopwords","igraph","ggraph")
to_install <- setdiff(packs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packs, library, character.only = TRUE))
theme_set(theme_minimal(base_size = 12))
options(stringsAsFactors = FALSE)

## 2) Carga de datos -------------------------------------------
if (file.exists(DATA_RDATA)) load(DATA_RDATA)

required_cols <- c("n","name","title","link","year","bp_categories",
                   "abstract","adv1","adv2","adv3","adv4","adv5","pdfhtml","rcit")

# Detecta df de forma robusta
if (exists("df") && is.data.frame(df) && all(required_cols %in% names(df))) {
  dat <- df
} else if (exists("df_bp3") && is.data.frame(df_bp3) && all(required_cols %in% names(df_bp3))) {
  dat <- df_bp3
} else {
  objs <- mget(ls(), inherits = TRUE)
  cands <- Filter(function(x) is.data.frame(x) && all(required_cols %in% names(x)), objs)
  if (length(cands) == 0L) stop("No encontré un data.frame con las columnas requeridas.")
  dat <- cands[[1]]
}

## 3) Limpieza mínima + cobertura 2009+ ------------------------
normalize_txt <- function(x){
  x %>% tidyr::replace_na("") %>% as.character() %>% stringr::str_squish()
}
dat <- dat %>%
  mutate(across(everything(), ~ if(is.character(.x) || is.factor(.x)) normalize_txt(.x) else .x),
         year = suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year), year >= 2009)

## 4) Normalización de nombres de advisors ---------------------
canonize_advisor <- function(x) {
  x %>% tolower() %>%
    stringr::str_replace_all("[.]", "") %>%     # quita puntos de iniciales
    stringr::str_replace_all("\\b[a-z]\\b", "") %>%
    stringr::str_squish()
}
titlecase <- function(x) stringr::str_to_title(x)

adv_long_raw <- dat %>%
  pivot_longer(starts_with("adv"), names_to = "adv_role", values_to = "advisor") %>%
  filter(advisor != "") %>%
  mutate(advisor_canon = canonize_advisor(advisor))

display_map <- adv_long_raw %>%
  count(advisor_canon, advisor, sort = TRUE) %>%
  group_by(advisor_canon) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% transmute(advisor_canon, advisor_display = titlecase(advisor))

adv_long <- adv_long_raw %>%
  select(n, year, adv_role, advisor_canon) %>% distinct() %>%
  left_join(display_map, by = "advisor_canon")

# Guarda alias detectados (útil para corrección manual)
write_csv(
  adv_long_raw %>% select(advisor_raw = advisor, advisor_canon) %>% distinct() %>%
    left_join(display_map, by = "advisor_canon"),
  file.path(DIR_TAB, "advisor_aliases_detected.csv")
)

## === ACTO 1: HISTOGRAMAS DE FRECUENCIAS POR ADVISOR (2009+) =================

# A) TODAS LAS POSICIONES (adv1..adv5)
freq_all <- adv_long %>%
  count(advisor_display, name = "tesis") %>%
  arrange(desc(tesis))

write_csv(freq_all, file.path(DIR_TAB, "frecuencias_advisors_todas_2009on.csv"))

# Gráfico: top 30 (evita “pared” de nombres)
top_k <- 30
p_all <- freq_all %>%
  slice_max(tesis, n = top_k) %>%
  ggplot(aes(x = reorder(advisor_display, tesis), y = tesis)) +
  geom_col(fill = "grey20") +
  coord_flip() +
  labs(title = "Frecuencia de advisors (2009–presente, todas las posiciones)",
       x = NULL, y = "Tesis asesoradas")
ggsave(file.path(DIR_FIG, "hist_advisors_todas_top30.png"), p_all, width = 8, height = 9, dpi = 200)

# B) SOLO MAIN ADVISOR (adv1)
freq_main <- dat %>%
  filter(adv1 != "") %>%
  mutate(advisor_canon = canonize_advisor(adv1)) %>%
  left_join(display_map, by = "advisor_canon") %>%
  count(advisor_display, name = "tesis_main") %>%
  arrange(desc(tesis_main))

write_csv(freq_main, file.path(DIR_TAB, "frecuencias_advisors_main_2009on.csv"))

p_main <- freq_main %>%
  slice_max(tesis_main, n = top_k) %>%
  ggplot(aes(x = reorder(advisor_display, tesis_main), y = tesis_main)) +
  geom_col(fill = "grey20") +
  coord_flip() +
  labs(title = "Frecuencia de MAIN advisors (2009–presente, adv1)",
       x = NULL, y = "Tesis (adv1)")
ggsave(file.path(DIR_FIG, "hist_advisors_main_top30.png"), p_main, width = 8, height = 9, dpi = 200)

## === ACTO 2: Núcleo de outputs para la “historia UMass” =====================

# Cobertura (tesis por año)
theses_by_year <- dat %>% distinct(n, year) %>% count(year, name = "theses_total")
write_csv(theses_by_year, file.path(DIR_TAB, "tesis_por_anio_2009on.csv"))
ggsave(file.path(DIR_FIG, "tesis_por_anio_2009on.png"),
       ggplot(theses_by_year, aes(year, theses_total)) +
         geom_col(fill = "grey20") +
         labs(title = "Tesis por año (2009–presente)", x = "Año", y = "Tesis"),
       width = 7, height = 4.2, dpi = 200)

# % sin main advisor
pct_missing <- dat %>%
  mutate(miss_adv1 = adv1 == "" | is.na(adv1)) %>%
  group_by(year) %>% summarise(pct_missing_adv1 = mean(miss_adv1)*100, .groups = "drop")
write_csv(pct_missing, file.path(DIR_TAB, "pct_sin_main_por_anio.csv"))
ggsave(file.path(DIR_FIG, "pct_sin_main_por_anio.png"),
       ggplot(pct_missing, aes(year, pct_missing_adv1)) +
         geom_line() + geom_point() +
         labs(title = "% de tesis sin main advisor", x = "Año", y = "%"),
       width = 7, height = 3.8, dpi = 200)

# Tamaño de comité (n_advisors por tesis) y composición anual
n_advisors_by_thesis <- adv_long %>% group_by(n) %>%
  summarise(n_advisors = n_distinct(advisor_canon), .groups = "drop")
dat2 <- dat %>% left_join(n_advisors_by_thesis, by = "n") %>%
  mutate(n_advisors = tidyr::replace_na(n_advisors, 0L))

comp <- dat2 %>%
  group_by(year, n_advisors) %>% summarise(n = n(), .groups="drop") %>%
  group_by(year) %>% mutate(prop = n/sum(n)) %>% ungroup()
write_csv(comp, file.path(DIR_TAB, "comite_composicion_anual.csv"))
ggsave(file.path(DIR_FIG, "comite_composicion_anual.png"),
       ggplot(comp, aes(x = year, y = prop, fill = factor(n_advisors))) +
         geom_col() + scale_fill_grey(start = 0.85, end = 0.2, name = "N° advisors") +
         labs(title = "Composición anual por tamaño de comité", x = "Año", y = "Proporción"),
       width = 8, height = 4.6, dpi = 200)

# Heatmap year × advisor (todas las posiciones, top 12)
advisor_by_year_all <- adv_long %>%
  count(year, advisor_display, name = "count") %>% arrange(year, desc(count))
write_csv(advisor_by_year_all, file.path(DIR_TAB, "year_por_advisor_todos.csv"))

top12_all <- advisor_by_year_all %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups = "drop") %>%
  slice_max(total, n = 12)

plot_df_all <- advisor_by_year_all %>%
  semi_join(top12_all, by = "advisor_display") %>%
  tidyr::complete(advisor_display, year, fill = list(count = 0)) %>%
  left_join(top12_all, by = "advisor_display")

ggsave(file.path(DIR_FIG, "heatmap_year_advisor_top12.png"),
       ggplot(plot_df_all, aes(x = year,
                               y = forcats::fct_reorder(advisor_display, total),
                               fill = count)) +
         geom_tile(color = "white") +
         scale_fill_gradient(name = "Tesis", low = "grey90", high = "grey10") +
         labs(title = "Tesis por año y advisor (todas las posiciones, top 12)",
              x = "Año", y = NULL),
       width = 8, height = 5.2, dpi = 200)

# Facetas: main advisors top 12
main_by_year <- dat %>%
  filter(adv1 != "") %>%
  mutate(advisor_canon = canonize_advisor(adv1)) %>%
  left_join(display_map, by = "advisor_canon") %>%
  count(year, advisor_display, name = "count")
write_csv(main_by_year, file.path(DIR_TAB, "year_por_main_advisor.csv"))

top12_main <- main_by_year %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups = "drop") %>%
  slice_max(total, n = 12)

ggsave(file.path(DIR_FIG, "facetas_main_advisors_top12.png"),
       ggplot(main_by_year %>% semi_join(top12_main, by = "advisor_display"),
              aes(x = year, y = count)) +
         geom_col(fill = "grey20") +
         facet_wrap(~ advisor_display, scales = "free_y") +
         labs(title = "Main advisors: tesis por año (facetas, top 12)",
              x = "Año", y = "Tesis"),
       width = 10, height = 7.5, dpi = 200)

# Ranking dinámico (top 8)
ranks <- main_by_year %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups="drop") %>%
  slice_max(total, n = 8) %>% pull(advisor_display)

rank_df <- main_by_year %>%
  filter(advisor_display %in% ranks) %>%
  group_by(year) %>% arrange(year, desc(count)) %>% mutate(rank = row_number()) %>% ungroup()

ggsave(file.path(DIR_FIG, "ranking_main_advisors_top8.png"),
       ggplot(rank_df, aes(x = year, y = rank, group = advisor_display)) +
         geom_line() + geom_point() +
         scale_y_reverse(breaks = 1:8) +
         labs(title = "Ranking anual de main advisors (top 8)",
              x = "Año", y = "Posición (1=top)") +
         theme(panel.grid.minor = element_blank()),
       width = 8.5, height = 5, dpi = 200)

## 5) Mensaje final ------------------------------------------------------------
cat("\nListo. Salidas en:\n",
    "- Figuras: ", normalizePath(DIR_FIG), "\n",
    "- Tablas : ", normalizePath(DIR_TAB), "\n", sep = "")
