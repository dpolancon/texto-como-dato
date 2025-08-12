## =============================================================
## UMass theses — Text-as-Data & Redes (pipeline R integrado)
## Salidas: C:/textdata/post/output/{figures,tables}
## Requiere columnas en 'df':
## n, name, title, link, year, bp_categories, abstract,
## adv1, adv2, adv3, adv4, adv5, pdfhtml, rcit
## =============================================================

## 0) Carga de datos ------------------------------------------------------------
load("C:/textdata/post/df_bp3.RData")  # <- tu objeto vive aquí

# Detecta el data.frame con las columnas requeridas y lo asigna a df
required_cols <- c("n","name","title","link","year","bp_categories",
                   "abstract","adv1","adv2","adv3","adv4","adv5","pdfhtml","rcit")

if (!exists("df")) {
  objs <- mget(ls(), inherits = TRUE)
  candidates <- Filter(function(x) is.data.frame(x) && all(required_cols %in% names(x)), objs)
  if (length(candidates) == 0L) stop("No encontré un data.frame con las columnas requeridas.")
  df <- candidates[[1]]
}

## 1) Paquetes y configuración --------------------------------------------------
packs <- c("tidyverse","tidytext","stopwords","igraph","ggraph","xtable")
to_install <- setdiff(packs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packs, library, character.only = TRUE))
theme_set(theme_minimal(base_size = 12))

## 2) Carpetas de salida --------------------------------------------------------
out_dir <- "C:/textdata/post/output"
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

## 3) Limpieza mínima -----------------------------------------------------------
normalize_txt <- function(x){
  x %>% tidyr::replace_na("") %>% as.character() %>% stringr::str_squish()
}
df <- df %>%
  mutate(across(everything(),
                ~ if (is.character(.x) || is.factor(.x)) normalize_txt(.x) else .x)) %>%
  mutate(year = suppressWarnings(as.integer(year)))

## 4) Corte de cobertura (UMass repositorio: >= 2009) --------------------------
cut_year <- 2009L
df <- df %>% filter(!is.na(year), year >= cut_year)

## 5) Normalización de nombres de advisors -------------------------------------
canonize_advisor <- function(x) {
  x %>%
    tolower() %>%
    stringr::str_replace_all("[.]", "") %>%        # quita puntos/iniciales
    stringr::str_replace_all("\\b[a-z]\\b", "") %>%
    stringr::str_squish()
}
titlecase <- function(x) stringr::str_to_title(x)

# Long raw: (n, year, adv_role, advisor)
adv_long_raw <- df %>%
  tidyr::pivot_longer(starts_with("adv"), names_to = "adv_role", values_to = "advisor") %>%
  filter(advisor != "") %>%
  mutate(advisor_canon = canonize_advisor(advisor))

# Mapa de alias → display (variante más frecuente con capitalización limpia)
display_map <- adv_long_raw %>%
  count(advisor_canon, advisor, sort = TRUE) %>%
  group_by(advisor_canon) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(advisor_canon, advisor_display = titlecase(advisor))

# Permitir sobre-escrituras manuales si existe archivo (edítalo cuando quieras)
override_path <- file.path(tab_dir, "advisor_alias_overrides.csv")
if (file.exists(override_path)) {
  overrides <- readr::read_csv(override_path, show_col_types = FALSE)
  # columnas esperadas: advisor_canon, advisor_display
  display_map <- display_map %>%
    rows_update(overrides, by = "advisor_canon")
}

# adv_long normalizado
adv_long <- adv_long_raw %>%
  select(n, year, adv_role, advisor_canon) %>%
  distinct() %>% left_join(display_map, by = "advisor_canon")

# Guarda alias para revisión/edición
alias_export <- adv_long_raw %>%
  select(advisor_raw = advisor, advisor_canon) %>% distinct() %>%
  left_join(display_map, by = "advisor_canon")
readr::write_csv(alias_export, file.path(tab_dir, "advisor_aliases_detected.csv"))

## 6) Historia 1 — Cobertura y calidad del metadato ---------------------------
# Tesis por año (únicas)
theses_by_year <- df %>% distinct(n, year) %>% count(year, name = "theses_total")
readr::write_csv(theses_by_year, file.path(tab_dir, "theses_by_year_2009on.csv"))
ggplot(theses_by_year, aes(year, theses_total)) +
  geom_col(fill = "grey20") +
  labs(title = "Tesis por año (UMass Amherst, ≥ 2009)", x = "Año", y = "Tesis")
ggsave(file.path(fig_dir, "theses_by_year_2009on.png"), width = 7, height = 4.2, dpi = 200)

# % sin main advisor registrado (adv1 vacío)
missing_adv1 <- df %>%
  mutate(miss_adv1 = adv1 == "" | is.na(adv1)) %>%
  group_by(year) %>% summarise(pct_missing_adv1 = mean(miss_adv1)*100, .groups="drop")
readr::write_csv(missing_adv1, file.path(tab_dir, "pct_missing_adv1_by_year.csv"))
ggplot(missing_adv1, aes(year, pct_missing_adv1)) +
  geom_line() + geom_point() +
  labs(title = "% de tesis sin main advisor registrado (≥ 2009)", x = "Año", y = "% sin adv1")
ggsave(file.path(fig_dir, "pct_missing_adv1_by_year.png"), width = 7, height = 3.8, dpi = 200)

## 7) Historia 2 — De la tutoría individual a la co-asesoría -------------------
# n_advisors por tesis
n_advisors_by_thesis <- adv_long %>% group_by(n) %>%
  summarise(n_advisors = n_distinct(advisor_canon), .groups = "drop")
df <- df %>% left_join(n_advisors_by_thesis, by = "n") %>%
  mutate(n_advisors = tidyr::replace_na(n_advisors, 0L))

# Distribución por año (stack de proporciones)
multi_by_year <- df %>%
  group_by(year, n_advisors) %>% summarise(n = n(), .groups="drop") %>%
  group_by(year) %>% mutate(prop = n/sum(n)) %>% ungroup()
readr::write_csv(multi_by_year, file.path(tab_dir, "n_advisors_dist_by_year.csv"))
ggplot(multi_by_year, aes(x = year, y = prop, fill = factor(n_advisors))) +
  geom_col() + scale_fill_grey(start = 0.85, end = 0.2, name = "N° advisors") +
  labs(title = "¿Cuántos advisors por tesis? (composición anual, ≥ 2009)",
       x = "Año", y = "Proporción")
ggsave(file.path(fig_dir, "n_advisors_stack_by_year.png"), width = 8, height = 4.6, dpi = 200)

## 8) Historia 3 — “Quiénes aconsejan”: year × advisor -------------------------
# (a) Todas las posiciones adv1..adv5, top 12 por total
advisor_by_year_all <- adv_long %>%
  count(year, advisor_display, name = "count") %>%
  arrange(year, desc(count))
readr::write_csv(advisor_by_year_all, file.path(tab_dir, "advisor_by_year_all_2009on.csv"))

top_k_all <- 12
top12_all <- advisor_by_year_all %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups="drop") %>%
  slice_max(total, n = top_k_all)

plot_df_all <- advisor_by_year_all %>%
  semi_join(top12_all, by = "advisor_display") %>%
  tidyr::complete(advisor_display, year, fill = list(count = 0)) %>%
  left_join(top12_all, by = "advisor_display")

ggplot(plot_df_all,
       aes(x = year,
           y = forcats::fct_reorder(advisor_display, total),
           fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "Tesis", low = "grey90", high = "grey10") +
  labs(title = "Tesis por año y advisor (todas las posiciones, top 12, ≥ 2009)",
       x = "Año", y = NULL)
ggsave(file.path(fig_dir, "advisor_by_year_heatmap_top12_2009on.png"),
       width = 8, height = 5.2, dpi = 200)

# (b) Solo main advisor (adv1) limpio
main_adv_by_year <- df %>%
  filter(adv1 != "") %>%
  mutate(advisor_canon = canonize_advisor(adv1)) %>%
  left_join(display_map, by = "advisor_canon") %>%
  count(year, advisor_display, name = "count")
readr::write_csv(main_adv_by_year, file.path(tab_dir, "main_advisor_by_year_2009on.csv"))

top_k_main <- 12
top12_main <- main_adv_by_year %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups="drop") %>%
  slice_max(total, n = top_k_main)

plot_main <- main_adv_by_year %>% semi_join(top12_main, by = "advisor_display")

ggplot(plot_main, aes(x = year, y = count)) +
  geom_col(fill = "grey20") +
  facet_wrap(~ advisor_display, scales = "free_y") +
  labs(title = "Main advisors (adv1): tesis por año — top 12 (≥ 2009)",
       x = "Año", y = "Cantidad")
ggsave(file.path(fig_dir, "main_advisor_by_year_top12_facets_2009on.png"),
       width = 10, height = 7.5, dpi = 200)

## 9) Historia 4 — Ranking dinámico de main advisors (líneas de posición) -----
# Rankeos por año para top 8 en el total del período
ranks <- main_adv_by_year %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups="drop") %>%
  slice_max(total, n = 8) %>% pull(advisor_display)

rank_df <- main_adv_by_year %>%
  filter(advisor_display %in% ranks) %>%
  group_by(year) %>%
  arrange(year, desc(count)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

ggplot(rank_df, aes(x = year, y = rank, group = advisor_display)) +
  geom_line() + geom_point() +
  scale_y_reverse(breaks = 1:8) +
  labs(title = "Ranking anual de main advisors (top 8, ≥ 2009)",
       x = "Año", y = "Posición (1 = top)") +
  theme(panel.grid.minor = element_blank())
ggsave(file.path(fig_dir, "main_advisors_rank_lines_top8.png"),
       width = 8.5, height = 5, dpi = 200)

## 10) Red de co-asesorías (opcional pero útil para el relato) -----------------
# edges bipartita y pares (co-asesorías)
edges_bip <- adv_long %>% select(name, advisor = advisor_display) %>% distinct()
readr::write_csv(edges_bip, file.path(tab_dir, "edges_bipartite_student_advisor.csv"))

pairs <- edges_bip %>%
  group_by(name) %>% summarise(advisors = sort(unique(advisor)), .groups="drop") %>%
  mutate(pairs = purrr::map(advisors, ~ {
    a <- .
    if (length(a) < 2) return(tibble(advisor_u = character(), advisor_v = character()))
    comb <- t(combn(a, 2))
    tibble(advisor_u = comb[,1], advisor_v = comb[,2])
  })) %>%
  select(pairs) %>% unnest(pairs) %>%
  count(advisor_u, advisor_v, sort = TRUE, name = "weight")
readr::write_csv(pairs, file.path(tab_dir, "advisor_pairs_weights.csv"))

# grado (n° de co-asesores distintos)
deg_manual <- bind_rows(
  pairs %>% transmute(advisor = advisor_u, neigh = advisor_v),
  pairs %>% transmute(advisor = advisor_v, neigh = advisor_u)
) %>% distinct(advisor, neigh) %>% count(advisor, name = "degree") %>% arrange(desc(degree))
readr::write_csv(deg_manual, file.path(tab_dir, "advisor_degree.csv"))

# gráfico simple si hay librerías
plot_done <- FALSE
try({
  g <- igraph::graph_from_data_frame(pairs, directed = FALSE)
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(alpha = .4) +
    geom_node_point(size = 3) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    theme_void() + ggtitle("Red de co-asesorías (UMass, ≥ 2009)")
  ggsave(file.path(fig_dir, "advisor_network.png"), plot = p, width = 8, height = 6, dpi = 200)
  plot_done <- TRUE
}, silent = TRUE)
if (!plot_done) message("No se generó 'advisor_network.png' (falta ggraph/igraph o layout falló).")

## 11) Export a LaTeX ejemplo (tabla top main advisors total) ------------------
top_main_total <- main_adv_by_year %>%
  group_by(advisor_display) %>% summarise(total = sum(count), .groups="drop") %>%
  arrange(desc(total))
print(xtable::xtable(top_main_total, caption = "Top main advisors (≥ 2009).",
                     label = "tab:top-main"), file = file.path(tab_dir, "top_main_total.tex"),
      include.rownames = FALSE)

## 12) Epílogo — Mensaje final -------------------------------------------------
cat("\nListo. Artefactos escritos en:\n", normalizePath(out_dir), "\n",
    " - figures/: theses_by_year_2009on.png, pct_missing_adv1_by_year.png,\n",
    "             n_advisors_stack_by_year.png, advisor_by_year_heatmap_top12_2009on.png,\n",
    "             main_advisor_by_year_top12_facets_2009on.png, main_advisors_rank_lines_top8.png,\n",
    "             advisor_network.png (si aplica)\n",
    " - tables/ : theses_by_year_2009on.csv, pct_missing_adv1_by_year.csv,\n",
    "             n_advisors_dist_by_year.csv, advisor_aliases_detected.csv,\n",
    "             advisor_by_year_all_2009on.csv, main_advisor_by_year_2009on.csv,\n",
    "             edges_bipartite_student_advisor.csv, advisor_pairs_weights.csv,\n",
    "             advisor_degree.csv, top_main_total.tex\n", sep = "")
