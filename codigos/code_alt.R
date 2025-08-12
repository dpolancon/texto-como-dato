
# 4) Conteos básicos -----------------------------------------------------------
# 4.1 Tesis por año
theses_by_year <- df %>%
  filter(!is.na(year), dplyr::between(year, 1900, 2100)) %>%
  count(year, name = "count")

readr::write_csv(theses_by_year, file.path(tab_dir, "theses_by_year.csv"))

ggplot(theses_by_year, aes(year, count)) +
  geom_col(fill = "grey20") +
  labs(title = "Tesis por año", x = "Año", y = "Cantidad")
ggsave(filename = file.path(fig_dir, "theses_by_year.png"),
       width = 7, height = 4.2, dpi = 200)

# 4.2 Longitud de abstracts (n° de palabras)
abstract_lengths <- df %>%
  mutate(abstract_len = ifelse(abstract == "", 0L, stringr::str_count(abstract, "\\S+"))) %>%
  select(n, name, year, abstract_len)

readr::write_csv(abstract_lengths, file.path(tab_dir, "abstract_lengths.csv"))

# 5) Advisors: tabla larga, top y gráfico -------------------------------------
adv_long <- df %>%
  pivot_longer(starts_with("adv"), names_to = "adv_role", values_to = "advisor") %>%
  filter(advisor != "") %>%
  mutate(advisor = stringr::str_squish(advisor)) %>%
  distinct(n, name, advisor)   # evita duplicados triviales

top_advisors <- adv_long %>%
  count(advisor, sort = TRUE, name = "count") %>%
  slice_head(n = 12)

readr::write_csv(top_advisors, file.path(tab_dir, "top_advisors.csv"))

ggplot(top_advisors, aes(x = reorder(advisor, count), y = count)) +
  geom_col(fill = "grey20") +
  coord_flip() +
  labs(title = "Principales advisors (frecuencia en adv1–adv5)", x = NULL, y = "Conteo")
ggsave(filename = file.path(fig_dir, "top_advisors.png"),
       width = 7, height = 4.5, dpi = 200)

# 6) Red bipartita alumno–advisor y co-asesorías ------------------------------
# 6.1 Aristas bipartitas alumno–advisor
edges_bip <- adv_long %>% select(name, advisor)
readr::write_csv(edges_bip, file.path(tab_dir, "edges_bipartite.csv"))

# 6.2 Pares de co-asesorías (cómputo agnóstico a igraph)
pairs <- edges_bip %>%
  group_by(name) %>%
  summarise(advisors = sort(unique(advisor)), .groups = "drop") %>%
  mutate(pairs = purrr::map(advisors, ~ {
    a <- .
    if (length(a) < 2) return(tibble(advisor_u = character(), advisor_v = character()))
    comb <- t(combn(a, 2))
    tibble(advisor_u = comb[,1], advisor_v = comb[,2])
  })) %>%
  select(pairs) %>% unnest(pairs) %>%
  count(advisor_u, advisor_v, sort = TRUE, name = "weight")

readr::write_csv(pairs, file.path(tab_dir, "advisor_pairs.csv"))

# 6.3 Grado de la red de co-asesorías (sin/ con igraph)
if (nrow(pairs) > 0) {
  # grado sin igraph (número de vecinos únicos)
  deg_manual <- bind_rows(
    pairs %>% transmute(advisor = advisor_u, neigh = advisor_v),
    pairs %>% transmute(advisor = advisor_v, neigh = advisor_u)
  ) %>%
    distinct(advisor, neigh) %>%
    count(advisor, name = "degree") %>%
    arrange(desc(degree))
  readr::write_csv(deg_manual, file.path(tab_dir, "advisor_degree.csv"))
  
  # gráfico con igraph + ggraph si están disponibles
  plot_done <- FALSE
  try({
    g <- graph_from_data_frame(pairs, directed = FALSE)
    set.seed(42)
    p <- ggraph(g, layout = "fr") +
      geom_edge_link(alpha = .4) +
      geom_node_point(size = 3) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_void() +
      ggtitle("Red de co-asesorías (proyección de advisors)")
    ggsave(file.path(fig_dir, "advisor_network.png"), plot = p,
           width = 7.5, height = 6, dpi = 200)
    plot_done <- TRUE
  }, silent = TRUE)
  
  if (!plot_done) {
    message("No se generó 'advisor_network.png' (falta ggraph/igraph o layout falló).")
  }
}

# 7) TF–IDF de abstracts por advisor principal (adv1) -------------------------
sw <- unique(c(stopwords::stopwords("es"), stopwords::stopwords("en")))

tokens <- df %>%
  mutate(group = dplyr::if_else(adv1 == "", "sin_adv1", adv1),
         abstract = tolower(abstract)) %>%
  select(group, abstract) %>%
  tidytext::unnest_tokens(word, abstract) %>%
  filter(stringr::str_detect(word, "^[a-záéíóúñ]+$")) %>%
  filter(!word %in% sw)

tfidf_by_adv1 <- tokens %>%
  count(group, word, name = "n") %>%
  bind_tf_idf(word, group, n) %>%
  group_by(group) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

readr::write_csv(tfidf_by_adv1, file.path(tab_dir, "tfidf_by_adv1.csv"))

# 8) Exportable a LaTeX (ejemplo: top advisors) -------------------------------
xa <- xtable::xtable(top_advisors,
                     caption = "Principales advisors (frecuencia total).",
                     label   = "tab:top-advisors")
print(xa, file = file.path(tab_dir, "top_advisors.tex"), include.rownames = FALSE)

# 9) Mensaje final -------------------------------------------------------------
cat("\nListo. Artefactos escritos en:\n", normalizePath(out_dir), "\n",
    " - figures/: theses_by_year.png, top_advisors.png, advisor_network.png (si aplica)\n",
    " - tables/ : theses_by_year.csv, abstract_lengths.csv, top_advisors.csv,\n",
    "             edges_bipartite.csv, advisor_pairs.csv, advisor_degree.csv,\n",
    "             tfidf_by_adv1.csv, top_advisors.tex\n", sep = "")
