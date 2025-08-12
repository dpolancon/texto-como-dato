# --- Minimal deps ---
pkgs <- c("tidyverse","tidytext","stopwords","wordcloud","RColorBrewer")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- Paths ---
BASE <- "C:/textdata/post"
DATA <- file.path(BASE, "datos", "df_bp3.RData")  # NOTE: no leading slash here
FIGS <- file.path(BASE, "figuras")
TABS <- file.path(BASE, "tablas")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
dir.create(TABS, recursive = TRUE, showWarnings = FALSE)

# --- Load data ---
load(DATA)  # should define df_bp3 (or df)
d <- if (exists("df")) df else if (exists("df_bp3")) df_bp3 else stop("No df / df_bp3 found.")

# --- Clean base & keep your original barplot logic (adv1) ---
d <- d %>%
  mutate(year = suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year), year >= 2009) %>%
  mutate(across(starts_with("adv"), ~ str_squish(tolower(.x))))

# Barplot: Top 30 main advisors (same as your code)
top30 <- d %>%
  filter(!is.na(adv1), adv1 != "", adv1 != "na") %>%
  count(adv1, name = "theses") %>%
  arrange(desc(theses)) %>%
  slice_max(theses, n = 30)

p <- ggplot(top30, aes(x = reorder(adv1, theses), y = theses)) +
  geom_col(fill = "gray30") +
  coord_flip() +
  labs(title = "Top 30 Main Advisors (2009+)", x = NULL, y = "Theses")
ggsave(file.path(FIGS, "top_main_advisors.png"), p, width = 8, height = 8, dpi = 150)

# --- Unified word cloud (unigrams + bigrams) for Boyce, ANY advisor role ---
# Custom stopwords (American English + extras)
extra_sw <- c("dissertation","chapter","also","can","cfm","csa")
custom_sw <- unique(c(stopwords::stopwords("en"), extra_sw))

make_wordcloud <- function(advisor_pattern = "boyce",
                           roles = 1:5,
                           min_freq = 2,
                           max_words = 150,
                           out_prefix = "boyce_uni_bi") {
  role_cols <- paste0("adv", roles)
  
  # Subset: advisor appears in ANY role
  sub <- d %>%
    filter(if_any(all_of(role_cols), ~ str_detect(.x, advisor_pattern))) %>%
    filter(!is.na(abstract), abstract != "") %>%
    transmute(abstract = tolower(abstract))
  
  if (nrow(sub) == 0) {
    message("No theses matched pattern: ", advisor_pattern)
    return(invisible(NULL))
  }
  
  # UNIGRAMS (letters only, >=2 chars) minus stopwords
  uni <- sub %>%
    unnest_tokens(word, abstract, token = "words") %>%
    filter(str_detect(word, "^[a-z][a-z]+$")) %>%
    filter(!word %in% custom_sw) %>%
    count(word, name = "n")
  
  # BIGRAMS with both words cleaned & not in stopwords
  bi <- sub %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("w1","w2"), sep = " ") %>%
    filter(str_detect(w1, "^[a-z][a-z]+$"),
           str_detect(w2, "^[a-z][a-z]+$")) %>%
    filter(!w1 %in% custom_sw, !w2 %in% custom_sw) %>%
    unite(word, w1, w2, sep = " ") %>%
    count(word, name = "n")
  
  # Combine & sum
  tokens <- bind_rows(uni, bi) %>%
    group_by(word) %>% summarise(n = sum(n), .groups = "drop") %>%
    arrange(desc(n)) %>%
    filter(n >= min_freq)
  
  # Save CSV (for transparency)
  readr::write_csv(tokens, file.path(TABS, paste0("tokens_", out_prefix, ".csv")))
  
  # Word cloud
  set.seed(123)
  png(file.path(FIGS, paste0("wordcloud_", out_prefix, ".png")),
      width = 1100, height = 800, res = 150)
  wordcloud(words = tokens$word, freq = tokens$n,
            max.words = max_words, random.order = FALSE,
            colors = RColorBrewer::brewer.pal(8, "Dark2"))
  dev.off()
  
  invisible(tokens)
}

# Build Boyce (any role)
make_wordcloud(advisor_pattern = "boyce",
               roles = 1:5,
               min_freq = 2,
               max_words = 150,
               out_prefix = "boyce_unigrams_bigrams")

cat("\nDone.\n- Barplot: ", file.path(FIGS, "top_main_advisors.png"),
    "\n- Wordcloud: ", file.path(FIGS, "wordcloud_boyce_unigrams_bigrams.png"),
    "\n- Tokens CSV: ", file.path(TABS, "tokens_boyce_unigrams_bigrams.csv"), "\n", sep = "")
