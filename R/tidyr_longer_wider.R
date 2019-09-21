source(here::here("R", "00_base_tidyr.R"))
# Data ----
set.seed(42)
wide <- tibble(
  id = rep(1:2),
  x = letters[1:2],
  y = letters[3:4],
  z = letters[5:6]
)

# generate long using `pivot_longer()` rather than `spread`
long <- tidyr::pivot_longer(wide, -id)


## new stuff
tidyr::pivot_longer(wide, -id)

tidyr::pivot_wider(long, names_from = name, values_from = value)

sgwide <- wide %>%
  proc_data("0-wide", colorize_wide_tidyr) %>%
  mutate(frame = 1, .id = "0-wide")

sglong <- wide %>%
  pivot_longer(-id) %>%
  proc_data("3-tall", color_fun = function(x, y) x) %>%
  split(.$label)

sglong$id <-
  sgwide %>%
  filter(label == "id") %>%
  select(value, color) %>%
  left_join(sglong$id, ., by = "value") %>%
  mutate(alpha = 1)

sglong$name <-
  sgwide %>%
  filter(label != "id") %>%
  select(label, color) %>%
  left_join(sglong$name, ., by = c("value" = "label")) %>%
  distinct() %>%
  mutate(alpha = 1)

sglong$val <-
  sgwide %>%
  filter(label != "id", .y < 0) %>%
  select(value, color) %>%
  left_join(sglong$val, ., by = "value") %>%
  mutate(alpha = 0.6)

sglong <- bind_rows(sglong) %>% mutate(frame = 2)

# above checked -----------------------------------------------------------
sglong_labels <- tibble(id = 1, a = "id", x = "name", y = "val") %>%
  proc_data("4-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 2, alpha = 1, label = recode(label, "a" = "id"))

sgwide_labels <- tibble(id = 1, a = "id") %>%
  proc_data("2-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 1, alpha = 1, label = recode(label, "a" = "id"))

sglong_extra_keys <- map_dfr(
  seq_len(nrow(wide) - 1),
  ~ filter(sgwide, .y > -1) # Extra key blocks in long column
)

n_key_cols <- length(setdiff(colnames(wide), "id"))

sglong_extra_id <- map_dfr(
  seq_len(n_key_cols - 1),
  ~ filter(sgwide, .x == 1) # Extra id column blocks for long column
)

sgdata <- bind_rows(
  sgwide,
  sgwide_labels,
  sglong,
  sglong_labels,
  sglong_extra_keys,
  sglong_extra_id
) %>%
  mutate(
    label = ifelse(value %in% setdiff(colnames(wide), "id"), "name", label),
    label = ifelse(value %in% c("name", "val"), "zzz", label),
    .text_color = ifelse(grepl("label", .id), "black", "white"),
    .text_size = ifelse(grepl("label", .id), 8, 10)
  ) %>%
  arrange(label, .id, value) %>%
  mutate(frame = factor(frame, labels = c('pivot_wider(long, names_from = name, values_from = value)', 'pivot_longer(wide, -id)'))) %>%
  select(.x, .y, everything())

sgstatic <-
  sgdata %>%
  split(.$frame) %>%
  imap(~ plot_data(.x, .y) +
         ylim(-6.5, 0.5) +
         labs(subtitle = "returns") +
         theme(
           plot.title = element_text(size = 16),
           plot.subtitle = element_text(family = "Fira Sans", size = 14, color = "grey50", hjust = 0.5, margin = margin(25))
           )
  )

save_static_plot(sgstatic[[1]], "tidyr-wider")
save_static_plot(sgstatic[[2]], "tidyr-longer")

sganim <-
  sgdata %>%
  plot_data() %>%
  animate_plot() +
  view_follow() +
  labs(title = "{ifelse(transitioning, next_state, ifelse(grepl('pivot_longer', next_state), 'long', 'wide'))}") +
  ease_aes("sine-in-out", x = "exponential-out")

sganim <- animate(sganim)
anim_save(here::here("images", "tidyr-longer-wider.gif"), sganim)

