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

psg_wide <- wide %>%
  proc_data("0-wide", colorize_wide_tidyr) %>%
  mutate(frame = 1, .id = "0-wide")

psg_long <- wide %>%
  pivot_longer(-id) %>%
  proc_data("3-tall", color_fun = function(x, y) x) %>%
  split(.$label)

psg_long$id <-
  psg_wide %>%
  filter(label == "id") %>%
  select(value, color) %>%
  left_join(psg_long$id, ., by = "value") %>%
  mutate(alpha = 1)

psg_long$name <-
  psg_wide %>%
  filter(label != "id") %>%
  select(label, color) %>%
  left_join(psg_long$name, ., by = c("value" = "label")) %>%
  distinct() %>%
  mutate(alpha = 1)

psg_long$val <-
  psg_wide %>%
  filter(label != "id", .y < 0) %>%
  select(value, color) %>%
  left_join(psg_long$val, ., by = "value") %>%
  mutate(alpha = 0.6)

psg_long <- bind_rows(psg_long) %>% mutate(frame = 2)

# above checked -----------------------------------------------------------
psg_long_labels <- tibble(id = 1, a = "id", x = "name", y = "val") %>%
  proc_data("4-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 2, alpha = 1, label = recode(label, "a" = "id"))

psg_wide_labels <- tibble(id = 1, a = "id") %>%
  proc_data("2-label") %>%
  filter(label != "id") %>%
  mutate(color = "#FFFFFF", .y = 0, .x = .x -1, frame = 1, alpha = 1, label = recode(label, "a" = "id"))

psg_long_extra_keys <- map_dfr(
  seq_len(nrow(wide) - 1),
  ~ filter(psg_wide, .y > -1) # Extra key blocks in long column
)

n_key_cols <- length(setdiff(colnames(wide), "id"))

psg_long_extra_id <- map_dfr(
  seq_len(n_key_cols - 1),
  ~ filter(psg_wide, .x == 1) # Extra id column blocks for long column
)

psg_data <- bind_rows(
  psg_wide,
  psg_wide_labels,
  psg_long,
  psg_long_labels,
  psg_long_extra_keys,
  psg_long_extra_id
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

psg_static <-
  psg_data %>%
  split(.$frame) %>%
  imap(~ plot_data(.x, .y) +
         ylim(-6.5, 0.5) +
         labs(subtitle = "returns") +
         theme(plot.subtitle = element_text(family = "Fira Sans", size = 14, color = "grey50", hjust = 0.5, margin = margin(25)))
  )

save_static_plot(psg_static[[1]], "tidyr-wider")
save_static_plot(psg_static[[2]], "tidyr-longer")

psg_anim <-
  psg_data %>%
  plot_data() %>%
  animate_plot() +
  view_follow() +
  labs(title = "{ifelse(transitioning, next_state, ifelse(grepl('pivot_longer', next_state), 'long', 'wide'))}") +
  ease_aes("sine-in-out", x = "exponential-out")

psg_anim <- animate(psg_anim)
anim_save(here::here("images", "tidyr-longer-wider.gif"), psg_anim)

