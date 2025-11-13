library(tidyverse)
library(lme4)

film <- rep(c("F1", "F2"), 4)
zustand <- rep(c("nuechtern", "nuechtern", "betrunken", "betrunken"), 2)
tomatometer <- c(40, 70, 68, 82, 5, 52, 35, 75)
proband <- rep(c("P1", "P2"), each = 4)
grand_mean <- mean(tomatometer)

df <- tibble(film, zustand, tomatometer, proband, grand_mean)


# fixed effects model prediction
df <- df %>%
  group_by(zustand) %>%
  summarise(FE = mean(tomatometer)) %>%
  right_join(df, by = c("zustand"))

# subjects random intercepts only prediction
df <- df %>%
  group_by(proband) %>%
  summarise(subj_mean_diff = (mean(tomatometer) - grand_mean[1])) %>%
  right_join(df, by = c("proband")) %>%
  mutate(subj_RI = FE + subj_mean_diff)


# item random intercepts only prediction
df <- df %>%
  group_by(film) %>%
  summarise(mov_mean_diff = (mean(tomatometer) - grand_mean[1])) %>%
  right_join(df, by = c("film")) %>%
  mutate(mov_RI = FE + mov_mean_diff)


# subjects random intercepts and random slopes
df <- df %>%
  group_by(proband, zustand) %>%
  summarise(subj_RI_RS = mean(tomatometer)) %>%
  right_join(df, by = c("proband", "zustand"))

# movie random intercepts and random slopes
df <- df %>%
  group_by(film, zustand) %>%
  summarise(mov_RI_RS = mean(tomatometer)) %>%
  right_join(df, by = c("film", "zustand"))

# crossed random intercepts and slopes  
  
df <- df %>%
  group_by(proband, film, zustand) %>%
  summarise(cr_RI_RS = mean(tomatometer)) %>%
  ungroup() %>%
  right_join(df, by = c("proband", "film", "zustand")) %>%
  select(-mov_mean_diff, -subj_mean_diff) %>%
  mutate(x_cat = c(1,2, 4, 5, 8, 9, 11, 12))

# write_csv(df, "_sessions/MixedModels/MEM_example.csv")

# Plots ========================================

# Fixed effects only plot --------------------
g_size <- 6
dat_plot <- ggplot(df, aes(x_cat, tomatometer)) +
  geom_hline(yintercept = grand_mean[1], lty = 2, alpha = .35, size = 1.25) +
  geom_point(size = g_size, col = "#606061")  +
  annotate(geom = "text", x = c(1,2, 4, 5, 8, 9, 11, 12), y = -5,
           label = rep(c("F1", "F2"), 4), size = 6) +
  annotate(geom = "text", x = c(1.5, 4.5, 8.5, 11.5),
           y = -12, label = rep(c("Nüchtern", "Betrunken"), 2), size = 6) +
  annotate(geom = "text", x = c(3, 10), y = -20, label = c("Proband 1", "Proband 2"),
           size = 7, fontface = 2) +
  geom_segment(x = c(1,2, 4, 5, 8, 9, 11, 12), y = rep(0),
               xend = c(1,2, 4, 5, 8, 9, 11, 12), yend = rep(-1),
               col = "black") +
  geom_vline(xintercept = 6.5) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 13), expand = FALSE, clip = "off") +
  scale_x_discrete(breaks = "none") +
  labs(y = "Tomatometer",
       x = " ") +
  theme(
    plot.margin = unit(c(.5, .5, .5, .5), "lines"),
    axis.title.x = element_text(margin = margin(t = 50, r = 0, b = 0, l = 0)),
    axis.text.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

dat_plot
ggsave("_sessions/GemischteModelle/image/dat_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Fixed Effects plot -----------------------------
dat_plot + geom_point(aes(y = FE), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/GemischteModelle/image/FE_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Subjects Random Intercepts only plot -----------
dat_plot + geom_point(aes(y = subj_RI), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/GemischteModelle/image/subj_RI_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Movie Random Intercepts only plot -----------
dat_plot + geom_point(aes(y = mov_RI), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/GemischteModelle/image/mov_RI_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)


# Subject Random Intercepts and Slopes plot -----------
dat_plot + geom_point(aes(y = subj_RI_RS), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/GemischteModelle/image/subj_RI_RS_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Movie Random Intercepts and Slopes plot -----------
dat_plot + geom_point(aes(y = mov_RI_RS), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/GemischteModelle/image/mov_RI_RS_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Crossed Random Intercepts and Slopes plot -----------
dat_plot + geom_point(aes(y = cr_RI_RS), shape = 17, col = "#EA4B68",
                      size = g_size)
ggsave("_sessions/GemischteModelle/image/cr_RI_RS_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)


### study site data for slides =========================================


b0_s1 <- 4
b0_s2 <- 2
b0_s3 <- 7

b1_s1 <- .8
b1_s2 <- .3
b1_s3 <- .4

x_s1 <- rep(c(5, 10, 15, 20, 25, 30), 30)
x_s2 <- rep(c(5, 10, 15, 20, 25, 30), 30)
x_s3 <- rep(c(5, 10, 15, 20, 25, 30), 30)

y_s1 <- b0_s1 + b1_s1 * x_s1 + rnorm(length(x_s1), 0, 5)
y_s2 <- b0_s2 + b1_s2 * x_s2 + rnorm(length(x_s2), 0, 5)
y_s3 <- b0_s3 + b1_s3 * x_s3 + rnorm(length(x_s3), 0, 5)

s_dat <- tibble(
  y = c(y_s1, y_s2, y_s3),
  x = c(x_s1, x_s2, x_s3),
  Site = rep(c("Site 1", "Site 2", "Site 3"), each = length(x_s1))
)

ggplot(s_dat, aes(x, y)) + 
  geom_point(col = "#606061", alpha = .7) +
  geom_smooth(method = "lm", col = "#EA4B68", se = FALSE) +
  theme_classic() +
  labs(y = "Effect",
       x = "Dose") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 18,face = "bold"),
    axis.line = element_line(size = 1)
  )


ggplot(s_dat, aes(x, y, col = Site)) + 
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(y = "Effect",
       x = "Dose") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 18,face = "bold"),
    axis.line = element_line(size = 1)
  )

