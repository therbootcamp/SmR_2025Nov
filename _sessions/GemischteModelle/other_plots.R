# general plots for MEM session
library(tidyverse)
library(lme4)
library(patchwork)

# normal distribution to draw data from 
# parameters that will be passed to ``stat_function``
n = 1000
mean = 50
sd = 12
binwidth = 0.3 # passed to geom_histogram and stat_function
set.seed(1)
df <- data.frame(x = rnorm(n, mean, sd))

ggplot(df, aes(x = x, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  theme_classic() +
#  geom_histogram(binwidth = binwidth, 
#                 colour = "white", fill = "cornflowerblue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth,
                color = "#EA4B68", size = 5) +
  xlim(0, 100) +
  labs(
    x = "Tomatometer",
    y = ""
  ) +
  scale_y_discrete(breaks = "none") +
  theme(
    axis.text.y = element_blank(),
    axis.text = element_text(size = 17),
    axis.title = element_text(size = 20,face = "bold")
  )
ggsave("_sessions/MixedModels/image/norm_dist_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)

# Fixed effects regression ---

baselers <- read_csv("https://raw.githubusercontent.com/therbootcamp/baselers/master/inst/extdata/baselers.txt")
baselers <- baselers %>% 
  filter(!is.na(income),!is.na(age),!is.na(height)) %>%
  slice(sample(1:nrow(baselers),1000)) %>% 
  mutate(sex01 = sex == 'male')
source("https://raw.githubusercontent.com/therbootcamp/therbootcamp.github.io/master/_materials/code/baselrbootcamp_palettes.R")
set.seed(102)
x <- rnorm(10)
y <- .7 * x + rnorm(10, sd = .3) + 2
data <- data.frame(x, y)
mod <- lm(y ~ x, data = data)

great_intercept <- mod$coefficients[1]
great_slope <- mod$coefficients[2]
dat_great <- data.frame(x0 = x, x1 = x,
                        y0 = y, y1 = great_intercept + great_slope * x)

great_err <- great_raw + 
  geom_linerange(data = dat_great, aes(x = x0, ymin = y0, ymax = y1), col = baselrbootcamp_cols("magenta")) +
  geom_point(data = dat_great, aes(x = x0, y = y1, size = 2), col = baselrbootcamp_cols("green"), pch = "X", size = 4) +
  labs(subtitle = paste("Mean Squared Error (MSE) = ", round(mean((dat_great$y1 - dat_great$y0) ^ 2), 2)),
       x = "Predictor", y = "Criterion")


# Hierarchical shrinkage --------------------------
df <- readRDS("_sessions/MixedModels/Tomatometer_dat.RDS")
grand_mean <- mean(df$Tomatometer)

# fixed effects model prediction
df <- df %>%
  group_by(State) %>%
  summarise(FE = mean(Tomatometer)) %>%
  right_join(df, by = c("State"))

# random intercepts only
df <- df %>%
  group_by(ID) %>%
  summarise(subj_mean_diff = (mean(Tomatometer) - grand_mean)) %>%
  right_join(df, by = c("ID")) %>%
  mutate(subj_RI = FE + subj_mean_diff)

# fit model
LMM_out <- lmer(Tomatometer ~ State +
                  # by-ID random slopes and intercepts
                  (State|ID) +
                  # by-ID random slopes and intercepts
                  (State|Movie),
                # define data
                data = df)

mod_int <- fixef(LMM_out)[1]
mod_ran <- as_vector(ranef(LMM_out)$ID[, 1])
mod_ran_state <- as_vector(ranef(LMM_out)$ID[, 2])

mod_pred_drunk <- mod_int + mod_ran + mod_ran_state
mod_pred_sober <- mod_int + mod_ran

dat_drunk <- df %>% group_by(ID, State) %>% summarise(subj_RI = subj_RI[1])



lmm_dat <- tibble(
  Data = rep(c("Real", "Predicted"), each = 200),
  Means = c(dat_drunk$subj_RI[dat_drunk$State == "Sober"],
            dat_drunk$subj_RI[dat_drunk$State == "Drunk"],
            mod_pred_sober, mod_pred_drunk),
  State = rep(c("Sober", "Drunk"), each = 100, times = 2),
  ID = rep(1:100, times = 4)
)

line_dat <- tibble(
  x = lmm_dat$Means[lmm_dat$State == "Sober" & lmm_dat$Data == "Predicted"],
  xend = lmm_dat$Means[lmm_dat$State == "Sober" & lmm_dat$Data == "Real"]
)
ggplot(lmm_dat %>% filter(State == "Sober"), aes(Means, Data)) +
  geom_point(lmm_dat %>% filter(State == "Sober" & Data == "Predicted"),
             mapping = aes(Means, Data),
             size = 5, alpha = .85, col = "white") +
  geom_segment(aes(x = x, y = 1, xend = xend, yend = 2 ),
               data=line_dat, colour = "#6ABA9A", size = 1,
               alpha = .8) +
  geom_point(lmm_dat %>% filter(State == "Sober" & Data == "Predicted"),
             mapping = aes(Means, Data),
             size = 5, alpha = .85, col = "#EA4B68") +
  geom_point(data = lmm_dat %>% filter(State == "Sober" & Data == "Real"),
             mapping = aes(Means, Data),
             size = 5, alpha = .85, col = "#606061") +
  theme_bw() +
  labs(y = "Person Intercepts (Sober)",
       x = "Tomatometer") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18,face = "bold")
  )

ggsave("_sessions/MixedModels/image/shrinkage_plot.png", width = 8,
       height = 5, device = "png", dpi = 600)



# distributions functions -----------------


p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1),
                col = "#EA4B68", alpha = .8, size = 3) + ylab("") +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL) +
  xlab("Gaussian") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold")
  )
p1


x1  <- 3:17
df <- data.frame(x = x1, y = dbinom(x1, 20, 0.3))

p2 <- ggplot(data.frame(x=c(0:20)), aes(x)) +
  geom_point(aes(y=dbinom(x, 20, .5)), colour="#EA4B68", alpha = .8, size = 5) +  
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL) +
  xlab("Binomial") + ylab("") + theme_bw() + theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold")
  )

p2

p3 <- ggplot(data = data.frame(x = c(0, 10)), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = 1),
                col = "#EA4B68", alpha = .8, size = 3) + ylab("") +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL) +
  xlab("Gamma") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold")
  )
p3

p4 <- ggplot(data.frame(x=c(0:10)), aes(x)) +
  geom_point(aes(y=dpois(x, 1)), colour="#EA4B68", alpha = .8, size = 5) +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL) +
  ylab("") +
  xlab("Poisson") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold")
  )
p4


p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

ggsave("_sessions/MixedModels/image/dist_ex_plot.png", width = 8,
       height = 6, device = "png", dpi = 600)
