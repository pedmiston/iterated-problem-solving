# ---- iterated-problem-solving

# Setup ----
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
library(tidyverse) # Load tidyverse after totems to prevent dplyr::filter from being masked
t_ <- load_totems_theme()

t_$base_theme <- t_$base_theme +
  theme(text = element_text(family = "serif"))

# List to hold descriptives for in-text citation
exp1 <- list()

# Methods ----
data("Sessions")

Exp1Participants <- Sessions %>%
  filter_exp1() %>%
  count(Generation) %>%
  rename(N = n) %>%
  mutate(Inheritance = c("No", rep("Yes", 3)))

n_teams <- Sessions %>%
  filter_exp1() %>%
  select(TeamID) %>%
  unique() %>%
  nrow()

exp1$n_participants <- sum(Exp1Participants$N)
exp1$n_teams <- n_teams

data("Teams")
data("Sessions")

methods <- list()  # Store vars for in-text reference
methods$n_unique_guesses_6 <- count_unique_combinations(6)
methods$n_unique_guesses_6_pct <- round(3/methods$n_unique_guesses_6 * 100, 1)


report_lmer_mod <- function(lmer_mod, term, formats = NULL, reverse_sign = FALSE) {
  term_ <- term  # work around NSE in filter
  results <- broom::tidy(lmer_mod, effects = "fixed") %>%
    filter(term == term_) %>%
    as.list()

  if(reverse_sign) {
    results$estimate <- -results$estimate
    results$statistic <- -results$statistic
  }

  fmt <- c(b=2, se=2, t=2)
  if(!is.null(formats)) fmt[names(formats)] <- formats

  fstring <- sprintf("_b_ = %%.%sf (SE = %%.%sf), _t_ = %%.%sf", fmt["b"], fmt["se"], fmt["t"])
  sprintf(fstring, results$estimate, results$std.error, results$statistic)
}

report_lm_mod <- function(lm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(lm_mod) %>%
    filter(term == term_) %>%
    as.list()

  lm_summary <- broom::glance(lm_mod) %>% as.list()
  results$df <- lm_summary$df.residual

  results$p_value_str <- compute_p_string(results$p.value)

  if (p_value_only == TRUE) {
    return(results$p_value_str)
  }

  sprintf("_b_ = %.2f (SE = %.2f), _t_(%.1f) = %.2f, %s",
          results$estimate, results$std.error, results$df, results$statistic, results$p_value_str)
}

report_glm_mod <- function(glm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(glm_mod) %>%
    filter(term == term_) %>%
    as.list()

  glm_summary <- broom::glance(glm_mod) %>% as.list()
  results$df <- glm_summary$df.residual

  results$p_value_str <- compute_p_string(results$p.value)

  sprintf("_b_ = %.2f logodds (SE = %.2f), _z_ = %.2f, %s",
          results$estimate, results$std.error, results$statistic, results$p_value_str)
}

report_beta <- function(mod, param, digits = 1, transform = NULL) {
  param_ <- param # prevent masking in NSE
  estimate <- mod %>%
    summary %>%
    .$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("param") %>%
    filter(param == param_) %>%
    .$Estimate
  if(!is.null(transform)) estimate <- transform(estimate)
  round(estimate, digits = digits)
}

report_modcomp <- function(modcomp) {
  modcomp <- as.list(modcomp[2, ])
  p_string <- compute_p_string(modcomp$`Pr(>Chisq)`)
  print(sprintf("$\\chi^2$(%i) = %.4f, %s", modcomp$`Chi Df`, modcomp$Chisq, p_string))
}

report_page_test <- function(page_trend_test_results) {
  page_trend_test_results$p_val_str <- compute_p_string(page_trend_test_results$px2)
  print(sprintf("Page's _L_ = %.0f, $\\chi^2$ = %.0f, %s",
                page_trend_test_results$L,
                page_trend_test_results$x2L,
                page_trend_test_results$p_val_str))
}

compute_p_string <- function(p_value) {
  min_p_value <- 0.001
  if (p_value < min_p_value) {
    p_value_str <- "_p_ < 0.001"
  } else {
    p_value_str <- paste("_p_ = ", round(p_value, 3))
  }
  p_value_str
}

# Jitter Generation by TeamID for plotting
jitter_team_generation <- . %>%
  group_by(TeamID) %>%
  mutate(GenerationJittered = Generation + rnorm(1, mean = 0, sd = 0.05)) %>%
  ungroup()

# Recode Generation poly
recode_generation_quad <- . %>%
  mutate(
    GenerationSqr = Generation^2,
    Generation0Sqr = Generation0^2
  )

recode_generation_base0 <- . %>%
  mutate(Generation0 = Generation - 1)

# Data ----
data("Sessions")
data("Guesses")
data("InventoryInfo")

TeamCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumTeams = n)

PlayerCounts <- Sessions %>%
  left_join(
    Teams %>%
      select(TeamID, SessionDuration, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(PlayerID, Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumPlayers = n)

ConditionCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts) %>%
  left_join(PlayerCounts)

Innovations <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad()

# Link each player to their ancestor
AncestorMap <- Sessions %>%
  filter_exp1() %>%
  group_by(TeamID) %>%
  arrange(Generation) %>%
  mutate(AncestorID = lag(SessionID)) %>%
  ungroup() %>%
  filter(Generation > 1) %>%
  select(SessionID, AncestorID)

# Get the final inventory id achieved by each session
InheritedInventoryIDs <- Guesses %>%
  filter_exp1() %>%
  group_by(SessionID) %>%
  summarize(AncestorInventoryID = PrevSessionInventoryID[SessionTime == max(SessionTime)]) %>%
  rename(AncestorID = SessionID)

InheritedInnovations <- Guesses %>%
  filter_exp1() %>%
  filter(Generation < 4) %>%
  group_by(SessionID) %>%
  summarize(InheritanceSize = max(SessionInventorySize) - 6) %>%
  rename(AncestorID = SessionID) %>%
  left_join(AncestorMap, .)

# Create map of innovations to difficulty scores
Difficulties <- InventoryInfo %>%
  transmute(
    PrevSessionInventoryID = ID,
    UniqueSessionResult = 1,
    GuessDifficulty = (UniqueGuesses/max(UniqueGuesses)),
    CombinationDifficulty = (UniqueCombinations/max(UniqueCombinations))
  )

DifficultyScores <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  left_join(Difficulties) %>%
  group_by(SessionID) %>%
  summarize(DifficultyScore = sum(CombinationDifficulty, na.rm = TRUE)) %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad()

InheritedDifficulties <- DifficultyScores %>%
  filter(Generation < 4) %>%
  select(AncestorID = SessionID, InheritedDifficulty = DifficultyScore) %>%
  left_join(AncestorMap, .)

Inheritances <- left_join(AncestorMap, InheritedInventoryIDs) %>%
  left_join(InheritedInnovations) %>%
  left_join(InheritedDifficulties)

StageTimes <- Guesses %>%
  filter_exp1() %>%
  filter(Generation > 1) %>%
  group_by(SessionID) %>%
  summarize(LearningTime = max(SessionTime[Stage == "learning"])) %>%
  mutate(PlayingTime = 25 - LearningTime)

OutliersByLearningTime <- StageTimes %>%
  transmute(SessionID, Outlier = LearningTime > 22)
StageTimes %<>% left_join(OutliersByLearningTime)
exp1$n_outliers <- sum(OutliersByLearningTime$Outlier)

exp1$mean_inheritance_size <- round(mean(Inheritances$InheritanceSize), 1)
exp1$sd_inheritance_size <- round(sd(Inheritances$InheritanceSize), 1)

# Learning times ----
exp1$mean_learning_time_min <- round(mean(StageTimes$LearningTime), 1)
exp1$proportion_learning_time <- round((exp1$mean_learning_time_min/25) * 100, 1)

learning_times_plot <- ggplot(StageTimes) +
  aes(LearningTime, fill = Outlier) +
  geom_histogram(binwidth = 2.5, center = 1.25) +
  scale_x_continuous("Learning period (min)", breaks = seq(0, 25, by = 5)) +
  ylab("Count") +
  scale_fill_manual(values = t_$color_picker(c("blue", "orange"))) +
  t_$base_theme +
  theme(legend.position = "none")

# Learning rates ----
LearningRates <- left_join(Inheritances, StageTimes)

learning_rates_mod <- lm(LearningTime ~ 0 + InheritanceSize,
                         data = LearningRates)
learning_rates_preds <- get_lm_mod_preds(learning_rates_mod) %>%
  rename(LearningTime = fit, SE = se.fit)

t_$scale_shape_outlier <- scale_shape_manual(values = c(1, 4))

exp1$learning_cor <- round(cor(LearningRates$InheritanceSize, LearningRates$LearningTime), 2)

learning_rates_plot <- ggplot(LearningRates) +
  aes(InheritanceSize, LearningTime, shape = Outlier) +
  geom_point(position = position_jitter(width = 0.1)) +
  # geom_ribbon(aes(ymin = LearningTime-SE, ymax = LearningTime + SE),
  #             stat = "identity", data = learning_rates_preds,
  #             fill = t_$diachronic_color, alpha = 0.4) +
  scale_x_continuous("Tools inherited") +
  scale_y_continuous("Learning time (min)", breaks = seq(0, 25, by = 5)) +
  t_$scale_shape_outlier +
  t_$base_theme +
  guides(shape = "none")

# New innovations ----
NewInnovations <- left_join(Inheritances, StageTimes) %>%
  inner_join(Innovations) %>%
  mutate(
    NumUniqueInnovations = NumInnovations - InheritanceSize,
    ExceededAncestor = (NumUniqueInnovations > 0),
    InheritanceSizeC = InheritanceSize - mean(InheritanceSize)
  )

exp1$n_did_not_exceed <- with(NewInnovations, sum(NumUniqueInnovations == 0))
exp1$n_did_exceed <- with(NewInnovations, sum(NumUniqueInnovations > 0))
exp1$pct_did_exceed <- round(
  (exp1$n_did_exceed/(exp1$n_did_not_exceed + exp1$n_did_exceed)) * 100,
  digits = 1
)

exceed_ancestor_mod <- glm(
  ExceededAncestor ~ 1,
  family = "binomial",
  data = NewInnovations
)
exp1$odds_of_exceeding <- report_beta(exceed_ancestor_mod, "(Intercept)", transform = exp)
exp1$logodds_of_exceeding <- report_glm_mod(exceed_ancestor_mod, "(Intercept)")

innovations_created_and_inherited_plot <- ggplot(NewInnovations) +
  aes(InheritanceSize, NumInnovations) +
  geom_point(position = position_jitter(width = 0.2), shape = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.5) +
  scale_x_continuous("Tools inherited") +
  scale_y_continuous("Tools created") +
  t_$base_theme +
  guides(shape = "none")

new_innovations_mod <- lmer(
  NumUniqueInnovations ~ InheritanceSize + (1|AncestorInventoryID),
  data = NewInnovations
)
exp1$inheritance_size_slope_stats <- report_lmer_mod(new_innovations_mod, "InheritanceSize")

new_innovations_preds <- data_frame(InheritanceSize = 2:20) %>%
  cbind(., predictSE(new_innovations_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueInnovations = fit, SE = se.fit)

scale_alpha_outlier <- scale_alpha_manual(values = c(1, 0.5))
filter_lm_range <- . %>% filter(InheritanceSize >= 2, InheritanceSize <= 20)

new_innovations_plot <- ggplot(NewInnovations) +
  aes(InheritanceSize, NumUniqueInnovations) +
  geom_ribbon(aes(ymin = NumUniqueInnovations-SE, ymax = NumUniqueInnovations+SE),
              stat = "identity", data = new_innovations_preds,
              fill = t_$diachronic_color, alpha = 0.8) +
  geom_point(position = position_jitter(width = 0.2), shape = 1) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
  scale_alpha_outlier +
  scale_x_continuous("Tools inherited") +
  scale_y_continuous("New tools discovered") +
  guides(shape = "none", alpha = "none") +
  t_$base_theme

# Page's trend test ----
data("Guesses")
data("Sessions")

PerformanceMatrix <- Innovations %>%
  select(TeamID, Generation, NumInnovations) %>%
  tidyr::spread(Generation, NumInnovations) %>%
  select(-TeamID) %>%
  as.matrix()

page_trend_test_results <- crank::page.trend.test(PerformanceMatrix, ranks = FALSE)
exp1$page_test <- report_page_test(page_trend_test_results)

# Innovations by generation ----
data("Guesses")
data("Sessions")

Innovations <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad()

innovations_by_generation_mod <- lmer(
  NumInnovations ~ Generation0 + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
innovations_by_generation_quad_mod <- lmer(
  NumInnovations ~ Generation0 + Generation0Sqr + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
innovations_by_generation_modcomp <-
  anova(innovations_by_generation_mod, innovations_by_generation_quad_mod)

exp1$gen0_slope <- report_beta(innovations_by_generation_quad_mod, "Generation0")
exp1$gen0_slope_stats <- report_lmer_mod(innovations_by_generation_quad_mod, "Generation0")
exp1$gen0sqr_slope <- report_beta(innovations_by_generation_quad_mod, "Generation0Sqr")
exp1$gen0sqr_slope_stats <- report_lmer_mod(innovations_by_generation_quad_mod, "Generation0Sqr")
exp1$quad_modcomp <- report_modcomp(innovations_by_generation_modcomp)

innovations_by_generation_preds <- data_frame(Generation = 1:4) %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  cbind(., predictSE(innovations_by_generation_quad_mod, newdata = ., SE = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

innovations_by_generation_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations) +
  geom_line(aes(GenerationJittered, group = TeamID),
            color = t_$color_picker("blue"), alpha = 0.6) +
  geom_line(aes(group = 1), data = innovations_by_generation_preds,
            color = "black", size = 1.0) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = innovations_by_generation_preds,
                color = "black", width = 0.15, size = 1.0) +
  t_$scale_y_num_tools +
  t_$base_theme +
  theme(
    panel.grid.minor.x = element_blank()
  )

# Difficulty by generations ----
difficulty_by_generation_mod <- lmer(
  DifficultyScore ~ Generation0 + (Generation0 + Generation0Sqr|TeamID),
  data = DifficultyScores
)
difficulty_by_generation_quad_mod <- lmer(
  DifficultyScore ~ Generation0 + Generation0Sqr + (Generation0 + Generation0Sqr|TeamID),
  data = DifficultyScores
)
difficulty_by_generation_modcomp <-
  anova(difficulty_by_generation_mod, difficulty_by_generation_quad_mod)

exp1$difficulty_by_generation_mod_slope_stats <- report_lmer_mod(difficulty_by_generation_mod, "Generation0")
exp1$difficulty_by_generation_quad_mod_slope_stats <- report_lmer_mod(difficulty_by_generation_quad_mod, formats = c(b=4, se=4), "Generation0Sqr")

# Delta difficulty ----
DeltaDifficulty <- left_join(Inheritances, StageTimes) %>%
  inner_join(DifficultyScores) %>%
  mutate(DifficultyDelta = DifficultyScore - InheritedDifficulty)

delta_difficulty_mod <- lmer(
  DifficultyDelta ~ InheritanceSize + (1|AncestorInventoryID),
  data = DeltaDifficulty
)
exp1$delta_diff_slope_stats <- report_lmer_mod(delta_difficulty_mod, "InheritanceSize", formats = c(b=4, se=4))

delta_difficulty_preds <- data_frame(InheritanceSize = 2:20) %>%
  cbind(., predictSE(delta_difficulty_mod, newdata = ., se = TRUE)) %>%
  rename(DifficultyDelta = fit, SE = se.fit)

delta_difficulty_plot <- ggplot(DeltaDifficulty) +
  aes(InheritanceSize, DifficultyDelta) +
  geom_ribbon(aes(ymin = DifficultyDelta-SE, ymax = DifficultyDelta+SE),
              stat = "identity", data = delta_difficulty_preds,
              fill = t_$diachronic_color) +
  geom_point(position = position_jitter(width = 0.2), shape = 1) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
  scale_x_continuous("Tools inherited") +
  scale_y_continuous("Change in complexity score") +
  t_$scale_shape_outlier +
  scale_alpha_outlier +
  guides(shape = "none", alpha = "none") +
  t_$base_theme

# Playing time ----
playing_time_mod <- lmer(
  NumUniqueInnovations ~ PlayingTime + (1|AncestorInventoryID),
  data = NewInnovations
)

exp1$new_innovations_per_minute <- report_beta(playing_time_mod, "PlayingTime", digits = 2)
exp1$minutes_per_new_innovation <- round(1/report_beta(playing_time_mod, "PlayingTime", digits = 2), 1)
exp1$playing_time_slope_stats <- report_lmer_mod(playing_time_mod, "PlayingTime")

playing_time_by_inheritance_mod <- lmer(
  NumUniqueInnovations ~ PlayingTime * InheritanceSize + (1|AncestorInventoryID),
  data = NewInnovations
)

playing_time_modcomp <-
  anova(playing_time_mod, playing_time_by_inheritance_mod)
exp1$playing_time_modcomp <- report_modcomp(playing_time_modcomp)

playing_time_preds <- data_frame(PlayingTime = 4:23) %>%
  cbind(., predictSE(playing_time_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueInnovations = fit, SE = se.fit)

playing_time_plot <- ggplot(NewInnovations) +
  aes(PlayingTime, NumUniqueInnovations) +
  geom_ribbon(aes(ymin = NumUniqueInnovations-SE, ymax = NumUniqueInnovations+SE),
              stat = "identity", data = playing_time_preds,
              fill = t_$diachronic_color, alpha = 0.8) +
  geom_point(aes(shape = Outlier), position = position_jitter(height = 0.1)) +
  xlab("Discovery period (min)") +
  ylab("New tools discovered") +
  guides(shape = "none", alpha = "none") +
  t_$scale_shape_outlier +
  t_$base_theme

# Guesses per item ----
data("Guesses")
data("AdjacentItems")
data("Teams")

SessionTypes50min <- Sessions %>%
  filter_exp1()

GuessesPerItem50min <- Guesses %>%
  filter_exp1() %>%
  # Copy guesses for each adjacent item
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID"))

CostPerItem50min <- GuessesPerItem50min %>%
  filter(Generation == 1 | Stage == "learning") %>%
  group_by(TeamID, Generation, Adjacent) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%

  # Re-label summarized data
  left_join(SessionTypes50min) %>%
  recode_strategy() %>%
  recode_discovered() %>%
  label_inheritance() %>%
  recode_inheritance()

# Guesses per item by inheritance.
# Determine the impact of inheritance on guessing ability
# by comparing guessing rates for each item discovered
# by Diachronic players.
guesses_per_item_by_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance +
    (Diachronic_v_NoInheritance|Adjacent),
  data = filter(CostPerItem50min, Discovered))

exp1$guesses_per_item_by_inheritance <- report_lmer_mod(guesses_per_item_by_inheritance_mod, "Diachronic_v_NoInheritance")

guesses_per_item_by_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance != "individual_inheritance") %>%
  cbind(., predictSE(guesses_per_item_by_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_by_inheritance_plot <- ggplot(CostPerItem50min) +
  aes(InheritanceLabel, TotalGuesses) +
  geom_line(aes(group = Adjacent), color = t_$color_picker("blue"),
            stat = "summary", fun.y = "mean",
            size = 0.6) +
  geom_line(aes(group = 1),
            stat = "identity", data = guesses_per_item_by_inheritance_preds,
            size = 0.6) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_item_by_inheritance_preds,
                width = 0.05, size = 0.6) +
  scale_x_discrete("", labels = c("Generation 1", "Generations 2-4")) +
  scale_y_continuous("Guesses per tool", breaks = seq(0, 150, by = 25)) +
  coord_cartesian(ylim = c(0, 150)) +
  t_$base_theme

# Guesses per item: Playing ----
CostPerItem50minPlaying <- GuessesPerItem50min %>%
  filter(Stage == "playing") %>%
  group_by(TeamID, Generation, Adjacent) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%

  # Re-label summarized data
  left_join(SessionTypes50min) %>%
  recode_strategy() %>%
  recode_discovered() %>%
  label_inheritance() %>%
  recode_inheritance()

# Guesses per item by inheritance.
# Determine the impact of inheritance on guessing ability
# by comparing guessing rates for each item discovered
# by Diachronic players.
guesses_per_new_item_by_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance +
    (Diachronic_v_NoInheritance|Adjacent),
  data = filter(CostPerItem50minPlaying, Discovered))

guesses_per_new_item_by_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance != "individual_inheritance") %>%
  cbind(., predictSE(guesses_per_new_item_by_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

exp1$guesses_per_new_item_by_inheritance_mod <- report_lmer_mod(guesses_per_new_item_by_inheritance_mod, "Diachronic_v_NoInheritance")

guesses_per_new_item_by_inheritance_plot <- ggplot(CostPerItem50minPlaying) +
  aes(InheritanceLabel, TotalGuesses) +
  geom_line(aes(group = Adjacent), color = t_$color_picker("blue"),
            stat = "summary", fun.y = "mean",
            size = 0.6) +
  geom_line(aes(group = 1),
            stat = "identity", data = guesses_per_new_item_by_inheritance_preds,
            size = 0.6) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_new_item_by_inheritance_preds,
                width = 0.05, size = 0.6) +
  scale_x_discrete("", labels = c("Generation 1", "Generations 2-4")) +
  scale_y_continuous("Guesses per tool", breaks = seq(0, 150, by = 25)) +
  coord_cartesian(ylim = c(0, 150)) +
  t_$base_theme

# Guess types ----
GuessTypes <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  label_inheritance() %>%
  recode_inheritance()

num_redundant_guesses_mod <- lm(NumRedundantGuesses ~ Diachronic_v_NoInheritance,
                                data = GuessTypes)
exp1$num_redundant_guesses <- report_lm_mod(num_redundant_guesses_mod, "Diachronic_v_NoInheritance")

prop_redundant_guesses_mod <- lm(PropRedundantGuesses ~ Diachronic_v_NoInheritance,
                                 data = GuessTypes)
exp1$prop_redundant_guesses <- report_lm_mod(prop_redundant_guesses_mod, "Diachronic_v_NoInheritance")

prop_unique_guesses_mod <- lm(PropUniqueGuesses ~ Diachronic_v_NoInheritance,
                              data = GuessTypes)

GuessTypes50minSummary <- Guesses %>%
  filter_exp1() %>%
  label_inheritance() %>%
  recode_inheritance() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(Inheritance) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  select(Inheritance, PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(PropGuessType, PropGuesses, -Inheritance) %>%
  recode_prop_guess_type_total() %>%
  recode_inheritance()

prop_guess_types_50min_plot <- ggplot(GuessTypes50minSummary) +
  aes(InheritanceLabel, PropGuesses, fill = PropGuessTypeLabel) +
  geom_bar(stat = "identity") +
  scale_x_discrete("", labels = c("No inheritance", "Inheritance")) +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  scale_fill_manual("Guess types",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  t_$base_theme +
  theme(panel.grid.major.x = element_blank())
