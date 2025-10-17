# ----------------------------------------------------------------------
# 1. SETUP: Load Libraries and Define Constants/Functions
# ----------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggprism)
library(ggpubr)
library(ggbeeswarm)
library(extrafont)
library(rstatix)
library(ggpmisc)

# --- Define Constants ---

DATA_BASE_PATH <- ""
OUTPUT_DIR <- "SVGs"

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}

CHOW_COLOR <- "grey50"
ADENINE_COLOR <- "black"

FRACTION_COLORS <- c(
  "70" = "#51A0E1", "40-1" = "#C76BB3", "40-2" = "#12A597", "Mito" = "#AD725C",
  "PT" = "#51A0E1", "DT" = "#C76BB3", "Glom" = "#12A597"
)
FRACTION_DARK_COLORS <- c(
  "70" = "#154C7A", "40-1" = "#853273", "40-2" = "#084942", "Mito" = "#36221B",
  "PT" = "#154C7A", "DT" = "#853273", "Glom" = "#084942"
)

# --- Define Functions ---

read_and_clean_data <- function(file_name, sheet_name, cols_to_pivot, diet_levels = c("Chow", "Adenine")) {
  path <- paste0(DATA_BASE_PATH, file_name)
  df <- read_xlsx(path, sheet = sheet_name) |>
    na.omit() |>
    pivot_longer(all_of(cols_to_pivot), names_to = "State", values_to = "Rate") |>
    mutate(
      State = factor(State, levels = unique(State)),
      Diet = factor(Diet, levels = diet_levels)
    )
  
  if ("Fraction" %in% names(df)) {
    df <- df |> mutate(Fraction = factor(Fraction, levels = unique(Fraction)))
  }
  
  return(df)
}

plot_oxygen_or_atp_rate <- function(data, fraction_label, y_max, y_breaks, y_lab, color_set, file_suffix) {
  color_vals <- c(color_set[fraction_label], FRACTION_DARK_COLORS[fraction_label])
  fill_vals <- alpha(color_vals, 0.35)
  
  p <- data |>
    filter(Fraction == fraction_label & State != 'Basal') |>
    ggplot(aes(x = State, y = Rate, color = Diet, fill = Diet)) +
    theme_prism() +
    stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1,
                 position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
    stat_summary(geom = "crossbar", fun = 'mean', size = 0.625,
                 position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
    geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
    scale_color_manual(values = color_vals) +
    scale_fill_manual(values = fill_vals) +
    coord_cartesian(ylim = c(0, y_max), clip = "off") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, y_max, y_breaks)) +
    labs(y = y_lab) +
    geom_vline(xintercept = c(1.5, 2.5), linewidth = 1, linetype = "longdash", alpha = 0.25) +
    theme(
      axis.title.x = element_blank(),
      legend.text = element_text(size = 20),
      axis.title.y = element_text(size = 26),
      axis.text.x = element_text(size = 18),
      legend.position = 'bottom'
    ) +
    stat_pwc(
      p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE,
      label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}",
      y.position = y_max * 0.925, show.legend = FALSE, method = 't_test',
      size = 1, tip.length = 0, label.size = 7
    ) +
    ggtitle(NULL)
  
  if (grepl("JATP", file_suffix)) {
    p <- p +
      geom_segment(x = 1, xend = 4, y = y_max * (-0.09), yend = y_max * (-0.09), size = 0.9, color = "black") +
      annotate("text", label = '[ADP]', x = 2.5, y = y_max * (-0.13), fontface = 2, size = 6) +
      theme(
        legend.text = element_text(size = 18),
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 16),
        plot.margin = unit(c(1, 0, 0, 0), "cm"),
        legend.margin = margin(1, 0, 0, 0, unit = 'cm')
      )
    if (fraction_label == "Mito") {
      p <- p + geom_segment(x = 1, xend = 3, y = y_max * (-0.09), yend = y_max * (-0.09), size = 0.9, color = "black") +
        annotate("text", label = '[ADP]', x = 2, y = y_max * (-0.13), fontface = 2, size = 6)
    }
  }
  
  ggsave(paste0(OUTPUT_DIR, "/Fig ", file_suffix, ".svg"), p, width = 6, height = 6, dpi = 1200)
  
  return(p)
}

plot_weight <- function(data, y_col, y_max, y_breaks, y_lab, file_suffix) {
  y_col_sym <- sym(y_col)
  ggplot(data = data, aes(x = Diet, y = !!y_col_sym, color = Diet, fill = Diet)) +
    theme_prism() +
    stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1,
                 position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
    stat_summary(geom = "crossbar", fun = 'mean', size = 0.625,
                 position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
    geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
    scale_color_manual(values = c(CHOW_COLOR, ADENINE_COLOR)) +
    scale_fill_manual(values = alpha(c(CHOW_COLOR, ADENINE_COLOR), 0.35)) +
    coord_cartesian(ylim = c(0, y_max), clip = 'off') +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, y_max, y_breaks)) +
    labs(y = y_lab) +
    theme(
      axis.title.x = element_blank(),
      legend.text = element_text(size = 18),
      axis.title.y = element_text(size = 28),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 18)
    ) +
    stat_pwc(
      p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE,
      label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}",
      y.position = y_max * 0.95, show.legend = FALSE, method = 't_test',
      size = 1, tip.length = 0, label.size = 7
    ) +
    ggtitle(NULL) |>
    (\(p) { ggsave(paste0(OUTPUT_DIR, "/Fig ", file_suffix, ".svg"), p, width = 6, height = 6, dpi = 1200); p })()
}

plot_delta_rate <- function(data, fraction_label, y_max, y_breaks, y_lab, color_set, file_suffix, subset_rows = NULL) {
  color_vals <- c(color_set["Chow"], color_set["Adenine"])
  fill_vals <- alpha(color_vals, 0.35)
  
  plot_data <- data |> filter(Fraction == fraction_label)
  if (!is.null(subset_rows)) {
    plot_data <- plot_data[-subset_rows, ]
  }
  
  x_lab_expr <- expression(bold(Delta*G[ATP]~'('*kcal*'/'*mol[ATP]*')'))
  if (grepl("MP", file_suffix)) {
    x_lab_expr <- expression(bold(Delta*G[ATP]~'('*kcal*'/'*mol[ATP]*')'))
    y_lab <- expression(bold(Delta*Psi*"'"[mt]*~'('*'577'*'/'*'552'*')'))
  }
  
  ggplot(plot_data, aes(x = deltaG, y = JO2, fill = Diet, color = Diet)) +
    theme_prism() +
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, xseq = c(-14.85, -12.65)) +
    geom_point(stat = 'summary', size = 3.5, shape = 21, stroke = 1.25, alpha = 100) +
    geom_beeswarm(size = 3, stroke = 1.5, show.legend = FALSE, cex = 1, pch = 21) +
    scale_color_manual(values = color_vals) +
    scale_fill_manual(values = fill_vals) +
    coord_cartesian(ylim = c(0, y_max), xlim = c(-15, -12.5), clip = 'off') +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, y_max, y_breaks)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(-15, -12.5, 0.5)) +
    labs(y = y_lab, x = x_lab_expr) +
    theme(
      axis.title.x = element_text(size = 22),
      legend.text = element_text(size = 18),
      axis.title.y = element_text(size = 24),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18)
    ) +
    ggpmisc::stat_poly_eq(
      size = 6, formula = y ~ x,
      aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\"; \"*")),
      parse = TRUE, coef.digits = 3, f.digits = 3, p.digits = 3, rr.digits = 3, small.p = TRUE, vjust = -0.15
    ) +
    ggtitle(NULL) |>
    (\(p) { ggsave(paste0(OUTPUT_DIR, "/Fig ", file_suffix, ".svg"), p, width = 9, height = 6, dpi = 1200); p })()
}

plot_membrane_potential <- function(data, fraction_label, y_max, y_min, y_breaks, color_set, file_suffix) {
  color_vals <- c(color_set[fraction_label], FRACTION_DARK_COLORS[fraction_label])
  fill_vals <- alpha(color_vals, 0.35)
  y_pos_stats <- y_max * 0.9375
  if (fraction_label %in% c("DT", "Glom")) { y_pos_stats <- 0.25 }
  if (fraction_label %in% c("DT", "Glom")) { y_min_seg <- 0.1275; y_text <- 0.1225 }
  if (fraction_label %in% c("PT", "Mito")) { y_min_seg <- 0.07; y_text <- 0.06 }
  if (fraction_label == "Mito") { x_end_seg <- 5; x_vline_end <- 6.5 } else { x_end_seg <- 5; x_vline_end <- 6.5 }
  
  p <- data |>
    filter(Fraction == fraction_label & name != 'Background') |>
    ggplot(aes(x = name, y = value, color = Diet, fill = Diet)) +
    theme_prism() +
    stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
    stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
    geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
    scale_color_manual(values = color_vals) +
    scale_fill_manual(values = alpha(color_vals, 0.35)) +
    coord_cartesian(ylim = c(y_min, y_max), clip = 'off') +
    scale_y_continuous(expand = c(0, 0), breaks = seq(y_min, y_max, y_breaks)) +
    labs(y = expression(bold(Delta*Psi*"'"[mt]*~'('*'577'*'/'*'552'*')'))) +
    theme(
      axis.title.x = element_blank(),
      legend.text = element_text(size = 18),
      axis.title.y = element_text(size = 24),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 18)
    ) +
    stat_pwc(
      p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE,
      label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}",
      y.position = y_pos_stats, show.legend = FALSE, method = 't_test',
      size = 1, tip.length = 0, label.size = 7
    ) +
    ggtitle(NULL) +
    geom_segment(x = 2, xend = x_end_seg, y = y_min_seg, yend = y_min_seg, size = 1, color = "black") +
    annotate("text", label = '+ PCr', x = 3.5, y = y_text, fontface = 2, size = 6) +
    geom_vline(xintercept = seq(1.5, x_vline_end, 1), linewidth = 1, linetype = "longdash", alpha = 0.25)
  
  ggsave(paste0(OUTPUT_DIR, "/Fig ", file_suffix, ".svg"), p, width = 14, height = 5, dpi = 1200)
  return(p)
}


# ----------------------------------------------------------------------
# 2. DATA IMPORT AND INITIAL PROCESSING
# ----------------------------------------------------------------------

# --- J_O2 Data Processing ---
jo2_long <- read_and_clean_data(
  file_name = "AdenineJO2.xlsx",
  sheet_name = "Individual",
  cols_to_pivot = 7:10
)

# --- J_ATP Data Processing ---
jatp_long <- read_and_clean_data(
  file_name = "AdenineJO2.xlsx",
  sheet_name = "JATP",
  cols_to_pivot = 7:10
)
jatp_long$Fraction <- factor(jatp_long$Fraction, levels = c("PT", "DT", "Glom"))

# --- J_ATP Mito Data Processing ---
jatpm_long <- read_and_clean_data(
  file_name = "AdenineJO2.xlsx",
  sheet_name = "MitoJATP",
  cols_to_pivot = 5:7
)

# --- Weights Data Processing (Cross-sectional) ---
weights <- read_xlsx(paste0(DATA_BASE_PATH, "AdenineWeight.xlsx"), sheet = 'Sheet1') |>
  na.omit() |>
  mutate(
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    Mouse = factor(Mouse, levels = unique(Mouse))
  )
rstatix::t_test(weights, Kidney ~ Diet, var.equal = FALSE)

# --- Weights Data Processing (Longitudinal) ---
bodyweights <- read_xlsx(paste0(DATA_BASE_PATH, "AdenineWeight.xlsx"), sheet = 'Sheet2') |>
  na.omit() |>
  mutate(
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    Mouse = factor(Mouse, levels = unique(Mouse)),
    Time = factor(Time, levels = unique(Time))
  )
rstatix::t_test(bodyweights, Weight ~ Diet, var.equal = FALSE)

# --- CK Clamp Data Processing ---
CKdata <- read_xlsx(paste0(DATA_BASE_PATH, "AdenineJO2.xlsx"), sheet = "Processed CK", col_names = TRUE) |>
  na.omit() |>
  rename(
    '-54.15 kJ/mol' = 'CytC', '-59.00 kJ/mol' = 'PCr1',
    '-60.36 kJ/mol' = 'PCr2', '-61.21 kJ/mol' = 'PCr3'
  ) |>
  mutate(
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    Fraction = factor(Fraction, levels = c('PT', 'DT', 'Glom', 'Mito'))
  )

CKdata_long <- CKdata |>
  pivot_longer(all_of(6:16)) |>
  mutate(name = factor(name, levels = unique(name)))

# --- DeltaG Data Processing (JO2 vs DeltaG) ---
DeltaG_long <- CKdata |>
  rename(
    '-12.94' = '-54.15 kJ/mol', '-14.10' = '-59.00 kJ/mol',
    '-14.43' = '-60.36 kJ/mol', '-14.63' = '-61.21 kJ/mol'
  ) |>
  pivot_longer(all_of(5:8), names_to = 'deltaG', values_to = 'JO2') |>
  mutate(
    deltaG = as.numeric(deltaG),
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    Fraction = factor(Fraction, levels = c('PT', 'DT', 'Glom', 'Mito'))
  )

# --- Membrane Potential Data Processing (MP) ---
MPdata <- read_xlsx(paste0(DATA_BASE_PATH, "AdenineJO2.xlsx"), sheet = "Membrane Potential", col_names = TRUE) |>
  na.omit() |>
  rename(
    '-54.15 kJ/mol' = 'CK/PCr/ATP', '-59.00 kJ/mol' = 'PCr1',
    '-60.36 kJ/mol' = 'PCr2', '-61.21 kJ/mol' = 'PCr3'
  ) |>
  mutate(
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    Fraction = factor(Fraction, levels = c('PT', 'DT', 'Glom', 'Mito'))
  )

MPdata_long <- MPdata |>
  pivot_longer(all_of(4:11)) |>
  mutate(name = factor(name, levels = unique(name)))

# --- DeltaPsi Data Processing (MMP vs DeltaG) ---
DeltaPsi_long <- MPdata |>
  rename(
    '-12.94' = '-54.15 kJ/mol', '-14.10' = '-59.00 kJ/mol',
    '-14.43' = '-60.36 kJ/mol', '-14.63' = '-61.21 kJ/mol'
  ) |>
  pivot_longer(all_of(4:7), names_to = 'deltaG', values_to = 'MMP') |>
  mutate(
    deltaG = as.numeric(deltaG),
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    Fraction = factor(Fraction, levels = c('PT', 'DT', 'Glom', 'Mito'))
  )

# --- JO2 Per MMP Data Processing ---
JO2MPdata <- read_xlsx(paste0(DATA_BASE_PATH, "AdenineJO2.xlsx"), sheet = "JO2MMP", col_names = TRUE) |>
  na.omit() |>
  rename('-59.00 kJ/mol' = 'PCr1', '-60.36 kJ/mol' = 'PCr2', '-61.21 kJ/mol' = 'PCr3')

JO2MPdata_long <- pivot_longer(JO2MPdata, 4:6)

mergedMPJO2data_long <- merge(JO2MPdata_long, MPdata_long, by = c('Subject', 'Diet', 'Fraction', 'name')) |>
  rename(JO2 = value.x, MMP = value.y) |>
  mutate(
    Diet = factor(Diet, levels = c("Chow", "Adenine")),
    name = factor(name, levels = unique(name)),
    Fraction = factor(Fraction, levels = c('PT', 'DT', 'Glom', 'Mito'))
  )

summary_mpjo2 <- mergedMPJO2data_long |>
  group_by(Fraction, Diet, name) |>
  summarise(
    JO2_mean = mean(JO2), JO2_SD = sd(JO2),
    MMP_mean = mean(MMP), MMP_SD = sd(MMP)
  )

# --- Human Data Processing ---
jo2human <- read_xlsx(paste0(DATA_BASE_PATH, "AdenineJO2.xlsx"), sheet = "Human") |>
  na.omit() |>
  mutate(sample = factor(c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)))

jo2human_long <- pivot_longer(jo2human, 5:9, names_to = "State", values_to = "Rate") |>
  mutate(
    State = factor(State, levels = unique(State)),
    Fraction = factor(Fraction, levels = unique(Fraction)),
    Sample = factor(Sample, levels = unique(Sample))
  )


# ----------------------------------------------------------------------
# 3. GENERATE FIGURES (J_O2)
# ----------------------------------------------------------------------

jo2_70 <- plot_oxygen_or_atp_rate(jo2_long, "70", 40000, 5000,
                                  y_lab = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")")),
                                  color_set = FRACTION_COLORS, file_suffix = "2K")

jo2_40_1 <- plot_oxygen_or_atp_rate(jo2_long, "40-1", 7000, 1000,
                                    y_lab = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")")),
                                    color_set = FRACTION_COLORS, file_suffix = "2L")

jo2_40_2 <- plot_oxygen_or_atp_rate(jo2_long, "40-2", 7000, 1000,
                                    y_lab = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")")),
                                    color_set = FRACTION_COLORS, file_suffix = "2M")

jo2_mito <- plot_oxygen_or_atp_rate(jo2_long, "Mito", 2500, 500,
                                    y_lab = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")")),
                                    color_set = FRACTION_COLORS, file_suffix = "2J")

# Zoomed plots (not saved here as they were commented out, but original logic is kept)
from <- c(xmin = 0.25, xmax = 3.75, ymin = 23.5, ymax = 500)
to <- c(xmin = 0.35, xmax = 3.65, ymin = 2000, ymax = 9000)
zoomed70 <- jo2_70 + ggmagnify::geom_magnify(from = from, to = to, axes = 'xy', shadow = TRUE, linewidth = 1, aspect = 'free')
# ggsave('SVGs/zoomed70.svg', zoomed70, width = 8)

from <- c(xmin = 0.25, xmax = 3.75, ymin = 18, ymax = 400)
to <- c(xmin = 0.35, xmax = 3.65, ymin = 400, ymax = 1600)
# zoomed40 <- jo2_40_1 + ggmagnify::geom_magnify(from = from, to = to, axes = 'xy', shadow = T, linewidth = 1, aspect = 'free')
# ggsave('SVGs/zoomed40.svg', zoomed40, width = 8)


# ----------------------------------------------------------------------
# 4. GENERATE FIGURES (J_ATP)
# ----------------------------------------------------------------------

jatp_70 <- plot_oxygen_or_atp_rate(jatp_long, "PT", 40000, 5000,
                                   y_lab = expression(bold(bolditalic(J)*ATP~"("*pmol[ATP]*"/"*sec*"/"*mg[protein]*")")),
                                   color_set = FRACTION_COLORS, file_suffix = "2O")

jatp_40_1 <- plot_oxygen_or_atp_rate(jatp_long, "DT", 30000, 5000,
                                     y_lab = expression(bold(bolditalic(J)*ATP~"("*pmol[ATP]*"/"*sec*"/"*mg[protein]*")")),
                                     color_set = FRACTION_COLORS, file_suffix = "2P")

jatp_40_2 <- plot_oxygen_or_atp_rate(jatp_long, "Glom", 20000, 2500,
                                     y_lab = expression(bold(bolditalic(J)*ATP~"("*pmol[ATP]*"/"*sec*"/"*mg[protein]*")")),
                                     color_set = FRACTION_COLORS, file_suffix = "2Q")

jatp_mito <- plot_oxygen_or_atp_rate(jatpm_long, "Mito", 10000, 2000,
                                     y_lab = expression(bold(bolditalic(J)*ATP~"("*pmol[ATP]*"/"*sec*"/"*mg[protein]*")")),
                                     color_set = FRACTION_COLORS, file_suffix = "2N")


# ----------------------------------------------------------------------
# 5. GENERATE FIGURES (WEIGHTS)
# ----------------------------------------------------------------------

body_weight <- plot_weight(weights, "Body", 35, 5, 'Body Mass (g)', "2B")
kidney_weight <- plot_weight(weights, "Kidney", 400, 50, 'Kidney Weight (mg)', "2C")
liver_weight <- plot_weight(weights, "Liver", 1500, 250, 'Liver Weight (mg)', "2D")

long_body_weight <- bodyweights |>
  ggplot(aes(x = Time, y = Weight, color = Diet, fill = Diet, group = Diet)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, width = 0.25, show.legend = FALSE) +
  geom_path(stat = "summary", size = 1.25, show.legend = TRUE) +
  stat_summary(geom = 'point', size = 5, pch = 21, stroke = 1.75) +
  scale_color_manual(values = c(CHOW_COLOR, ADENINE_COLOR)) +
  scale_fill_manual(values = c("grey90", "grey10")) +
  coord_cartesian(ylim = c(0, 35), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 35, 5)) +
  labs(y = 'Body Mass (g)') +
  theme(
    axis.title.x = element_blank(),
    legend.text = element_text(size = 18),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20)
  ) +
  stat_pwc(
    p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = TRUE,
    label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}",
    y.position = 34 * 0.95, show.legend = FALSE, method = 't_test',
    size = 1, tip.length = 0, label.size = 7
  )
ggsave(paste0(OUTPUT_DIR, '/BodyWeightLong.svg'), long_body_weight, width = 10, height = 6)


# ----------------------------------------------------------------------
# 6. CALCULATIONS (Optional for display, kept for replication)
# ----------------------------------------------------------------------

# NOTE: The original script calculated percent changes and stored them in global environment.
# Since these aren't used in a plot, the block is kept for reference but could be removed
# if only plotting code is desired for the repository.

# for(i in unique(jo2_long$State)){
#   for(j in unique(jo2_long$Fraction)){
#     subsetted <- subset(subset(jo2_long, State == paste(i)), Fraction == paste(j))
#     descriptives <- psych::describeBy(subsetted$Rate, subsetted$Diet)
#     assign(paste(i, j, "desc", sep = "_"), descriptives)
#   }
# }

# 1-PGMDS_70_desc$Adenine[3]/PGMDS_70_desc$Chow[3]
# 1-`PGMDS_40-1_desc`$Adenine[3]/`PGMDS_40-1_desc`$Chow[3]
# 1-`PGMDS_40-2_desc`$Adenine[3]/`PGMDS_40-2_desc`$Chow[3]

# 1-PGMD_70_desc$Adenine[3]/PGMD_70_desc$Chow[3]
# 1-`PGMD_40-1_desc`$Adenine[3]/`PGMD_40-1_desc`$Chow[3]
# 1-`PGMD_40-2_desc`$Adenine[3]/`PGMD_40-2_desc`$Chow[3]


# ----------------------------------------------------------------------
# 7. GENERATE FIGURES (CK and JO2/DeltaG)
# ----------------------------------------------------------------------

# --- CK Plots (JO2 Rate at different DeltaG) ---
CK_plot_p <- CKdata_long |>
  filter(value < 30000 & Fraction == "PT" & !(name %in% c("Background", "CK/PCr/ATP", "FCCP", "PM", "PMG"))) |>
  ggplot(aes(x = name, y = value, color = Diet, fill = Diet)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
  scale_color_manual(values = c("#51A0E1", "#154C7A")) +
  scale_fill_manual(values = alpha(c("#51A0E1", "#154C7A"), 0.35)) +
  coord_cartesian(ylim = c(0, 30000), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 30000, 5000)) +
  labs(y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[Protein]*")"))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 18)) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}", y.position = 30000 * 0.9, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7) +
  ggtitle(NULL)

CK_text_p <- CK_plot_p +
  geom_segment(x = 2, xend = 5, y = -4000, yend = -4000, size = 1, color = "black") +
  annotate("text", label = '+ PCr', x = 3.5, y = -5000, fontface = 2, size = 6) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), linewidth = 1, linetype = "longdash", alpha = 0.25)
ggsave(paste0(OUTPUT_DIR, '/Fig 4C.svg'), CK_text_p, width = 14, dpi = 1200, height = 5)

CK_plot_d <- CKdata_long |>
  filter(value < 15000 & Fraction == "DT" & !(name %in% c("Background", "CK/PCr/ATP", "FCCP", "PM", "PMG"))) |>
  ggplot(aes(x = name, y = value, color = Diet, fill = Diet)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
  scale_color_manual(values = c("#C76BB3", "#853273")) +
  scale_fill_manual(values = alpha(c("#C76BB3", "#853273"), 0.35)) +
  coord_cartesian(ylim = c(0, 15000), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 15000, 2500)) +
  labs(y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[Protein]*")"))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 18)) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}", y.position = 15000 * 0.9, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7) +
  ggtitle(NULL)

CK_text_d <- CK_plot_d +
  geom_segment(x = 2, xend = 5, y = -5500, yend = -5500, size = 1, color = "black") +
  annotate("text", label = '+ PCr', x = 3.5, y = -6500, fontface = 2, size = 6) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), linewidth = 1, linetype = "longdash", alpha = 0.25)
ggsave(paste0(OUTPUT_DIR, '/Fig 4E.svg'), CK_text_d, width = 14, dpi = 1200, height = 5)

CK_plot_g <- CKdata_long |>
  filter(Fraction == "Glom" & !(name %in% c("Background", "CK/PCr/ATP", "FCCP", "PM", "PMG"))) |>
  ggplot(aes(x = name, y = value, color = Diet, fill = Diet)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
  scale_color_manual(values = c("#12A597", "#084942")) +
  scale_fill_manual(values = alpha(c("#12A597", "#084942"), 0.35)) +
  coord_cartesian(ylim = c(0, 15000), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 15000, 2500)) +
  labs(y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[Protein]*")"))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 18)) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}", y.position = 15000 * 0.9, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7) +
  ggtitle(NULL)

CK_text_g <- CK_plot_g +
  geom_segment(x = 2, xend = 5, y = -5500, yend = -5500, size = 1, color = "black") +
  annotate("text", label = '+ PCr', x = 3.5, y = -6500, fontface = 2, size = 6) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), linewidth = 1, linetype = "longdash", alpha = 0.25)
ggsave(paste0(OUTPUT_DIR, '/Fig 4G.svg'), CK_text_g, width = 14, dpi = 1200, height = 5)

CK_plot_Mito <- CKdata_long |>
  filter(Fraction == "Mito" & !(name %in% c("Background", "CK/PCr/ATP", "FCCP", "PM", "PMG"))) |>
  ggplot(aes(x = name, y = value, color = Diet, fill = Diet)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
  scale_color_manual(values = c("#AD725C", "#36221B")) +
  scale_fill_manual(values = alpha(c("#AD725C", "#36221B"), 0.35)) +
  coord_cartesian(ylim = c(0, 7000), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 7000, 1000)) +
  labs(y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[Protein]*")"))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 18)) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p.adj < 0.001, '< 0.001', sprintf('%.3f',round(p.adj,3)))}", y.position = 7000 * 0.9, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7) +
  ggtitle(NULL)

CK_text_Mito <- CK_plot_Mito +
  geom_segment(x = 2, xend = 5, y = -3100, yend = -3100, size = 1, color = "black") +
  annotate("text", label = '+ PCr', x = 3.5, y = -3600, fontface = 2, size = 6) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), linewidth = 1, linetype = "longdash", alpha = 0.25)
ggsave(paste0(OUTPUT_DIR, '/Fig 4A.svg'), CK_text_Mito, width = 14, dpi = 1200, height = 5)

# --- JO2 vs DeltaG Plots (Haldane Plots) ---
y_lab_jo2 <- expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[Protein]*")"))

DeltaG_plot_PT <- plot_delta_rate(DeltaG_long, "PT", 30000, 5000, y_lab_jo2, FRACTION_COLORS, "4D", subset_rows = which(DeltaG_long$JO2 > 26000 & DeltaG_long$Fraction == "PT"))
DeltaG_plot_DT <- plot_delta_rate(DeltaG_long, "DT", 15000, 2500, y_lab_jo2, FRACTION_COLORS, "4F", subset_rows = 9:12)
DeltaG_plot_G <- plot_delta_rate(DeltaG_long, "Glom", 15000, 2500, y_lab_jo2, FRACTION_COLORS, "4H")
DeltaG_plot_Mito <- plot_delta_rate(DeltaG_long, "Mito", 7000, 1000, y_lab_jo2, FRACTION_COLORS, "4B")


# ----------------------------------------------------------------------
# 8. GENERATE FIGURES (Membrane Potential - MP)
# ----------------------------------------------------------------------

# --- MP Plots (MMP Rate at different DeltaG) ---
MP_text_mito <- plot_membrane_potential(MPdata_long, "Mito", 0.4, 0.15, 0.05, FRACTION_COLORS, "5A")
MP_text_p <- plot_membrane_potential(MPdata_long, "PT", 0.4, 0.15, 0.05, FRACTION_COLORS, "5C")
MP_text_d <- plot_membrane_potential(MPdata_long, "DT", 0.26, 0.16, 0.02, FRACTION_COLORS, "5E")
MP_text_g <- plot_membrane_potential(MPdata_long, "Glom", 0.26, 0.16, 0.02, FRACTION_COLORS, "5G")

# --- MMP vs DeltaG Plots (Haldane Plots) ---
y_lab_mmp <- expression(bold(Delta*Psi*"'"[mt]*~'('*'577'*'/'*'552'*')'))

DeltaPsi_plot_Mito <- plot_delta_rate(DeltaPsi_long, "Mito", 0.4, 0.05, y_lab_mmp, FRACTION_COLORS, "5B") |>
  (\(p) { p + labs(y = y_lab_mmp, x = expression(bold(Delta*G[ATP]~'('*kcal*'/'*mol[ATP]*')'))) + coord_cartesian(ylim = c(0.15, 0.4), xlim = c(-15, -12.5), clip = 'off') + scale_y_continuous(expand = c(0,0), breaks = seq(0.15, 0.4, 0.05))})()
ggsave(paste0(OUTPUT_DIR, '/Fig 5B.svg'), DeltaPsi_plot_Mito, width = 9, dpi = 1200, height = 6)

DeltaPsi_plot_PT <- plot_delta_rate(DeltaPsi_long, "PT", 0.4, 0.05, y_lab_mmp, FRACTION_COLORS, "5D") |>
  (\(p) { p + labs(y = y_lab_mmp, x = expression(bold(Delta*G[ATP]~'('*kcal*'/'*mol[ATP]*')'))) + coord_cartesian(ylim = c(0.15, 0.4), xlim = c(-15, -12.5), clip = 'off') + scale_y_continuous(expand = c(0,0), breaks = seq(0.15, 0.4, 0.05))})()
ggsave(paste0(OUTPUT_DIR, '/Fig 5D.svg'), DeltaPsi_plot_PT, width = 9, dpi = 1200, height = 6)

DeltaPsi_plot_DT <- plot_delta_rate(DeltaPsi_long, "DT", 0.26, 0.02, y_lab_mmp, FRACTION_COLORS, "5F") |>
  (\(p) { p + labs(y = y_lab_mmp, x = expression(bold(Delta*G[ATP]~'('*kcal*'/'*mol[ATP]*')'))) + coord_cartesian(ylim = c(0.16, 0.26), xlim = c(-15, -12.5), clip = 'off') + scale_y_continuous(expand = c(0,0), breaks = seq(0.16, 0.26, 0.02))})()
ggsave(paste0(OUTPUT_DIR, '/Fig 5F.svg'), DeltaPsi_plot_DT, width = 9, dpi = 1200, height = 6)

DeltaPsi_plot_G <- plot_delta_rate(DeltaPsi_long, "Glom", 0.26, 0.02, y_lab_mmp, FRACTION_COLORS, "5H") |>
  (\(p) { p + labs(y = y_lab_mmp, x = expression(bold(Delta*G[ATP]~'('*kcal*'/'*mol[ATP]*')'))) + coord_cartesian(ylim = c(0.16, 0.26), xlim = c(-15, -12.5), clip = 'off') + scale_y_continuous(expand = c(0,0), breaks = seq(0.16, 0.26, 0.02))})()
ggsave(paste0(OUTPUT_DIR, '/Fig 5H.svg'), DeltaPsi_plot_G, width = 9, dpi = 1200, height = 6)


# ----------------------------------------------------------------------
# 9. COMPARE SLOPES (ETC Conductance and Coupling Coefficient)
# ----------------------------------------------------------------------

## JO2/Delta G Slopes (ETC Conductance)
JO2_slopes <- data.frame()

for(k in unique(DeltaG_long$Subject)){
  for(l in unique(DeltaG_long$Chamber)){
    
    individ <- subset(subset(DeltaG_long, Subject %in% paste(k)), Chamber %in% paste(l))
    
    slope <- ifelse(is.na(individ$deltaG[1]), NA, lm(JO2~deltaG, individ))
    
    df <- data.frame('Subject' = individ$Subject[1],
                     'Chamber' = individ$Chamber[1],
                     'Fraction' = individ$Fraction[1],
                     'Diet' = individ$Diet[1],
                     'Intercept' = slope[[1]][1],
                     'Slope' = slope[[1]][2])
    
    JO2_slopes <- rbind(JO2_slopes, df)
  }
}

JO2_slopes <- na.omit(JO2_slopes)
JO2_slopes$Fraction <- factor(JO2_slopes$Fraction, levels = c("Mito", "PT", "DT", "Glom"))
JO2_slopes$Group <- factor(paste(JO2_slopes$Fraction, JO2_slopes$Diet),
                           levels = c("Mito Chow", "Mito Adenine", "PT Chow", "PT Adenine",
                                      "DT Chow", "DT Adenine", "Glom Chow", "Glom Adenine"))

anova_res <- aov(Slope ~ Fraction*Diet, JO2_slopes[-c(3,7),])
summary(anova_res)

JO2_slopes_plot <- ggplot(data = JO2_slopes[-c(3,7),], aes(x = Fraction, y = Slope, color = Group, fill = Group)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
  scale_color_manual(values = c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942")) +
  scale_fill_manual(values = alpha(c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942"), 0.35)) +
  coord_cartesian(ylim = c(0, 8000), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8000, 2000)) +
  labs(y = expression(bold(ETC~Conductance~'('*frac(
    pmol[O['2']]*'/'*sec*'/'*mg[protein],
    bolditalic(d)*Delta*G[ATP])*')'))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 22), legend.position = 'bottom') +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linewidth = 1, linetype = "longdash", alpha = 0.25) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p < 0.001, '< 0.001', sprintf('%.3f',round(p,3)))}", y.position = 8000 * 0.92, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7)
ggsave(paste0(OUTPUT_DIR, '/Fig 4I.svg'), JO2_slopes_plot, width = 10, dpi = 1200, height = 6)

JO2_intercept_plot <- ggplot(data = JO2_slopes[-c(3,7),], aes(x = Fraction, y = Intercept, color = Group, fill = Group)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 3, stroke = 1.5, cex = 2, pch = 21) +
  scale_color_manual(values = c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942")) +
  scale_fill_manual(values = alpha(c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942"), 0.35)) +
  coord_cartesian(ylim = c(0, 125000), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 125000, 25000)) +
  labs(y = expression(bold('Zero-load'~bolditalic(J)*O['2']~'('*pmol[O['2']]*'/'*sec*'/'*mg[protein]*')'))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 22), legend.position = 'bottom') +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linewidth = 1, linetype = "longdash", alpha = 0.25) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p < 0.001, '< 0.001', sprintf('%.3f',round(p,3)))}", y.position = 125000 * 0.92, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7)
ggsave(paste0(OUTPUT_DIR, '/Fig 4J.svg'), JO2_intercept_plot, width = 10, dpi = 1200, height = 6)

## MMP/Delta G Slopes (Coupling Coefficient)
MMP_slopes <- data.frame()

for(k in unique(DeltaPsi_long$Subject)){
  
  individ <- subset(DeltaPsi_long, Subject %in% paste(k))
  
  slope <- ifelse(is.na(individ$MMP[1]), NA, lm(MMP~deltaG, individ))
  
  df <- data.frame('Subject' = individ$Subject[1],
                   'Fraction' = individ$Fraction[1],
                   'Diet' = individ$Diet[1],
                   'Intercept' = slope[[1]][1],
                   'Slope' = slope[[1]][2])
  
  MMP_slopes <- rbind(MMP_slopes, df)
}

MMP_slopes <- na.omit(MMP_slopes)
MMP_slopes$Fraction <- factor(MMP_slopes$Fraction, levels = c("Mito", "PT", "DT", "Glom"))
MMP_slopes$Group <- factor(paste(MMP_slopes$Fraction, MMP_slopes$Diet),
                           levels = c("Mito Chow", "Mito Adenine", "PT Chow", "PT Adenine",
                                      "DT Chow", "DT Adenine", "Glom Chow", "Glom Adenine"))

MMP_slopes_plot <- ggplot(data = MMP_slopes, aes(x = Fraction, y = Slope, color = Group, fill = Group)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1.25, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.75, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 5, stroke = 1.75, cex = 2, pch = 21) +
  scale_color_manual(values = c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942")) +
  coord_cartesian(ylim = c(-0.02, 0.08), clip = 'off') +
  scale_fill_manual(values = alpha(c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942"), 0.35)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-0.02, 0.08, 0.02)) +
  labs(y = expression(bold(Mitochondrial~Coupling~Coefficient~'('*frac(bolditalic(d)*Delta*Psi*"'"[mt],bolditalic(d)*Delta*G[ATP])*')'))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 26), axis.text.y = element_text(size = 20), legend.position = 'none') +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linewidth = 1, linetype = "longdash", alpha = 0.25) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p < 0.001, '< 0.001', sprintf('%.3f',round(p,3)))}", y.position = 0.08 * 0.9, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7.5)
ggsave(paste0(OUTPUT_DIR, '/Fig 5I.svg'), MMP_slopes_plot, width = 10, dpi = 1200, height = 8)

MMP_intercept_plot <- ggplot(data = MMP_slopes, aes(x = Fraction, y = Intercept, color = Group, fill = Group)) +
  theme_prism() +
  stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1.25, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fun = 'mean', size = 0.75, position = position_dodge(0.75), width = 0.5, show.legend = FALSE) +
  geom_beeswarm(dodge.width = 0.75, size = 5, stroke = 1.75, cex = 2, pch = 21) +
  scale_color_manual(values = c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942")) +
  scale_fill_manual(values = alpha(c("#AD725C", "#36221B", "#51A0E1", "#154C7A", "#C76BB3", "#853273", "#12A597", "#084942"), 0.35)) +
  coord_cartesian(ylim = c(0, 1.5), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.5, 0.25)) +
  labs(y = expression(bold('Zero-load'~Delta*Psi*"'"[mt]))) +
  theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 32), axis.text.x = element_text(size = 26), axis.text.y = element_text(size = 20), legend.position = 'none') +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linewidth = 1, linetype = "longdash", alpha = 0.25) +
  stat_pwc(p.adjust.method = 'holm', p.adjust.by = 'panel', hide.ns = FALSE, label = "{ifelse(p < 0.001, '< 0.001', sprintf('%.3f',round(p,3)))}", y.position = 1.5 * 0.92, show.legend = FALSE, method = 't_test', size = 1, tip.length = 0, label.size = 7.5)
ggsave(paste0(OUTPUT_DIR, '/Fig 5J.svg'), MMP_intercept_plot, width = 10, dpi = 1200, height = 8)


# ----------------------------------------------------------------------
# 10. GENERATE FIGURES (JO2 per MMP)
# ----------------------------------------------------------------------

MPJO2data_long_plot_PT_Chow <- summary_mpjo2 |>
  filter(Diet == "Chow" & Fraction == "PT") |>
  ggplot(aes(x = MMP_mean, y = JO2_mean, fill = Diet, color = Diet)) +
  theme_prism() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Chow" & Fraction == "PT"), aes(xmin = MMP_mean - MMP_SD, xmax = MMP_mean + MMP_SD)) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Chow" & Fraction == "PT"), aes(ymin = JO2_mean - JO2_SD, ymax = JO2_mean + JO2_SD)) +
  geom_point(stat = 'summary', size = 3.5, shape = 21, stroke = 1.25, alpha = 100) +
  geom_beeswarm(data = mergedMPJO2data_long |> filter(Diet == "Chow" & Fraction == "PT"), aes(x = MMP, y = JO2, fill = Diet, color = Diet), size = 3, stroke = 1.5, show.legend = FALSE, cex = 1.5, pch = 21) +
  scale_color_manual(values = c("#51A0E1")) +
  scale_fill_manual(values = alpha(c("#51A0E1"), 0.35)) +
  coord_cartesian(ylim = c(4000, 16000), xlim = c(0.2, 0.3), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(4000, 16000, 2000)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0.2, 0.3, 0.02)) +
  labs(x = expression(bold(Delta*Psi*"'"[mt]*'('*'577'*'/'*'552'*')')), y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")"))) +
  theme(axis.title.x = element_text(size = 18), legend.text = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14)) +
  ggpmisc::stat_poly_eq(size = 6, formula = y ~ x, aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\"; \"*")),
                        parse = TRUE, coef.digits = 3, f.digits = 3, p.digits = 3, rr.digits = 3, small.p = TRUE, vjust = -0.5) +
  ggtitle("Proximal Tubules")
ggsave(paste0(OUTPUT_DIR, '/Fig S3.svg'), MPJO2data_long_plot_PT_Chow, width = 9, dpi = 1200, height = 6)

MPJO2data_long_plot_PT_AD <- summary_mpjo2 |>
  filter(Diet == "Adenine" & Fraction == "PT") |>
  ggplot(aes(x = MMP_mean, y = JO2_mean, fill = Diet, color = Diet)) +
  theme_prism() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Adenine" & Fraction == "PT"), aes(xmin = MMP_mean - MMP_SD, xmax = MMP_mean + MMP_SD)) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Adenine" & Fraction == "PT"), aes(ymin = JO2_mean - JO2_SD, ymax = JO2_mean + JO2_SD)) +
  geom_point(stat = 'summary', size = 3.5, shape = 21, stroke = 1.25, alpha = 100) +
  geom_beeswarm(data = mergedMPJO2data_long |> filter(Diet == "Adenine" & Fraction == "PT"), aes(x = MMP, y = JO2, fill = Diet, color = Diet), size = 3, stroke = 1.5, show.legend = FALSE, cex = 1.5, pch = 21) +
  scale_color_manual(values = c("#154C7A")) +
  scale_fill_manual(values = alpha(c("#154C7A"), 0.35)) +
  coord_cartesian(ylim = c(4500, 7000), xlim = c(0.187, 0.192), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(4000, 7000, 500)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0.187, 0.192, 0.001)) +
  labs(x = expression(bold(Delta*Psi*"'"[mt]*'('*'577'*'/'*'552'*')')), y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")"))) +
  theme(axis.title.x = element_text(size = 18), legend.text = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14)) +
  ggpmisc::stat_poly_eq(size = 6, formula = y ~ x, aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\"; \"*")),
                        parse = TRUE, coef.digits = 3, f.digits = 3, p.digits = 3, rr.digits = 3, small.p = TRUE, vjust = -0.5) +
  ggtitle("Proximal Tubules")
ggsave(paste0(OUTPUT_DIR, '/Fig S4.svg'), MPJO2data_long_plot_PT_AD, width = 9, dpi = 1200, height = 6)

MPJO2data_long_plot_Mito_Chow <- summary_mpjo2 |>
  filter(Diet == "Chow" & Fraction == "Mito") |>
  ggplot(aes(x = MMP_mean, y = JO2_mean, fill = Diet, color = Diet)) +
  theme_prism() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Chow" & Fraction == "Mito"), aes(xmin = MMP_mean - MMP_SD, xmax = MMP_mean + MMP_SD)) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Chow" & Fraction == "Mito"), aes(ymin = JO2_mean - JO2_SD, ymax = JO2_mean + JO2_SD)) +
  geom_point(stat = 'summary', size = 3.5, shape = 21, stroke = 1.25, alpha = 100) +
  geom_beeswarm(data = mergedMPJO2data_long |> filter(Diet == "Chow" & Fraction == "Mito"), aes(x = MMP, y = JO2, fill = Diet, color = Diet), size = 3, stroke = 1.5, show.legend = FALSE, cex = 1, pch = 21) +
  scale_color_manual(values = c("#AD725C")) +
  scale_fill_manual(values = alpha(c("#AD725C"), 0.35)) +
  coord_cartesian(ylim = c(1000, 4500), xlim = c(0.27, 0.34), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5000, 500)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0.27, 0.34, 0.01)) +
  labs(x = expression(bold(Delta*Psi*"'"[mt]*'('*'577'*'/'*'552'*')')), y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")"))) +
  theme(axis.title.x = element_text(size = 18), legend.text = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14)) +
  ggpmisc::stat_poly_eq(size = 6, formula = y ~ x, aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\"; \"*")),
                        parse = TRUE, coef.digits = 3, f.digits = 3, p.digits = 3, rr.digits = 3, small.p = TRUE, vjust = -0.5) +
  ggtitle("Isolated Mitochondria")
ggsave(paste0(OUTPUT_DIR, '/Fig S1.svg'), MPJO2data_long_plot_Mito_Chow, width = 9, dpi = 1200, height = 6)

MPJO2data_long_plot_Mito_AD <- summary_mpjo2 |>
  filter(Diet == "Adenine" & Fraction == "Mito") |>
  ggplot(aes(x = MMP_mean, y = JO2_mean, fill = Diet, color = Diet)) +
  theme_prism() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Adenine" & Fraction == "Mito"), aes(xmin = MMP_mean - MMP_SD, xmax = MMP_mean + MMP_SD)) +
  geom_errorbar(data = summary_mpjo2 |> filter(Diet == "Adenine" & Fraction == "Mito"), aes(ymin = JO2_mean - JO2_SD, ymax = JO2_mean + JO2_SD)) +
  geom_point(stat = 'summary', size = 3.5, shape = 21, stroke = 1.25, alpha = 100) +
  geom_beeswarm(data = mergedMPJO2data_long |> filter(Diet == "Adenine" & Fraction == "Mito"), aes(x = MMP, y = JO2, fill = Diet, color = Diet), size = 3, stroke = 1.5, show.legend = FALSE, cex = 1, pch = 21) +
  scale_color_manual(values = c("#36221B")) +
  scale_fill_manual(values = alpha(c("#36221B"), 0.35)) +
  coord_cartesian(ylim = c(0, 4500), xlim = c(0.23, 0.29), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5000, 500)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0.23, 0.29, 0.01)) +
  labs(x = expression(bold(Delta*Psi*"'"[mt]*'('*'577'*'/'*'552'*')')), y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")"))) +
  theme(axis.title.x = element_text(size = 18), legend.text = element_text(size = 14), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14)) +
  ggpmisc::stat_poly_eq(size = 6, formula = y ~ x, aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\"; \"*")),
                        parse = TRUE, coef.digits = 3, f.digits = 3, p.digits = 3, rr.digits = 3, small.p = TRUE, vjust = -0.5) +
  ggtitle("Isolated Mitochondria")
ggsave(paste0(OUTPUT_DIR, '/Fig S2.svg'), MPJO2data_long_plot_Mito_AD, width = 9, dpi = 1200, height = 6)


# ----------------------------------------------------------------------
# 11. GENERATE FIGURES (Human Data)
# ----------------------------------------------------------------------

for(i in unique(jo2human_long$Fraction)){
  jo2human_plot <- jo2human_long |>
    filter(State != "Basal" & Fraction == paste(i)) |>
    ggplot(aes(x = State, y = Rate, color = Fraction, fill = Fraction)) +
    theme_prism() +
    geom_line(aes(group = sample), linewidth = 1, alpha = 0.15, show.legend = FALSE) +
    stat_summary(geom = 'errorbar', fun.data = "mean_se", size = 1, position = position_dodge(0.75), width = 0.25, show.legend = FALSE) +
    stat_summary(geom = "crossbar", fun = 'mean', size = 0.625, position = position_dodge(0.75), width = 0.5, show.legend = FALSE, width = 0.5) +
    geom_beeswarm(size = 4, cex = 2, show.legend = FALSE, stroke = 1.5, pch = 21) +
    scale_color_manual(values = ifelse(i == 'PT', "#51A0E1", ifelse(i == 'DT', "#C76BB3", "#12A597"))) +
    scale_fill_manual(values = alpha(ifelse(i == 'PT', "#51A0E1", ifelse(i == 'DT', "#C76BB3", "#12A597")), 0.35)) +
    scale_x_discrete(labels = c(expression(bold("PGM")), expression(bold("PGMD")), expression(bold("PGMS")), expression(bold(FCCP[Peak])))) +
    coord_cartesian(ylim = c(0, 3500), clip = "off") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 3500, 500)) +
    labs(y = expression(bold(bolditalic(J)*O['2']~"("*pmol[O['2']]*"/"*sec*"/"*mg[protein]*")"))) +
    geom_vline(xintercept = c(1.5, 2.5, 3.5), linewidth = 1, linetype = "longdash", alpha = 0.25) +
    theme(axis.title.x = element_blank(), legend.text = element_text(size = 18), axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 16)) +
    ggtitle(NULL)
  
  ggsave(paste0(OUTPUT_DIR, '/Human_JO2', i, '.svg'), jo2human_plot, width = 6, dpi = 1200, height = 5)
}