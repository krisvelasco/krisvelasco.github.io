# ==============================================================================
# Project: WCF Network Over Time — Descriptive Figures
# Author: Kristopher Velasco
#
# NOTE: This script was originally written in Stata (1a. WCF Descriptive
# Figures.do). For ease of replication alongside the other R-based scripts in
# this project, it has been converted to R. The results replicate those produced
# by the original Stata code.
#
# Overview:
#   1) Figure 1: Total conferences and total speakers (cumulative), by
#      global congresses and regional conferences
#   2) Figure 2: Participants by organizational type (three-year rolling avg)
#   3) Appendix Figure A1: Participants by organizational type, by region
#
# Date of original Stata version: December 17, 2023
# Date of R conversion: April 6, 2026
# ==============================================================================

# --- Load Required Libraries ---
library(tidyverse)
library(readxl)
library(cowplot)

# ==============================================================================
# SET FILE PATHS
# ==============================================================================
# Update this path to match your local directory
base_path <- "/Users/kv7379/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network"
data_file <- file.path(base_path, "Datasets", "WCF Presenters_20230804.xlsx")
fig_path  <- file.path(base_path, "Figures", "For Mobilization Publication")

dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# FIGURE 1: Cumulative Conferences and Cumulative Speakers
# ==============================================================================

# --- Panel A: Cumulative Conferences ---

conferences <- read_excel(data_file, sheet = "List of Conferences") %>%
  mutate(
    meeting_type = case_when(
      str_detect(EventType, "Congress")  ~ "congress",
      str_detect(EventType, "Regional")  ~ "conference",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(meeting_type)) %>%
  count(Year, meeting_type, name = "count") %>%
  complete(Year = full_seq(Year, 1), meeting_type, fill = list(count = 0)) %>%
  arrange(meeting_type, Year) %>%
  group_by(meeting_type) %>%
  mutate(cum_count = cumsum(count)) %>%
  ungroup()

panel_a <- ggplot(conferences, aes(x = Year, y = cum_count,
                                    color = meeting_type,
                                    shape = meeting_type)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_color_manual(values = c("congress" = "black", "conference" = "grey60"),
                     labels = c("congress" = "Global", "conference" = "Regional")) +
  scale_shape_manual(values = c("congress" = 16, "conference" = 15),
                     labels = c("congress" = "Global", "conference" = "Regional")) +
  scale_x_continuous(breaks = seq(1995, 2025, 10), limits = c(1997, 2022)) +
  scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  labs(title = "Conferences", y = "Cumulative Total", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5))

# --- Panel B: Cumulative Speakers ---

speakers_raw <- read_excel(data_file, sheet = "WCF Presenters")

speakers <- speakers_raw %>%
  mutate(
    meeting_type = case_when(
      str_detect(Conference_Type, "Congress")    ~ "congress",
      str_detect(Conference_Type, "Conference")  ~ "conference",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(meeting_type), !is.na(Year)) %>%
  count(Year, meeting_type, name = "count") %>%
  complete(Year = full_seq(Year, 1), meeting_type, fill = list(count = 0)) %>%
  arrange(meeting_type, Year) %>%
  group_by(meeting_type) %>%
  mutate(cum_count = cumsum(count)) %>%
  ungroup()

panel_b <- ggplot(speakers, aes(x = Year, y = cum_count,
                                 color = meeting_type,
                                 shape = meeting_type)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_color_manual(values = c("congress" = "black", "conference" = "grey60"),
                     labels = c("congress" = "Global", "conference" = "Regional")) +
  scale_shape_manual(values = c("congress" = 16, "conference" = 15),
                     labels = c("congress" = "Global", "conference" = "Regional")) +
  scale_x_continuous(breaks = seq(1995, 2025, 10), limits = c(1997, 2022)) +
  scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(0, 1500)) +
  labs(title = "Speakers", y = "Cumulative Total", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, hjust = 0.5))

# --- Combine and save Figure 1 ---
figure1 <- plot_grid(panel_a, panel_b, ncol = 2, align = "h")
ggsave(file.path(fig_path, "Figure 1. Conferences and Speakers.png"),
       figure1, width = 10, height = 5, dpi = 300)

# ==============================================================================
# FIGURE 2: Participants by Organizational Type (Three-Year Rolling Average)
# ==============================================================================

org_type <- speakers_raw %>%
  filter(!is.na(Year), Organization_Type != "" & !is.na(Organization_Type)) %>%
  count(Year, Organization_Type, name = "count") %>%
  complete(Year = full_seq(Year, 1), Organization_Type,
           fill = list(count = 0)) %>%
  arrange(Organization_Type, Year) %>%
  group_by(Organization_Type) %>%
  mutate(ave_count = (lag(count) + count + lead(count)) / 3) %>%
  ungroup()

figure2 <- ggplot(org_type %>% filter(Year < 2022, !is.na(ave_count)),
                  aes(x = Year, y = ave_count,
                      color = Organization_Type,
                      shape = Organization_Type)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Business" = "black", "NGO" = "black",
               "Education" = "grey60", "Religious" = "grey60",
               "State" = "black")
  ) +
  scale_shape_manual(
    values = c("Business" = 1, "NGO" = 0, "Education" = 15,
               "Religious" = 16, "State" = 5)
  ) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1995, 2020)) +
  scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 150)) +
  labs(y = "Total Number of Participating Organizations", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", legend.title = element_blank())

ggsave(file.path(fig_path, "Figure 2. Organizational Type.png"),
       figure2, width = 8, height = 5, dpi = 300)

# ==============================================================================
# APPENDIX FIGURE A1: Organizational Type by Region
# ==============================================================================

org_region <- speakers_raw %>%
  filter(!is.na(Year), Organization_Type != "" & !is.na(Organization_Type)) %>%
  count(Year, Organization_Type, Organization_Region, name = "count") %>%
  complete(Year = full_seq(Year, 1),
           nesting(Organization_Type, Organization_Region),
           fill = list(count = 0)) %>%
  arrange(Organization_Type, Organization_Region, Year) %>%
  group_by(Organization_Type, Organization_Region) %>%
  mutate(ave_count = (lag(count) + count + lead(count)) / 3) %>%
  ungroup()

# Regions and display titles
region_titles <- c(
  "Western Europe & North America" = "Western Europe & North America",
  "Africa" = "Africa",
  "Asia and the Pacific" = "Asia and the Pacific",
  "Eastern Europe & Central Asia" = "Post-Soviet",
  "Latin America and the Caribbean" = "Latin America and the Caribbean"
)

# Generate and save one plot per region
walk(names(region_titles), function(reg) {
  p <- ggplot(org_region %>% filter(Year < 2022, !is.na(ave_count),
                                     Organization_Region == reg),
              aes(x = Year, y = ave_count,
                  color = Organization_Type,
                  shape = Organization_Type)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Business" = "black", "NGO" = "black",
                 "Education" = "grey60", "Religious" = "grey60",
                 "State" = "black")
    ) +
    scale_shape_manual(
      values = c("Business" = 1, "NGO" = 0, "Education" = 15,
                 "Religious" = 16, "State" = 5)
    ) +
    scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1995, 2020)) +
    labs(title = region_titles[reg],
         y = "Number of Total Participants", x = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right", legend.title = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5))

  ggsave(file.path(fig_path, paste0("Figure A1. ", region_titles[reg], ".png")),
         p, width = 8, height = 5, dpi = 300)
})

cat("\n=== Replication complete. Figures saved to:\n", fig_path, "\n")
