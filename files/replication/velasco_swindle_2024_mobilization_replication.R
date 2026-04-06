# ==============================================================================
# Replication Script
# "Assembling God's 'Last Best Hope': The Expanding Reach of the World
#  Congress of Families"
# Velasco, Kristopher and Jeffrey Swindle
# Mobilization: An International Quarterly 29(4): 441-468
#
# NOTE: The descriptive figures were originally written in Stata and have been
# converted to R for ease of replication alongside the network analyses. The
# results replicate those produced by the original Stata code.
#
# This script produces all replicable tables and figures in the article:
#   Table 1:     List of WCF Conferences
#   Table 2:     Twenty Most Common Speakers
#   Table 3:     Twenty Most Common Organizations
#   Table 4:     Top 5 Betweenness Centrality by Historical Period
#   Figure 3:    Cumulative Conferences and Speakers
#   Figure 4:    Three-Year Rolling Avg of Participating Orgs by Type
#   Figure 5:    Network Visualizations by Historical Period (4 panels)
#   Appendix:    Figure 4 Re-Created by Region
#
# Note: Figures 1, 2a, and 2b are photographs/scans and are not replicable.
#
# HOW TO USE:
#   1. Place these three files in the same directory:
#        velasco_swindle_2024_mobilization_main.csv
#        velasco_swindle_2024_mobilization_network.csv
#        velasco_swindle_2024_mobilization_replication.R
#   2. Set your working directory to that folder (or update the paths below).
#   3. Install required packages (see below) and run the script.
#   4. Tables are saved as HTML files; figures are saved as PNG files.
#
# REQUIRED PACKAGES:
#   install.packages(c("tidyverse", "cowplot", "stargazer",
#                      "igraph", "network", "sna", "GGally",
#                      "intergraph", "gridExtra"))
#
# Contact:
#   Kristopher Velasco (kvelasco@princeton.edu)
#   Jeffrey Swindle (jswindle@umich.edu)
#
# Date of original scripts: May 2022 - December 2023
# Date of consolidated R replication: April 6, 2026
# ==============================================================================

# --- Load Required Libraries ---
library(tidyverse)
library(cowplot)
library(stargazer)
library(igraph)
library(network)
library(sna)
library(GGally)
library(intergraph)
library(gridExtra)

options(stringsAsFactors = FALSE)

# ==============================================================================
# SET FILE PATHS — Update to match your local directory
# ==============================================================================
# By default, assumes data files are in the same directory as this script.
# If you placed the files elsewhere, update these paths accordingly.
data_file <- "velasco_swindle_2024_mobilization_main.csv"
net_file  <- "velasco_swindle_2024_mobilization_network.csv"
fig_path  <- "figures"
tab_path  <- "tables"

dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_path, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# READ DATA
# ==============================================================================
speakers_raw <- read.csv(data_file)
wcf_net      <- read.csv(net_file)

# Derive conference-level data from speaker-level records
conferences_raw <- speakers_raw %>%
  distinct(Conference_ID, Conference_Type, Conference_Title,
           Conference_Country, Conference_City, Year, Conference_Theme)

# ******************************************************************************
# SECTION 1: TABLES
# ******************************************************************************

# ==============================================================================
# TABLE 1: List of World Congress of Families Conferences
# ==============================================================================

table1 <- conferences_raw %>%
  mutate(
    `Conference Type` = case_when(
      str_detect(Conference_Type, "Congress")    ~ "Global Congress",
      str_detect(Conference_Type, "Conference")  ~ "Regional Conference",
      TRUE ~ Conference_Type
    )
  ) %>%
  arrange(Year, `Conference Type`) %>%
  select(Year, `Conference Type`, Location = Conference_City,
         `Theme` = Conference_Title)

# Count distinct orgs per conference-year
orgs_per_conf <- speakers_raw %>%
  filter(!is.na(Organization) & Organization != "") %>%
  group_by(Year, Conference_Type) %>%
  summarise(Orgs = n_distinct(Organization), .groups = "drop")

stargazer(as.data.frame(table1),
          type = "html",
          title = "Table 1. List of World Congress of Families Conferences",
          summary = FALSE, rownames = FALSE,
          out = file.path(tab_path, "Table1_Conference_List.html"))

cat("Table 1 saved.\n")

# ==============================================================================
# TABLE 2: Twenty Most Common Speakers, 1997-2022
# ==============================================================================

table2 <- speakers_raw %>%
  filter(!is.na(Person) & Person != "") %>%
  group_by(Person) %>%
  summarise(
    `Organization(s)` = first(na.omit(Organization)),
    Location           = first(na.omit(Organization_Country)),
    Appearances        = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Appearances)) %>%
  slice_head(n = 20) %>%
  select(Name = Person, `Organization(s)`, Location, Appearances)

stargazer(as.data.frame(table2),
          type = "html",
          title = "Table 2. Twenty Most Common Speakers at WCF Conferences, 1997-2022",
          summary = FALSE, rownames = FALSE,
          out = file.path(tab_path, "Table2_Top_Speakers.html"))

cat("Table 2 saved.\n")

# ==============================================================================
# TABLE 3: Twenty Most Common Organizations, 1997-2022
# ==============================================================================

table3 <- speakers_raw %>%
  filter(!is.na(Organization) & Organization != "") %>%
  group_by(Organization) %>%
  summarise(
    Location    = first(na.omit(Organization_Country)),
    Appearances = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Appearances)) %>%
  slice_head(n = 20) %>%
  select(Organization, Location, Appearances)

stargazer(as.data.frame(table3),
          type = "html",
          title = "Table 3. 20 Most Common Organizations at WCF Conferences, 1997-2022",
          summary = FALSE, rownames = FALSE,
          out = file.path(tab_path, "Table3_Top_Organizations.html"))

cat("Table 3 saved.\n")

# ******************************************************************************
# SECTION 2: FIGURE 3 — Cumulative Conferences and Speakers
# ******************************************************************************

# --- Panel A: Cumulative Conferences ---
conferences <- conferences_raw %>%
  mutate(
    meeting_type = case_when(
      str_detect(Conference_Type, "Congress")    ~ "congress",
      str_detect(Conference_Type, "Conference")  ~ "conference",
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
                                    color = meeting_type, shape = meeting_type)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2) +
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
                                 color = meeting_type, shape = meeting_type)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2) +
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

figure3 <- plot_grid(panel_a, panel_b, ncol = 2, align = "h")
ggsave(file.path(fig_path, "Figure 3. Conferences and Speakers.png"),
       figure3, width = 10, height = 5, dpi = 300)
cat("Figure 3 saved.\n")

# ******************************************************************************
# SECTION 3: FIGURE 4 — Participating Orgs by Type (Three-Year Rolling Avg)
# ******************************************************************************

org_type <- speakers_raw %>%
  filter(!is.na(Year), Organization_Type != "" & !is.na(Organization_Type)) %>%
  count(Year, Organization_Type, name = "count") %>%
  complete(Year = full_seq(Year, 1), Organization_Type, fill = list(count = 0)) %>%
  arrange(Organization_Type, Year) %>%
  group_by(Organization_Type) %>%
  mutate(ave_count = (lag(count) + count + lead(count)) / 3) %>%
  ungroup()

figure4 <- ggplot(org_type %>% filter(Year < 2022, !is.na(ave_count)),
                  aes(x = Year, y = ave_count,
                      color = Organization_Type, shape = Organization_Type)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2) +
  scale_color_manual(values = c("Business" = "black", "NGO" = "black",
                                "Education" = "grey60", "Religious" = "grey60",
                                "State" = "black")) +
  scale_shape_manual(values = c("Business" = 1, "NGO" = 0, "Education" = 15,
                                "Religious" = 16, "State" = 5)) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1995, 2020)) +
  scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 150)) +
  labs(y = "Total Number of Participating Organizations", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", legend.title = element_blank())

ggsave(file.path(fig_path, "Figure 4. Organizational Type.png"),
       figure4, width = 8, height = 5, dpi = 300)
cat("Figure 4 saved.\n")

# ******************************************************************************
# SECTION 4: FIGURE 5 — Network Visualizations by Historical Period
# ******************************************************************************

# --- Helper: build igraph object from conference column subset ---
build_network <- function(data, conf_cols) {
  orgs    <- data$Organization
  country <- data$Organization_Country
  region  <- data$Organization_Region

  mat <- as.matrix(data[, conf_cols, drop = FALSE])
  rownames(mat) <- orgs

  # Two-mode to one-mode projection
  mat <- mat %*% t(mat)
  diag(mat) <- 0

  g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  V(g)$name    <- orgs
  V(g)$country <- country
  V(g)$region  <- region
  return(g)
}

# Conference columns for each historical period
cols_1997 <- c("C1", "C2", "R1999A", "R1999B", "R2000A", "R2000B",
               "R2001A", "R2001B", "R2001C")
cols_2002 <- c("C3", "R2002A", "R2006A")
cols_2007 <- c("C4", "C5", "C6", "C7", "C9", "R2009B", "R2010D",
               "R2011D", "R2011E", "R2011F", "R2011G", "R2013C",
               "R2013D", "R2013E", "R2014C", "R2014D", "R2014E",
               "R2014F", "R2015B", "R2015D", "R2015E", "R2015F",
               "R2015G", "R2015H")
cols_2016 <- c("C10", "C11", "C12", "C13", "C14", "R2016B", "R2016C",
               "R2016D", "R2017A", "R2017B", "R2017C", "R2017D",
               "R2017E", "R2018C", "R2018D", "R2018E", "R2018F",
               "R2019B")

# Build period-specific networks
g_1997 <- build_network(wcf_net, cols_1997)
g_2002 <- build_network(wcf_net, cols_2002)
g_2007 <- build_network(wcf_net, cols_2007)
g_2016 <- build_network(wcf_net, cols_2016)

# Drop isolates for visualization
g_1997_vis <- igraph::delete.vertices(g_1997, V(g_1997)[igraph::degree(g_1997) == 0])
g_2002_vis <- igraph::delete.vertices(g_2002, V(g_2002)[igraph::degree(g_2002) == 0])
g_2007_vis <- igraph::delete.vertices(g_2007, V(g_2007)[igraph::degree(g_2007) == 0])
g_2016_vis <- igraph::delete.vertices(g_2016, V(g_2016)[igraph::degree(g_2016) == 0])

# Region color palette
region_palette <- c(
  "Post-Soviet"             = "#ffbb44",
  "Latin America/Caribbean" = "#ee8577",
  "Asia/Pacific"            = "#ce4441",
  "Africa"                  = "#859b6c",
  "The West"                = "#004f63"
)

# Network plot helper
plot_network <- function(g, title_text) {
  ggnet2(g, color = "region", palette = region_palette,
         size = 2, size.min = 1, size.cut = 4,
         node.alpha = 0.8, segment.alpha = 0.8,
         edge.color = c("color", "grey70"), edge.alpha = 0.5,
         layout.exp = 0.05, color.legend = "Region") +
    guides(size = "none") +
    ggtitle(title_text) +
    theme(plot.title = element_text(hjust = 0.5))
}

net1 <- plot_network(g_1997_vis, "1997-2001")
net2 <- plot_network(g_2002_vis, "2002-2006")
net3 <- plot_network(g_2007_vis, "2007-2015")
net4 <- plot_network(g_2016_vis, "2016-2022")

figure5 <- grid.arrange(net1, net2, net3, net4, ncol = 2, nrow = 2)
ggsave(file.path(fig_path, "Figure 5. Network Visualizations.png"),
       figure5, width = 14, height = 12, dpi = 300)
cat("Figure 5 saved.\n")

# ******************************************************************************
# SECTION 5: TABLE 4 — Betweenness Centrality by Period
# ******************************************************************************

get_top_betweenness <- function(g, period_label, n = 5) {
  V(g)$betweenness <- igraph::betweenness(g)
  data.frame(
    Period       = period_label,
    Organization = V(g)$name,
    Country      = V(g)$country,
    Score        = round(V(g)$betweenness, 0),
    stringsAsFactors = FALSE
  ) %>%
    filter(Score > 0) %>%
    arrange(desc(Score)) %>%
    slice_head(n = n)
}

table4 <- bind_rows(
  get_top_betweenness(g_1997, "1997-2001"),
  get_top_betweenness(g_2002, "2002-2006"),
  get_top_betweenness(g_2007, "2007-2015"),
  get_top_betweenness(g_2016, "2016-2022")
) %>%
  mutate(Label = paste0(Organization, ", ", Country)) %>%
  select(Period, Organization = Label, `Betweenness Centrality` = Score)

stargazer(as.data.frame(table4),
          type = "html",
          title = "Table 4. Top Five Organizations with Greatest Betweenness Centrality in Each Historical Period",
          summary = FALSE, rownames = FALSE,
          out = file.path(tab_path, "Table4_Betweenness_Centrality.html"))

cat("Table 4 saved.\n")

# ******************************************************************************
# SECTION 6: APPENDIX — Figure 4 Re-Created by Region
# ******************************************************************************

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

region_titles <- c(
  "Western Europe & North America"  = "Western Europe & North America",
  "Africa"                          = "Africa",
  "Asia and the Pacific"            = "Asia and the Pacific",
  "Eastern Europe & Central Asia"   = "Eastern Europe & Central Asia",
  "Latin America and the Caribbean" = "Latin America and the Caribbean"
)

walk(names(region_titles), function(reg) {
  p <- ggplot(org_region %>% filter(Year < 2022, !is.na(ave_count),
                                     Organization_Region == reg),
              aes(x = Year, y = ave_count,
                  color = Organization_Type, shape = Organization_Type)) +
    geom_line(linewidth = 0.6) + geom_point(size = 2) +
    scale_color_manual(values = c("Business" = "black", "NGO" = "black",
                                  "Education" = "grey60", "Religious" = "grey60",
                                  "State" = "black")) +
    scale_shape_manual(values = c("Business" = 1, "NGO" = 0, "Education" = 15,
                                  "Religious" = 16, "State" = 5)) +
    scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1995, 2020)) +
    labs(title = region_titles[reg],
         y = "Number of Total Participants", x = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right", legend.title = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5))
  ggsave(file.path(fig_path, paste0("Appendix Figure. ", region_titles[reg], ".png")),
         p, width = 8, height = 5, dpi = 300)
})
cat("Appendix figures saved.\n")

# ==============================================================================
cat("\n=== REPLICATION COMPLETE ===\n")
cat("Figures saved to:", fig_path, "\n")
cat("Tables saved to:", tab_path, "\n")
# ==============================================================================
