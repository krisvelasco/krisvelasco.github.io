################################################################################
#  Replication Script for:
#
#  Gonsalves, Tara, and Kristopher Velasco. 2022. "Seeking Friends in
#    Troubled Times: The Structure and Dynamics of Transnational LGBT
#    Networks in Europe." Mobilization: An International Quarterly
#    27(1): 91-114. https://doi.org/10.17813/1086-671X-27-91
#
#  Authors: Tara Gonsalves (UC Berkeley), Kristopher Velasco (Princeton)
#  Date Created: April 2026
#
#  NOTE: The original analysis was split between R (for network centrality
#  computation from bipartite affiliation matrices) and Stata (for data
#  merging, variable construction, and multilevel models). For ease of
#  replication, we have consolidated all code into this single R script.
#
#  OVERVIEW:
#    PART 1: DATA CONSTRUCTION (Sections 1-5)
#      Constructs the analysis dataset from raw source files. Includes
#      network centrality computation, organization attribute coding,
#      country-level control merging, and variable centering.
#      Requires raw source files in raw_sources/ subfolder.
#
#    PART 2: ANALYSIS (Sections 6-9)
#      Replicates all published tables and figures.
#
#  HOW TO USE:
#    Option A -- Full pipeline: Run Part 1 then Part 2. Requires raw
#      source files in raw_sources/ (contact authors for these).
#
#    Option B -- Analysis only: Skip Part 1, uncomment the line at the
#      start of Section 6 to load the pre-built dataset, then run Part 2.
#      Only requires the .dta file shipped with this package.
#
#    Files for upload:
#      gonsalves_velasco_2022_mob_main.dta       Pre-built analysis dataset
#      gonsalves_velasco_2022_mob_replication.R   This file
#
#    Contact:
#      Tara Gonsalves (taragonsalves@berkeley.edu)
#      Kristopher Velasco (kvelasco@princeton.edu)
#
#  RAW SOURCE DATASETS (in raw_sources/, available upon request):
#
#    01_affiliation_matrix.xlsx
#      Bipartite affiliation matrix: ~3,100 domestic LGBT associations x
#      46 LGBT INGOs across Europe, coded at 2010, 2017, and 2020.
#      Sheets: "2010" (22 INGOs), "2017" (39 INGOs), "2020" (46 INGOs).
#
#    02_ingo_counts.xlsx
#      Number of INGO memberships per association per year.
#
#    03_domestic_lgbt_ngos.xlsx
#      Master dataset of ~12,000 domestic LGBT associations worldwide.
#      Used to derive: year_founded, advocacy, hr_frame.
#
#    04_country_controls.xlsx
#      Country-year panel: GDP per capita, population, Freedom House,
#      EU membership, marriage/ban indicators, domestic associations.
#
#    05_gai_index.xls
#      Global Acceptance Index (public opinion toward LGBT people).
#
#    06_domestic_ngo_totals.dta
#      Cumulative domestic LGBT NGO counts by country (for per-capita).
#
#  REQUIRED PACKAGES:
#    install.packages(c("haven", "lme4", "lmerTest", "texreg",
#                       "ggplot2", "dplyr", "tidyr",
#                       "readxl", "igraph"))
#
################################################################################


library(readxl)
library(igraph)
library(haven)
library(lme4)
library(lmerTest)
library(texreg)
library(ggplot2)
library(dplyr)
library(tidyr)


# USERS: Set this to the directory containing the replication files.
# setwd("/path/to/replication/files")


# ==============================================================================
# PART 1: DATA CONSTRUCTION
# ==============================================================================
# Requires raw source files in raw_sources/ subfolder.
# To skip and use the pre-built dataset, go to Section 6 and uncomment the
# read_dta() line.
# ==============================================================================


################################################################################
# 1. NETWORK CENTRALITY COMPUTATION
#    Read the bipartite (two-mode) affiliation matrix at each wave,
#    project to one-mode (association x association), compute eigenvector
#    centrality and degree.
################################################################################

centrality_all <- data.frame()

for (yr in c(2010, 2017, 2020)) {
  cat(sprintf("Computing centrality for %d...\n", yr))

  mat_raw <- read_excel("raw_sources/01_affiliation_matrix.xlsx",
                        sheet = as.character(yr))

  # Store identifiers (trim whitespace from column names and values)
  names(mat_raw) <- trimws(names(mat_raw))
  org_names   <- trimws(mat_raw$Organization)
  country_names <- trimws(mat_raw$Country)

  # Extract binary INGO membership columns (everything after Organization & Country)
  ingo_cols <- setdiff(names(mat_raw), c("Organization", "Country"))
  bip_mat <- as.matrix(mat_raw[, ingo_cols])
  bip_mat[is.na(bip_mat)] <- 0
  storage.mode(bip_mat) <- "numeric"
  rownames(bip_mat) <- org_names

  # Project bipartite to one-mode: shared INGO membership
  one_mode <- bip_mat %*% t(bip_mat)
  diag(one_mode) <- 0

  # Compute centrality
  g <- graph_from_adjacency_matrix(one_mode, mode = "undirected", weighted = TRUE)
  eigen_scores <- eigen_centrality(g)$vector
  degree_scores <- degree(g)

  wave <- data.frame(
    Organization = org_names,
    Country      = country_names,
    NGO_eigen    = eigen_scores,
    degree       = degree_scores,
    year         = yr,
    stringsAsFactors = FALSE
  )

  # Store INGO membership flags for later purpose inference (Section 3)
  ingo_flags <- as.data.frame(bip_mat)
  ingo_flags$Organization <- org_names
  ingo_flags$Country <- country_names
  ingo_flags$year <- yr
  if (yr == 2010) {
    all_ingo_flags <- ingo_flags
  } else {
    # Bind with NA fill for INGOs not present in earlier waves
    all_ingo_flags <- bind_rows(all_ingo_flags, ingo_flags)
  }

  centrality_all <- bind_rows(centrality_all, wave)
}

# Clean country name inconsistencies
centrality_all <- centrality_all %>%
  mutate(Country = case_when(
    Country == "united Kingdom"  ~ "United Kingdom",
    Country == "United kingdom"  ~ "United Kingdom",
    Country == "ireland"         ~ "Ireland",
    Country == "italy"           ~ "Italy",
    Country == "Span"            ~ "Spain",
    Country == "Kazahkstan"      ~ "Kazakhstan",
    Country == "Cyrpus"          ~ "Cyprus",
    TRUE ~ Country
  ))

# Create unique ID and deduplicate
centrality_all$uniqueid <- paste0(centrality_all$Organization,
                                   centrality_all$Country)
centrality_all <- centrality_all %>%
  distinct(uniqueid, year, .keep_all = TRUE)

cat(sprintf("Centrality computed: %d association-year obs\n", nrow(centrality_all)))


################################################################################
# 2. INGO COUNTS
#    Merge number of INGO memberships per association per year.
################################################################################

ingo_counts <- read_excel("raw_sources/02_ingo_counts.xlsx", sheet = "2010")
names(ingo_counts) <- trimws(names(ingo_counts))
ingo_counts <- ingo_counts %>%
  mutate(Organization = trimws(Organization),
         Country = trimws(Country),
         uniqueid = paste0(Organization, Country)) %>%
  distinct(uniqueid, year, .keep_all = TRUE)

# Keep only associations in both datasets
centrality_all <- inner_join(
  centrality_all,
  ingo_counts %>% select(uniqueid, year, ingo_counts),
  by = c("uniqueid", "year")
)

cat(sprintf("After INGO counts merge: %d obs\n", nrow(centrality_all)))


################################################################################
# 3. ORGANIZATION-LEVEL ATTRIBUTES
#    Code purpose (-> advocacy), human rights framing, and founding year
#    from the master domestic LGBT NGO dataset.
################################################################################

ngo_master <- read_excel("raw_sources/03_domestic_lgbt_ngos.xlsx",
                         sheet = "Master Data")

# Keep European organizations only
ngo_master <- ngo_master %>%
  filter(Region == "Europe",
         is.na(Drop) | Drop != 1) %>%
  mutate(Organization = trimws(Organization),
         Country = trimws(Country))

# Clean country names to match
ngo_master <- ngo_master %>%
  mutate(Country = case_when(
    Country == "united Kingdom"  ~ "United Kingdom",
    Country == "United kingdom"  ~ "United Kingdom",
    Country == "ireland"         ~ "Ireland",
    Country == "italy"           ~ "Italy",
    TRUE ~ Country
  ))

# --- 3a. Keyword classification on org name and mission statement ---
ngo_master <- ngo_master %>%
  mutate(
    org_lower     = tolower(Organization),
    mission_lower = tolower(ifelse(is.na(`Mission Statement`), "",
                                    `Mission Statement`)),
    purpose = ""
  )

# Keywords for each category (applied to both name and mission)
classify_keywords <- function(text_org, text_mission) {
  purpose <- rep("", length(text_org))
  combined <- paste(text_org, text_mission)

  purpose[grepl("right|advoca|fight|discriminat|activist|homophobia|transphobia",
                combined)] <- "advocacy"
  purpose[purpose == "" & grepl("art |sing |chorus |film |choir |die |media |archive|cultur",
                                 combined)] <- "arts/culture"
  purpose[purpose == "" & grepl("sport|football|swim|aqua|dance|wrestl|rugby |game|martial|runners| fc | rufc | rfc |team|athlet",
                                 combined)] <- "sport"
  purpose[purpose == "" & grepl("pride", combined)] <- "pride parade"
  purpose[purpose == "" & grepl("jew|muslim |christ|crist|faith |islam |orthodox |church |catholic |religious",
                                 combined)] <- "religious"
  purpose[purpose == "" & grepl("parent|family |families", combined)] <- "family"
  purpose[purpose == "" & grepl("health|hiv|aids |positive", combined)] <- "health/support"
  purpose[purpose == "" & grepl("youth |student|teenager|campus", combined)] <- "youth"
  purpose[purpose == "" & grepl("commerc|business|entrepreuner|company |executive",
                                 combined)] <- "economic"
  purpose
}

ngo_master$purpose <- classify_keywords(ngo_master$org_lower,
                                         ngo_master$mission_lower)

# --- 3b. Manual purpose classifications ---
# ~350 hand-coded assignments for organizations whose purpose could not
# be determined from keyword search alone.

manual <- list(
  "arts/culture" = c("Barcedona", "Famba", "VREAK Holebitheater", "Ongehoordt",
    "Luvvies Theatre Company", "Linsert.org", "COC - Kennemerland",
    "Plymouth LGBT Archive", "Trans Media Watch", "KdT Fundacja Kultura dla Tolerancji - Culture for tolerance Foundation",
    "Genre d a Cote", "Les Fiertes Namuroises", "OLLOVE", "OutStories Bristol",
    "Academie Gay et Lesbienne", "Conversas Para Le-Las", "Barbieturix",
    "A voix et a vapeur", "GenderAgenda", "Lesbian and Gay Newsmedia Archive",
    "Fondazione FUORI", "Festival REFLETS - Association MPPM",
    "BGalva108", "Gay Birmingham remembered", "Poulailler et Cie",
    "Queer Office", "Spinnboden Lesbenarchiv & Bibliothek e V",
    "Cardiff Wales LGBT Mardi Gras", "BiMedia org", "Various Voices London",
    "Bears et Compagnie", "Pikara Magazine", "PublicG TV", "ScotsGay Magazine",
    "Let's Dyke!", "Les Concerts Gais", "Internationaal Homo/Lesbisch Informatiecentrum en Archief IHLIA-",
    "OurStory Scotland", "Secret Opera", "QWIEN", "GayKitschCamp",
    "Gay by - Gay Alliance Belarus", "Gay.Al Albania",
    "Fetish Lyon", "Shout!", "Remember When", "LGBT Archive UK",
    "Eesti Nahkmeeste Klub", "German Society for Social Scientific Sexuality Research",
    "Centro LGBT", "Festival de Films Gays et Lesbiens d'Orleans - D Un Bord a I'autre",
    "Les Joyeux Reporters", "OutBurst Magazine", "LGBT Vox",
    "Birmingham Gay Symphony Orchestra", "Hope Theatre Company",
    "Diario Digitical Transexual", "Rainbow Symphony Orchestra",
    "GAY & LESBIAN ARTS AND MEDIA LTD", "Paradise Press",
    "International Lesbian and Gay Correspondence Network",
    "Gay Star News"),
  "health/support" = c("Clare Women's Network", "London Lesbian and Gay Switchboard",
    "Schwulenberatung Berlin", "Gay Doctors Ireland", "WASAlgbt", "Proud2Be",
    "JUST LIKE US", "Opening Doors London", "Proud2Serve",
    "National Trans Police Association", "Edinburgh Trans Women",
    "Hinsegin Norourland", "Regard", "Coming Out UK", "Archway Glasgow",
    "Lesbians in Cork", "Diversitay LGBT Group - Dundee",
    "Trans Masculine Support and Advice UK", "Just Be Yourself",
    "Lesbian Immigration Support Group", "Transpire Southend Trans Network",
    "List LGBT Network", "LGBT Number 3 Social and Support Group",
    "Clare Project", "Trans Matters", "LGBT Moray", "Sarigai",
    "Mesmac North Wales", "Mesmac North East", "HIV Iceland",
    "Outhouse East", "West London Gay Men's Project",
    "Schools Out United Kingdom", "Lambeth LGBT Forum",
    "LGBT CANCER SUPPORT",
    "ALDARTE - Centro de Atencion a Gays, Lesbianas y Transexuales",
    "Unique Transgender Network", "Bradford LGBT Strategic Partnership",
    "Lambeth LGBT+ Network", "Quest for Gay Men",
    "East Lancashire LGBT",
    "Pink Sou'westers: South West Birmingham LGBT Group",
    "Peer Action", "Doncaster LGBT+ Forum",
    "LGBT Older People's Group North Staffordshire",
    "Fundacion Salud y Comunidad", "Avon Trans People",
    "AffirmNI", "Icebreakers: Manchester Gay Men's Support Group",
    "Maytree: A Sanctuary for the Suicidal", "LGBT Foundation",
    "Transgender Netwerk Nederland - TNN", "Foundation Transman",
    "Transgender Infopunt",
    "Deutsche Gesellschaft fur Transidentitat und Intersexualitat",
    "Sonntags-Club e V",
    "MANCHESTER LESBIAN AND GAY SWITCHBOARD SERVICES LIMITED",
    "Les Gai(e)s Retraites - LGR", "Lil!th",
    "Fundacion 26 de Diciembre", "Roze Joker",
    "Conjuntos Difusos - Autonomia Trance", "Dutch Bisexual Network",
    "Maison des Femmes",
    "Ireos - Centro Servizi Autogestito Comunita Queer",
    "Asociacion de Lesbianas Gays Transexuales y Bisexuales de Albacete",
    "York St  John University LGBT Staff Network",
    "Ireland National LGBT Helpline", "SLIMK e.V.",
    "Pari-t Transgenres", "Support Transgenre Strasbourg",
    "DreamwearClub Ry", "HM2F - Homosexuels Musulmans 2 France",
    "Association Gayttitude Psychologie", "Albany Trust",
    "Atthis VZW", "Free2B Alliance CIC", "INNER ENIGMA",
    "SO OUT in the South West",
    "Gay Essex Men's Social and Support Group", "Gender-L",
    "Age Concern - Opening Doors Project", "Outline",
    "GAY ADVICE SUNDERLAND", "Trans Resource and Empowerment center",
    "TransWirral", "Transpartners", "George House Trust",
    "Fine Gael LGBT", "Trans London", "TransPALS",
    "Encompass Network", "GAIS Positius", "Queer Altern",
    "Outwest Gay Helpline",
    "Association of Transgendered People in Norway",
    "Gay Ammanford", "NGO Estonian Network PLWHIV",
    "Labyrint Belgium", "Txy - Communaut des Travestis Transgenres & Transidentitaires",
    "Affirmation Scotland",
    "Homo en Lesbiennewerking Mechelen - HLWM",
    "FTM Brighton", "Greenbow LGBT Society of Ireland",
    "LGBT SHEFFIELD",
    "Gay & Lesbian to socialize and Rehabilitation - GLSRS",
    "AIDS-i Tugikeskus (AIDS Information & Support Center)",
    "Older Lesbian, Gay, Bisexual and Trans Association",
    "Rainbows Across Borders", "Empowerment Lifestyle Services",
    "International Support Group for Information Transfer and Networking ISGITN",
    "Abraco - Association in Support of People Living with HIV/AIDS",
    "LGBT Cymru Helpline", "TransForum Manchester",
    "BLOGS", "Rainbow Association Malkus",
    "Berkshire Older Gay and Lesbian Forum",
    "Strathclyde Gay and Lesbian Switchboard",
    "Czech Aids Help Society", "Dorset Lesbian and Gay Helpline",
    "TRANS MEDICINAL LTD", "LGBT Alcohol Support Group",
    "Aiuto Aids Ticino", "Bath and North East Somerset Mens Sexual Health",
    "Trans Peer Support Group", "Wiltshire Men's Sexual Health",
    "LGBT Domestic Abuse Centre",
    "Gay and Lesbian Switchboard Nederland",
    "Gay Switchboard Dublin",
    "Gi'h Gais I Lesbianes de L'Hospitalet",
    "Claire LGBT Support Forum",
    "Friends and Supporters of the Furness LGBT Community",
    "Coventry and Warwickshire Friend", "La Maison des Femmes",
    "East London Out Project - ELOP", "Pink Therapy",
    "Asociata Romana Anti-Sida - ARAS",
    "Brighton and Hove LGBT Switchboard",
    "Cest Pas Mon Genre", "Mypinkpal com",
    "Sandyford Trans Women's Support Group",
    "Unity Center LGBT Asylum Seekers Helpline",
    "GAY HEALTHY ALLIANCE PROJECT",
    "Switchboard: LGBT+ Helpline",
    "Imbarco Immediato", "OUTRANS", "London Friend",
    "Beaumont Society", "BiPhoria", "West Norfolk LGBT Network",
    "Connected", "2BU-Somerset"),
  "sport" = c("Berlin Seitenwechsel", "Focus: The Identity Trust",
    "Delft Roze Blok Delft", "Alcedo Praha", "Utrecht Vlerk",
    "Transfšreningen FPES", "Gir@s Caminhadas",
    "Nuova Kaos Milano", "Eindhoven Kouros Eindhoven",
    "Natuurlijk Holebi", "JyvŠskylŠ Seta",
    "Madrid Madpoint", "Rome gACE Tennis Roma",
    "Older and Out", "Caithness LGBTI Social Group",
    "English-Speaking Gay Group", "Ca La Dona", "GAYTITUDES",
    "Amsterdam Bridgevereniging Gay Forcing", "BoysProject",
    "A pas de Geant", "Pimpernel40Plus",
    "Bi and Beyond Scotland", "Aminours",
    "Zaragoza Elaios", "Panteres Grogues",
    "Schwulenreferat der Carl-von-ossietzky-universitat",
    "Leipzig RosaLowen", "Association Motorcycliste Alternative - AMA",
    "ABC Association Beaumont Continental", "The Lady List",
    "North and West London Lesbian and Gay Group",
    "Eisteiger Ringen Berlin", "MEC", "MIF",
    "Gay Birders Club", "Berliner Regenbogenforellen",
    "Trafford Prime", "Kiev NRG - Women's sports club",
    "Racqueteers Badminton Group", "Gay Yoga",
    "Dynamo Dykes", "London Spikers Volleyball Club",
    "Irons Golf Society", "Team Manchester",
    "BGMC Knalpijp Belgian Gay Motorcycle Club",
    "York Lesbians Social Group", "Ladybird Book Club",
    "Sewing Circle", "Al-Jannah", "Warsaw Volup",
    "Tennis London International",
    "West London Gay Bridge Club",
    "Les Benines d Apie", "Rubyfruits Edinburgh",
    "Edinburgh Gay Men's Book Group",
    "Word: Women's Book Group",
    "Just Friends: Gay Men's Social Group",
    "Bonn Bonner Hupfdohlen"),
  "pride parade" = c("Northern Pride", "Antwerp Pride",
    "Exmouth Pride", "LGP Biarritz Impact - Lesbiennes Gays Pride",
    "Bellingham Pride", "Stoke-on-Trent Pride",
    "Christopher Street Day Konstanz e V",
    "Pride Scotia", "Riga Pride"),
  "religious" = c("Evangelical Fellowship for Lesbian and Gay Christians",
    "Quaker Lesbian and Gay Fellowship", "GayChristians",
    "Interfaith Diversity Network of West Africa",
    "Crismhom Cristianas y Cristianos de Madrid LGBT+H",
    "Soho Masses - LGBT Catholics Westminster",
    "Adverta Evangeliska Draudze/Open Evangelical Congregation",
    "Lesbian and Gay Clergy Consultation",
    "LGCM London Fellowship", "Liberty Church Blackpool"),
  "family" = c("Familias Arco-Iris", "Hanse-X-Men - HXM",
    "LISTAG (Families of LGBT in Istanbul)", "Kwadraat",
    "Irish Pink Adoptions", "Contact Rhone",
    "Families and Friends of Lesbians and Gays - FFLAG",
    "Rainbow Future", "GBT Dads Ayrshire",
    "Be you: Berkshire Lesbian and Gay Information Network",
    "Families Together Devon"),
  "youth" = c("42nd Street", "Mixed", "Proud Trust", "Juventas",
    "G-Stud", "Being Gay is Okay",
    "BAStA Universitat zu Koln",
    "FAG Fac Aix Gay",
    "Cercle Homsoexuel Estudiantin de Liege - CHEL",
    "ComitŽ pour la reconnaissance sociale des homosexuel/les CRSH",
    "GLUP Groupe LGBT des Universites de Paris",
    "PYLGBT", "BLAH LGBT Youth",
    "London South Bank University Student's Union LGBT Programme",
    "London School of Economics and Political Science LGBT Society",
    "Stirling University LGBT Group",
    "Glasgow University LGBT Students Association",
    "Godiva Young Gay and Lesbians",
    "Asociacion Universitaria de Lesbianas, Gays, Transexuales, y Bisexuales - Universidad de Salamanca",
    "Breakout - Letterkenny LGBT Youth"),
  "economic" = c("Associazione Italiana Turismo Gay & Lesbian",
    "Devon County Council LGBT Staff Network", "PCS Proud",
    "Gay Business Association UK", "Gay Business Antwerp",
    "Associacio Catalana d Empreses per a Gais Lesbianes",
    "PROUT AT WORK-Foundation", "Rainbow Cops Belgium",
    "European Gay Police Association",
    "Stichting Homosexualiteit En Krijgsmacht",
    "G-Force: Garda Gay Lesbian and Bisexual Employee Resource Group",
    "Lesbian and Gay Lawyers Association",
    "Bedforshire Gay Police Association"),
  "leisure" = c("PROFIL Haderslev"),
  "heath/supprt" = c("Rainbow Bridge - Victim Support")
)

for (cat_name in names(manual)) {
  idx <- ngo_master$Organization %in% manual[[cat_name]] & ngo_master$purpose == ""
  ngo_master$purpose[idx] <- cat_name
}
# Fix typos in category names
ngo_master$purpose[ngo_master$purpose == "heath/supprt"] <- "health/support"


# --- 3c. INGO-tie-based purpose inference ---
# For organizations still unclassified, use INGO membership to infer purpose.
# Create "ever member" flags by collapsing the affiliation matrix across years.

all_ingo_flags <- all_ingo_flags %>%
  mutate(Organization = trimws(Organization),
         Country = trimws(Country)) %>%
  mutate(Country = case_when(
    Country == "united Kingdom"  ~ "United Kingdom",
    Country == "United kingdom"  ~ "United Kingdom",
    Country == "ireland"         ~ "Ireland",
    Country == "italy"           ~ "Italy",
    Country == "Span"            ~ "Spain",
    Country == "Kazahkstan"      ~ "Kazakhstan",
    Country == "Cyrpus"          ~ "Cyprus",
    TRUE ~ Country
  ))

# Get all INGO columns
ingo_col_names <- setdiff(names(all_ingo_flags),
                           c("Organization", "Country", "year"))

# Collapse to "ever member" across waves
ever_member <- all_ingo_flags %>%
  group_by(Organization, Country) %>%
  summarize(across(all_of(ingo_col_names),
                   ~ as.integer(max(.x, na.rm = TRUE) > 0)),
            .groups = "drop")

# Merge into ngo_master
ngo_master <- ngo_master %>%
  left_join(ever_member, by = c("Organization", "Country"))

# Replace NAs with 0 in INGO columns
for (col in intersect(ingo_col_names, names(ngo_master))) {
  ngo_master[[col]][is.na(ngo_master[[col]])] <- 0
}

# Helper to safely check INGO membership
has_tie <- function(col_name) {
  if (col_name %in% names(ngo_master)) {
    return(ngo_master[[col_name]] == 1)
  }
  return(rep(FALSE, nrow(ngo_master)))
}

# Sport INGOs
sport_tie <- has_tie("FGG") | has_tie("EGLSF") | has_tie("IGLFA") |
  has_tie("GALA") | has_tie("GLISA") | has_tie("IGRA")
ngo_master$purpose[ngo_master$purpose == "" & sport_tie] <- "sport"

# Pride INGOs
pride_tie <- has_tie("InterPride") | has_tie("EuroPride")
ngo_master$purpose[ngo_master$purpose == "" & pride_tie] <- "pride parade"

# Religious INGOs
religious_tie <- has_tie("EFLGBTCG") | has_tie("GNRC") | has_tie("WCGLBTJ")
ngo_master$purpose[ngo_master$purpose == "" & religious_tie] <- "religious"

# Family INGOs
family_tie <- has_tie("NELFA") | has_tie("CG")
ngo_master$purpose[ngo_master$purpose == "" & family_tie] <- "family"

# Health/support INGOs
health_tie <- has_tie("ECMH") | has_tie("AMSHeR")
ngo_master$purpose[ngo_master$purpose == "" & health_tie] <- "health/support"

# Youth INGOs
youth_tie <- has_tie("IGLYO") | has_tie("ANSO")
ngo_master$purpose[ngo_master$purpose == "" & youth_tie] <- "youth"

# Economic INGOs
econ_tie <- has_tie("NGLCC") | has_tie("ELGBTICC")
ngo_master$purpose[ngo_master$purpose == "" & econ_tie] <- "economic"

# All remaining unclassified -> "other"
ngo_master$purpose[ngo_master$purpose == ""] <- "other"


# --- 3d. Founding year ---
ngo_master <- ngo_master %>%
  mutate(year_founded = ifelse(!is.na(`Year Founded`),
                                `Year Founded`,
                                `Year Constitution`))


# --- 3e. Advocacy indicator ---
# advocacy = 1 if purpose is advocacy, youth, or pride parade
ngo_master <- ngo_master %>%
  mutate(advocacy = as.integer(purpose %in% c("advocacy", "youth",
                                                "pride parade")))

# --- 3f. Human rights framing ---
ngo_master <- ngo_master %>%
  mutate(hr_frame = as.integer(
    grepl("human right", mission_lower) | grepl("human right", org_lower)))


# --- 3g. Prepare for merge ---
ngo_master$uniqueid <- paste0(ngo_master$Organization, ngo_master$Country)

attributes <- ngo_master %>%
  distinct(uniqueid, .keep_all = TRUE) %>%
  select(uniqueid, purpose, advocacy, hr_frame, year_founded)

# Merge attributes into centrality data
ngo_data <- inner_join(centrality_all, attributes, by = "uniqueid")

cat(sprintf("After attribute merge: %d obs\n", nrow(ngo_data)))


################################################################################
# 4. COUNTRY-LEVEL CONTROLS
#    Merge Freedom House, GDP, EU membership, marriage laws, public
#    opinion (GAI), domestic NGO density. Apply Hoffman (2015)
#    grand-mean and country-mean centering.
################################################################################

# --- 4a. Main country controls (wide format) ---
controls_wide <- read_excel("raw_sources/04_country_controls.xlsx",
                            sheet = "Sheet1")
names(controls_wide) <- trimws(names(controls_wide))

# Reshape to long by extracting year-suffixed columns
years <- c(2008, 2015, 2018)

controls_long <- data.frame()
for (yr in years) {
  yr_str <- as.character(yr)
  row <- controls_wide %>%
    select(Country, EUMembershipDate, Religion,
           matches(paste0(yr_str, "$")))

  # Rename columns to remove year suffix
  names(row) <- gsub(paste0(yr_str, "$"), "", names(row))
  # Handle irregular column names
  names(row) <- gsub("^Domestic Associations ", "DomesticAssociations", names(row))
  names(row) <- gsub("^LGBTI Funding", "LGBTIFunding", names(row))
  names(row) <- trimws(names(row))

  row$year <- yr
  controls_long <- bind_rows(controls_long, row)
}

# --- 4b. Public opinion (GAI) ---
gai <- read_xls("raw_sources/05_gai_index.xls", sheet = "Sheet1")
gai <- gai %>%
  filter(!is.na(country), country != "") %>%
  mutate(Country = trimws(country)) %>%
  filter(year %in% c(2009, 2015, 2018)) %>%
  mutate(year = ifelse(year == 2009, 2008, year)) %>%
  rename(GAIScore = gai) %>%
  mutate(Country = ifelse(Country == "Bosnia Herzegovina",
                           "Bosnia and Herzegovina", Country)) %>%
  select(Country, year, GAIScore)

# --- 4c. Domestic NGO totals ---
ngo_totals <- read_dta("raw_sources/06_domestic_ngo_totals.dta")

# Reshape long
ngo_totals_long <- ngo_totals %>%
  pivot_longer(cols = starts_with("cum_lgbt_ngos"),
               names_to = "year_str", values_to = "cum_lgbt_ngos") %>%
  mutate(year = as.numeric(gsub("cum_lgbt_ngos", "", year_str))) %>%
  select(Country, year, cum_lgbt_ngos)


# --- 4d. Merge controls together ---
country_data <- controls_long %>%
  left_join(gai, by = c("Country", "year")) %>%
  left_join(ngo_totals_long, by = c("Country", "year"))


# --- 4e. Create derived variables ---
country_data <- country_data %>%
  mutate(
    # Ensure numeric
    GDPperCap = as.numeric(GDPperCap),
    Pop = as.numeric(Pop),
    FreedomHouse_Original = as.numeric(FreedomHouse_Original),
    DomesticAssociations = as.numeric(DomesticAssociations),
    marriage = as.numeric(marriage),
    marriageban = as.numeric(marriageban),
    employdisc = as.numeric(employdisc),
    civilunions = as.numeric(civilunions),

    # Log GDP
    log_GDPperCap = log(GDPperCap + 1),

    # Reverse-code Freedom House (higher = more democratic)
    freedom_house = 8 - FreedomHouse_Original,

    # Domestic association indicator
    DomesticAssociations = ifelse(is.na(DomesticAssociations), 0,
                                   DomesticAssociations),
    domestic_association = as.integer(DomesticAssociations > 0),

    # Domestic orgs per million population
    domestic_orgs_pop = (cum_lgbt_ngos / Pop) * 1000000,

    # Fix: civil unions should be 1 if marriage == 1
    civilunions = ifelse(marriage == 1, 1, civilunions),

    # Fix: Denmark employment discrimination
    employdisc = ifelse(Country == "Denmark" & year > 2007, 1, employdisc),
    employdisc = ifelse(Country == "Kosovo", 1, employdisc),
    employdisc = ifelse(Country == "Ukraine" & year > 2014, 1, employdisc)
  )


# --- 4f. EU status categories ---
eu15 <- c("Sweden", "Belgium", "Spain", "Italy", "Netherlands", "Denmark",
           "Portugal", "Germany", "Finland", "France", "Luxembourg",
           "United Kingdom", "Ireland", "Austria", "Greece")
eu12 <- c("Czech Republic", "Hungary", "Slovenia", "Slovakia", "Lithuania",
           "Romania", "Bulgaria", "Estonia", "Cyprus", "Latvia", "Malta",
           "Poland")
candidates_2008 <- c("Macedonia", "Turkey", "Croatia", "Serbia")
candidates_post2008 <- c("Macedonia", "Turkey", "Serbia")
candidates_post2014 <- c("Montenegro", "Albania")

country_data <- country_data %>%
  mutate(
    eu_status = case_when(
      Country %in% eu15 ~ "first-mover",
      Country == "Croatia" & year > 2008 ~ "late-adopter",
      Country %in% eu12 ~ "late-adopter",
      Country %in% candidates_2008 & year == 2008 ~ "candidate",
      Country %in% candidates_post2008 & year > 2008 ~ "candidate",
      Country %in% candidates_post2014 & year > 2014 ~ "candidate",
      TRUE ~ "other"
    ),
    # Encode as numeric factor matching Stata's encode order:
    # 1=candidate, 2=first-mover, 3=late-adopter, 4=other
    eu_id = case_when(
      eu_status == "candidate"    ~ 1,
      eu_status == "first-mover"  ~ 2,
      eu_status == "late-adopter" ~ 3,
      eu_status == "other"        ~ 4
    )
  )


# --- 4g. Grand-mean and country-mean centering (Hoffman 2015) ---
center_vars <- c("marriage", "marriageban", "GAIScore",
                  "domestic_association", "domestic_orgs_pop",
                  "freedom_house", "log_GDPperCap")

for (v in center_vars) {
  gm_name <- paste0("gm_", v)
  cm_name <- paste0("cm_", v)

  grand_mean <- mean(country_data[[v]], na.rm = TRUE)
  country_data[[gm_name]] <- country_data[[v]] - grand_mean

  country_means <- country_data %>%
    group_by(Country) %>%
    summarize(ctry_mean = mean(.data[[v]], na.rm = TRUE), .groups = "drop")

  country_data <- country_data %>%
    left_join(country_means, by = "Country") %>%
    mutate(!!cm_name := .data[[v]] - ctry_mean) %>%
    select(-ctry_mean)
}


# --- 4h. Align control years to centrality waves ---
# Controls measured 2008/2015/2018 -> aligned to centrality 2010/2017/2020
country_data <- country_data %>%
  mutate(year = case_when(
    year == 2008 ~ 2010,
    year == 2015 ~ 2017,
    year == 2018 ~ 2020
  ))

# Create merge key
country_data$countryid <- paste0(country_data$Country, country_data$year)

cat(sprintf("Country controls: %d country-years\n", nrow(country_data)))


################################################################################
# 5. MERGE AND FINAL VARIABLE CONSTRUCTION
################################################################################

# --- 5a. Merge org data with country controls ---
ngo_data$countryid <- paste0(ngo_data$Country, ngo_data$year)

df <- inner_join(
  ngo_data,
  country_data %>% select(countryid, eu_id, eu_status,
                           domestic_association, domestic_orgs_pop,
                           freedom_house, log_GDPperCap,
                           marriage, marriageban, GAIScore,
                           starts_with("gm_"), starts_with("cm_")),
  by = "countryid"
)

# --- 5b. Create additional variables ---
df <- df %>%
  mutate(
    country   = Country,
    orgid     = paste0(Organization, "_", Country),
    age       = year - year_founded,
    start_year = year - 2010
  )

# Encode org as numeric
df$org <- as.integer(factor(df$orgid))

# Grand-mean and country-mean center age and year
df <- df %>%
  mutate(gm_year = year - mean(year),
         gm_age  = age - mean(age, na.rm = TRUE))

df <- df %>%
  group_by(Country) %>%
  mutate(cm_year = year - mean(year),
         cm_age  = age  - mean(age, na.rm = TRUE)) %>%
  ungroup()


# --- 5c. Restrict to balanced panel ---
# Keep only associations present in all 3 waves
org_wave_counts <- df %>%
  group_by(orgid) %>%
  summarize(n_waves = n(), .groups = "drop")

balanced_orgs <- org_wave_counts %>% filter(n_waves == 3) %>% pull(orgid)
df <- df %>% filter(orgid %in% balanced_orgs)

# Drop observations with missing age
df <- df %>% filter(!is.na(gm_age))


# --- 5d. Create lagged dependent variables ---
df <- df %>%
  arrange(Organization, Country, year) %>%
  group_by(Organization, Country) %>%
  mutate(lagged_eigen  = lag(NGO_eigen),
         lagged_degree = lag(degree),
         lagged_counts = lag(ingo_counts)) %>%
  ungroup()


# --- 5e. Keep analysis variables only ---
keep_vars <- c("Organization", "Country", "country", "year", "countryid",
               "org", "orgid", "year_founded",
               "NGO_eigen", "degree", "ingo_counts",
               "lagged_eigen", "lagged_degree", "lagged_counts",
               "advocacy", "hr_frame", "eu_id", "eu_status",
               "gm_age", "cm_age", "gm_year", "cm_year", "start_year",
               "gm_marriage", "cm_marriage",
               "gm_marriageban", "cm_marriageban",
               "gm_GAIScore", "cm_GAIScore",
               "gm_domestic_association", "cm_domestic_association",
               "gm_domestic_orgs_pop", "cm_domestic_orgs_pop",
               "gm_freedom_house", "cm_freedom_house",
               "gm_log_GDPperCap", "cm_log_GDPperCap",
               "domestic_association", "domestic_orgs_pop",
               "freedom_house", "log_GDPperCap",
               "marriage", "marriageban", "GAIScore")
df <- df %>% select(all_of(keep_vars))

cat(sprintf("\nDataset constructed: %d obs, %d vars, %d countries, %d orgs\n",
            nrow(df), ncol(df),
            length(unique(df$Country)),
            length(unique(df$org))))


# ==============================================================================
# PART 2: ANALYSIS
# ==============================================================================
# If you skipped Part 1, uncomment the next line to load the pre-built dataset:
# df <- read_dta("gonsalves_velasco_2022_mob_main.dta")
# ==============================================================================


################################################################################
# 6. SETUP
################################################################################

# Prepare factor variables for models
df$advocacy_f   <- factor(df$advocacy, levels = c(0, 1))
df$hr_frame_f   <- factor(df$hr_frame, levels = c(0, 1))
df$eu_id_f      <- relevel(factor(df$eu_id), ref = "2")  # EU-15 as reference
df$country_f    <- factor(df$country)
df$org_f        <- factor(df$org)

cat("\nDataset ready:", nrow(df), "observations,",
    length(unique(df$country)), "countries,",
    length(unique(df$org)), "organizations\n\n")


################################################################################
# 7. TABLE 2: Longitudinal Multilevel Models Predicting LGBT Network
#    Centrality, 2010-2020
#
#    Three-level models: observations nested within organizations nested
#    within countries. REML estimation.
#
#    Model 1: Conditional baseline (year only)
#    Model 2: + organization- and country-level controls
#    Model 3: + within-country marriage law changes
#    Model 4: + between-country marriage laws
#    Model 5: + public opinion (GAI scores)
################################################################################

# --- Model 1: Conditional Baseline ---
cat("Estimating Model 1: Conditional Baseline...\n")
m1 <- lmer(NGO_eigen ~ gm_year +
             (1 | country_f) + (1 | org_f),
           data = df, REML = TRUE)

# --- Model 2: + Controls ---
cat("Estimating Model 2: + Controls...\n")
m2 <- lmer(NGO_eigen ~ gm_year +
             advocacy_f +
             gm_age + cm_age +
             hr_frame_f +
             gm_domestic_association + cm_domestic_association +
             gm_domestic_orgs_pop + cm_domestic_orgs_pop +
             gm_log_GDPperCap + cm_log_GDPperCap +
             gm_freedom_house + cm_freedom_house +
             (1 | country_f) + (1 | org_f),
           data = df, REML = TRUE)

# --- Model 3: Within-Country Marriage Laws ---
cat("Estimating Model 3: Within-Country Marriage Laws...\n")
m3 <- lmer(NGO_eigen ~ gm_year +
             gm_domestic_association + cm_domestic_association +
             advocacy_f +
             gm_age + cm_age +
             hr_frame_f +
             eu_id_f +
             gm_freedom_house +
             gm_domestic_orgs_pop +
             gm_log_GDPperCap +
             cm_marriage + cm_marriageban +
             cm_freedom_house + cm_domestic_orgs_pop + cm_log_GDPperCap +
             (1 | country_f) + (1 | org_f),
           data = df, REML = TRUE)

# --- Model 4: Between-Country Marriage Laws ---
cat("Estimating Model 4: Between-Country Marriage Laws...\n")
m4 <- lmer(NGO_eigen ~ gm_year +
             gm_domestic_association + cm_domestic_association +
             advocacy_f +
             gm_age + cm_age +
             hr_frame_f +
             eu_id_f +
             gm_freedom_house +
             gm_domestic_orgs_pop +
             gm_log_GDPperCap +
             gm_marriage + gm_marriageban +
             cm_freedom_house + cm_domestic_orgs_pop + cm_log_GDPperCap +
             (1 | country_f) + (1 | org_f),
           data = df, REML = TRUE)

# --- Model 5: Public Opinion ---
cat("Estimating Model 5: Public Opinion...\n")
m5 <- lmer(NGO_eigen ~ gm_year +
             gm_domestic_association + cm_domestic_association +
             advocacy_f +
             gm_age + cm_age +
             hr_frame_f +
             gm_GAIScore +
             eu_id_f +
             gm_freedom_house +
             gm_domestic_orgs_pop +
             gm_log_GDPperCap +
             cm_GAIScore +
             cm_freedom_house + cm_domestic_orgs_pop + cm_log_GDPperCap +
             (1 | country_f) + (1 | org_f),
           data = df, REML = TRUE)


# --- Display Results ---
cat("\n\n")
cat("================================================================\n")
cat("  TABLE 2: Multilevel Models Predicting Network Centrality\n")
cat("================================================================\n\n")

for (i in 1:5) {
  model <- get(paste0("m", i))
  cat(sprintf("\n--- Model %d ---\n", i))
  cat(sprintf("N observations: %d\n", nobs(model)))
  cat(sprintf("N organizations: %d\n", length(unique(model@frame$org_f))))
  cat(sprintf("N countries: %d\n", length(unique(model@frame$country_f))))
  print(summary(model)$coefficients, digits = 3)
  cat("\nRandom effects:\n")
  print(VarCorr(model))
  cat("\n")
}

# Combined table
cat("\n--- Combined Table 2 ---\n\n")
screenreg(list(m1, m2, m3, m4, m5),
          custom.model.names = paste("Model", 1:5),
          digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          symbol = "+")

# Save to HTML
htmlreg(list(m1, m2, m3, m4, m5),
        file = "table2.html",
        custom.model.names = paste("Model", 1:5),
        digits = 3,
        stars = c(0.001, 0.01, 0.05, 0.1),
        symbol = "+",
        caption = "Table 2: Multilevel Models Predicting Network Centrality")
cat("Table 2 saved to table2.html\n")


################################################################################
# 8. FIGURE 1: Average Centrality Scores by Country, 2010 and 2020
################################################################################

country_means <- df %>%
  filter(year %in% c(2010, 2020)) %>%
  group_by(Country, year) %>%
  summarize(mean_eigen = mean(NGO_eigen, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = mean_eigen,
              names_prefix = "eigen_") %>%
  arrange(eigen_2020)

country_long <- country_means %>%
  pivot_longer(cols = starts_with("eigen_"),
               names_to = "year", values_to = "mean_eigen",
               names_prefix = "eigen_") %>%
  mutate(year = factor(year, levels = c("2010", "2020")),
         Country = factor(Country, levels = country_means$Country))

fig1 <- ggplot(country_long, aes(x = Country, y = mean_eigen, fill = year)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("2010" = "gray60", "2020" = "gray20"),
                    name = "") +
  labs(title = "Average Centrality by Country",
       x = "",
       y = "Mean Eigenvector Centrality") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 6))

ggsave("figure1.pdf", fig1, width = 8, height = 12)
cat("Figure 1 saved to figure1.pdf\n")


################################################################################
# 9. TABLE 1: Fifteen Most Central Associations (Descriptive)
################################################################################

cat("\n\n")
cat("================================================================\n")
cat("  TABLE 1: Top 15 Most Central Associations\n")
cat("================================================================\n")

for (yr in c(2010, 2017, 2020)) {
  cat(sprintf("\n=== Top 15 Most Central Associations, %d ===\n", yr))
  top15 <- df %>%
    filter(year == yr) %>%
    arrange(desc(NGO_eigen)) %>%
    select(Organization, Country, NGO_eigen) %>%
    head(15)
  print(as.data.frame(top15), row.names = FALSE)
  cat("\n")
}


################################################################################
# END OF REPLICATION FILE
################################################################################

cat("\n\n")
cat("============================================================\n")
cat("  Replication complete.\n")
cat("============================================================\n")
