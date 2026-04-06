********************************************************************************
* Replication Do-File for:
*
* Velasco, Kristopher. 2020. "A Growing Queer Divide: The Divergence
*   between Transnational Advocacy Networks and Foreign Aid in Diffusing
*   LGBT Policies." International Studies Quarterly 64(1): 120-132.
*   https://doi.org/10.1093/isq/sqz075
*
* Author: Kristopher Velasco
* Date Created: April 2026
*
* OVERVIEW:
*   PART 1: DATA CONSTRUCTION DOCUMENTATION (Sections 1-4)
*     Documents the full pipeline. NOT executable without raw source data.
*     The analysis dataset (velasco_2020_isq_main.dta) is provided pre-built.
*
*   PART 2: ANALYSIS (Sections 5-8)
*     Replicates all tables and figures.
*     >>> TO REPLICATE, START HERE (search "PART 2") <<<
*
* HOW TO USE:
*   This repository contains two files:
*     velasco_2020_isq_main.dta           Analysis dataset
*     velasco_2020_isq_replication.do     This file
*
*   Part 1 documents how the analysis dataset was constructed from raw
*   source datasets. Part 2 is executable and replicates all published
*   tables, figures, and supplementary analyses.
*
*   For access to any individual raw source dataset described in Part 1,
*   please contact the author: Kristopher Velasco (kvelasco@princeton.edu).
*
* RAW SOURCE DATASETS (available from author upon request):
*   Copies stored in raw_sources/ subfolder (not uploaded):
*
*   raw_sources/01_foreign_aid_world_bank.xlsx     Foreign aid (World Bank)
*   raw_sources/02_chinese_aid_aiddata.xlsx        Chinese aid (AidData v1.0)
*   raw_sources/03_world_bank_indicators.xlsx      WDI controls
*   raw_sources/04_ilga_policy_data.xlsx           ILGA policy indicators
*   raw_sources/05_women_in_parliament.dta         V-Dem women in parliament
*   raw_sources/06_lingos_lgbti_pressure.dta       LGBTI INGOs & global pressure
*   raw_sources/07_kof_globalization_index.dta     KOF Globalization Index
*   raw_sources/08_freedom_house.dta               Freedom House ratings
*   raw_sources/09_lgbti_network_centrality.dta    LGBTI INGO network centrality
*   raw_sources/10_lgbti_aid_discourse.dta         Aid conditionality discourse
*   raw_sources/11_oecd_bilateral_aid.dta          OECD bilateral aid (cleaned)
*   raw_sources/12_oecd_bilateral_aid_raw.csv      OECD bilateral aid (raw)
*
* ANALYSIS DATASET VARIABLES (26 variables):
*   Identifiers:
*     country, recipient (country name variants), year, countryid
*
*   Dependent Variable:
*     policy_index      LGBT Policy Index. Additive index of 16 policies
*                       coded +1 (progressive) or -1 (regressive).
*                       See Table 1 in paper for full list.
*
*   Main Independent Variables:
*     global_lgbt_context   Global LGBT Context. Latent factor score from
*                           CFA of: cumulative global LGBTI INGOs,
*                           cumulative UN SOGI statements, cumulative
*                           global newspaper mentions. Scaled to LINGO count.
*
*     Transnational Advocacy Network Measures:
*       z_lgbt_inc_score      TAN INC Score (z-scored eigenvector centrality
*                             in LGBTI INGO network, from YIO)
*       z_lgbt_indegree       TAN In-Degree (z-scored count of LGBTI INGO
*                             ties, from YIO)
*
*     Foreign Aid Measures:
*       z_percent_oda_ln      Net ODA as % GNI (z-scored, logged)
*       z_oecd_donor_indegree Aid In-Degree (z-scored count of OECD donors
*                             providing bilateral aid)
*
*   Control Variables:
*     other_donors            Count of non-OECD bilateral aid donors
*     women_parliament        % women in national parliament (V-Dem)
*     demo_inverse_FH         Level of democracy (inverted Freedom House)
*     loggdp_cap              GDP per capita (logged, World Bank)
*     logpopden               Population density (logged, World Bank)
*     cultural_globalization  KOF Cultural Globalization Index
*
*   Unstandardized Versions (for reference):
*     lgbt_inc_score, lgbt_indegree, oecd_donor_indegree, percent_oda_ln
*
*   Figure 1 Policy Counts:
*     count_samesex_legal, count_employ_discrim, count_hate_crimes,
*     count_marriage, count_propaganda
*
*   Sample: 110 non-OECD countries, 1990-2016. N = 2,826 (2,742 for
*           ODA models due to missing values).
*
* REQUIRED PACKAGES:
*   ssc install outreg2
*   ssc install combomarginsplot
*
********************************************************************************


clear all
set more off
version 13


* ==============================================================================
* PART 1: DATA CONSTRUCTION DOCUMENTATION
* ==============================================================================
* NOT executable without raw source datasets.
* To replicate analyses, skip to PART 2.
* ==============================================================================

/*

The analysis dataset was constructed through four sequential stages.

--------------------------------------------------------------------------------
STAGE 1: Foreign Aid Data Compilation (0a. Merging Foreign Aid Datasets.do)

  Merged three sources of foreign aid data:
  a) World Bank: Net bilateral aid flows from DAC countries, EU institutions,
     UK, US, UN agencies. (Foreign Aid Data from World Bank.xlsx)
  b) AidData: Chinese Global Official Finance Dataset v1.0, including ODA,
     OOF, and vague financial flows. (GlobalChineseOfficialFinanceDataset)
  c) US Government: US foreign aid from foreignassistance.gov

  Processing: Standardized country names, collapsed to country-year,
  filtered to 139 countries with pop > 500,000 in 2000, set missing = 0
  (no aid received).

  Output: foreign aid_total_merged.dta

--------------------------------------------------------------------------------
STAGE 2: Master Merge (0b. Merging Foreign Aid and other Datasets.do)

  Base: World Bank Development Indicators (GDP, population, etc.)
  Sequential 1:1 merges on country-year for:
  - ILGA policy indicators (16 LGBT laws)
  - Women in parliament (V-Dem)
  - LGBTI INGO data (global counts, country ties)
  - KOF Globalization Index
  - Freedom House ratings
  - LGBTI network centrality (eigenvector scores from YIO)
  - OECD bilateral aid (cleaned donor counts and amounts)

  Output: Foreign Aid and Controls.dta

--------------------------------------------------------------------------------
STAGE 3: Variable Cleaning (0c. Cleaning Data v2.do)

  a) Constructed dependent variable:
     policy_index = sum of 16 policies * (+1/-1) weights
     Includes: same-sex acts legal/illegal, death penalty, age of consent,
     propaganda laws, employment discrimination, constitutional protections,
     hate crimes, incitement to hatred, marriage equality/ban, civil unions,
     partial unions, joint adoption, LGB military/ban.

  b) Constructed Global LGBT Context via CFA (SEM):
     Manifest indicators: cumulative global LINGOs, cumulative UN SOGI
     documents, cumulative global newspaper mentions of LGBTI communities.
     Scaled to cumulative LINGO count. Shifted to minimum = 0.

  c) Created control variables:
     loggdp_cap = ln(GDP per capita + 1)
     logpopden = ln(population density + 1)
     other_donors = total bilateral donors - OECD donors
     percent_oda_ln = ln(ODA as % GNI + 1)

  d) Missing data: Forward/backward fill for time-varying controls,
     interpolation for global-level variables.

  e) Sample: Restricted to non-OECD recipients, pop > 500,000,
     years 1990-2016. Dropped obs with missing INGO ties or policy index.

  f) Z-scored key independent variables for comparability:
     z_lgbt_inc_score, z_lgbt_indegree, z_percent_oda_ln,
     z_oecd_donor_indegree

  Output: Foreign Aid and Controls_Clean.dta

--------------------------------------------------------------------------------
STAGE 4: Trim to Analysis Variables

  keep country year recipient countryid ///
      policy_index global_lgbt_context ///
      z_lgbt_inc_score z_lgbt_indegree ///
      z_percent_oda_ln z_oecd_donor_indegree ///
      lgbt_inc_score lgbt_indegree oecd_donor_indegree percent_oda_ln ///
      oda_cap_ln other_donors women_parliament demo_inverse_FH ///
      loggdp_cap logpopden cultural_globalization ///
      count_samesex_legal count_employ_discrim count_hate_crimes ///
      count_marriage count_propaganda

  compress
  save "velasco_2020_isq_main.dta", replace

  Result: 2,826 observations x 26 variables.

*/


* ==============================================================================
* PART 2: ANALYSIS
* ==============================================================================
* Place both files in the same directory and set the path below.
* ==============================================================================


********************************************************************************
* 5. SETUP
********************************************************************************

* USERS: Set this to the directory containing the replication files.
* cd "/path/to/replication/files"

use "velasco_2020_isq_main.dta", clear
xtset countryid year, yearly


********************************************************************************
* 6. FIGURE 1: Number of Non-OECD Countries with Selected LGBT Policies
********************************************************************************

twoway ///
	(mspline count_samesex_legal year, ///
		title("Changes in Selected LGBT Policies, 1990-2016", ///
			size(medlarge) color(black)) ///
		ytitle("Number of Non-OECD States with Specified Policy") ///
		legend(label(1 "Same-Sex Acts Legal") size(vsmall) cols(3) ///
			symxsize(*.5))) ///
	(mspline count_employ_discrim year, ///
		legend(label(2 "Employment Discrimination Ban"))) ///
	(mspline count_hate_crimes year, ///
		legend(label(3 "Hate Crime Protections"))) ///
	(mspline count_marriage year, ///
		legend(label(4 "Same-Sex Marriage"))) ///
	(mspline count_propaganda year, ///
		legend(label(5 "Anti-Propaganda Laws")))
graph export "figure1.pdf", replace


********************************************************************************
* 7. TABLE 2: Two-Way Fixed-Effects Models Predicting LGBT Policy Index
*    Country + year fixed effects, country-clustered robust SEs.
*    All IVs lagged one year (pre-computed in dataset).
********************************************************************************

* --- Model 1: Baseline (Global Context) ---
xtreg policy_index c.global_lgbt_context ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Table 2: Two-Way FE Models Predicting LGBT Policy Index")

* --- Model 2a: TAN INC Score (no interaction) ---
xtreg policy_index c.global_lgbt_context c.z_lgbt_inc_score ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 2b: TAN INC Score (with interaction) ---
xtreg policy_index c.global_lgbt_context##c.z_lgbt_inc_score ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 3a: TAN In-Degree (no interaction) ---
xtreg policy_index c.global_lgbt_context c.z_lgbt_indegree ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 3b: TAN In-Degree (with interaction) ---
xtreg policy_index c.global_lgbt_context##c.z_lgbt_indegree ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 4a: Foreign Aid Net ODA (no interaction) ---
xtreg policy_index c.global_lgbt_context c.z_percent_oda_ln ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 4b: Foreign Aid Net ODA (with interaction) ---
xtreg policy_index c.global_lgbt_context##c.z_percent_oda_ln ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 5a: Foreign Aid In-Degree (no interaction) ---
xtreg policy_index c.global_lgbt_context c.z_oecd_donor_indegree ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 5b: Foreign Aid In-Degree (with interaction) ---
xtreg policy_index c.global_lgbt_context##c.z_oecd_donor_indegree ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization ///
	i.year, fe vce(cluster recipient)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)


********************************************************************************
* 8. FIGURE 2: Average Marginal Effects
*    Plots marginal effects of TAN and Foreign Aid measures at different
*    levels of Global LGBT Context. Combined into one figure.
*    Note: These models omit i.year to avoid collinearity with the
*    year-level global_lgbt_context variable in the margins calculation.
********************************************************************************

* TAN INC Score interaction
xtreg policy_index c.global_lgbt_context##c.z_lgbt_inc_score ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization, ///
	fe vce(cluster recipient)
margins, dydx(z_lgbt_inc_score) at(global_lgbt_context=(0(2)28)) ///
	saving(lgbt_ingo, replace)
marginsplot

* TAN In-Degree interaction
xtreg policy_index c.global_lgbt_context##c.z_lgbt_indegree ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization, ///
	fe vce(cluster recipient)
margins, dydx(z_lgbt_indegree) at(global_lgbt_context=(0(2)28)) ///
	saving(lgbt_ingo2, replace)
marginsplot

* Foreign Aid Net ODA interaction
xtreg policy_index c.global_lgbt_context##c.z_percent_oda_ln ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization, ///
	fe vce(cluster recipient)
margins, dydx(z_percent_oda_ln) at(global_lgbt_context=(0(2)28)) ///
	saving(aid, replace)
marginsplot

* Foreign Aid In-Degree interaction
xtreg policy_index c.global_lgbt_context##c.z_oecd_donor_indegree ///
	other_donors women_parliament demo_inverse_FH ///
	loggdp_cap logpopden cultural_globalization, ///
	fe vce(cluster recipient)
margins, dydx(z_oecd_donor_indegree) at(global_lgbt_context=(0(2)28)) ///
	saving(aid2, replace)
marginsplot

* Combined Figure 2
combomarginsplot aid aid2 lgbt_ingo lgbt_ingo2, noci
graph export "figure2.pdf", replace


********************************************************************************
* END OF REPLICATION FILE
********************************************************************************

display _newline(2)
display "============================================================"
display "  Replication complete."
display "============================================================"
