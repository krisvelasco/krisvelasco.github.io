********************************************************************************
* Replication Do-File for:
*
* Velasco, Kristopher. 2023. "Transnational Backlash and the
*   Deinstitutionalization of Liberal Norms: LGBTI Rights in a Contested
*   World." American Journal of Sociology 128(5): 1381-1429.
*   https://doi.org/10.1086/724724
*
* Author: Kristopher Velasco
* Date Created: April 2026
*
* OVERVIEW:
*   This file is organized in two parts:
*
*   PART 1: DATA CONSTRUCTION DOCUMENTATION (Sections 1-6)
*     Documents the full pipeline used to construct the analysis dataset,
*     from raw data import through variable construction. This section
*     is provided for transparency and is NOT executable without the
*     original raw source datasets (described below). The final analysis
*     dataset (velasco_2023_ajs_main.dta) is provided pre-built.
*
*   PART 2: ANALYSIS (Sections 7-14)
*     Replicates all tables, figures, and appendix models from the
*     published article using the provided analysis dataset.
*     >>> TO REPLICATE, START HERE (search "PART 2") <<<
*
* HOW TO USE:
*   This repository contains two files:
*     velasco_2023_ajs_main.dta          Analysis dataset
*     velasco_2023_ajs_replication.do    This file
*
*   Part 1 documents how the analysis dataset was constructed from raw
*   source datasets. Part 2 (search "PART 2") is executable and replicates
*   all published tables, figures, and appendix models.
*
*   For access to any individual raw source dataset described in Part 1,
*   please contact the author: Kristopher Velasco (kvelasco@princeton.edu).
*
* RAW SOURCE DATASETS (used to construct the analysis dataset):
*   Available from the author upon request. Copies are stored in the
*   raw_sources/ subfolder (not uploaded to the repository) with the
*   following numbering corresponding to the data construction stages:
*
*   raw_sources/01_lgbti_policy_data.xlsx          Original LGBTI policy data
*   raw_sources/02_world_bank_controls.dta         World Bank WDI (base)
*   raw_sources/03_anti_lgbti_ingo_eigenvector.dta FAIC eigenvector centrality
*   raw_sources/04_anti_lgbti_ingo_indegree.dta    FAIC in-degree counts
*   raw_sources/05_pro_lgbti_ingo_eigenvector.dta  LINGO eigenvector centrality
*   raw_sources/06_pro_lgbti_ingo_indegree.dta     LINGO in-degree counts
*   raw_sources/07_global_lgbti_context.dta        Global context factor inputs
*   raw_sources/08_domestic_lgbti_ngos.dta         Domestic LGBTI organizations
*   raw_sources/09_vdem_v8_democracy.dta           V-Dem v8 (v-dem.net)
*   raw_sources/10_state_religion.dta              State religion indicators
*   raw_sources/11_colonial_power.dta              Colonial legacy data
*   raw_sources/12_charismatic_christians.dta      World Christian Database
*   raw_sources/13_lgbtq_mps.dta                   LGBTQ MPs (Reynolds 2013)
*   raw_sources/14_religious_characteristics.dta   RCS-Dem 2.0 (ARDA)
*   raw_sources/15_regional_pro_lgbti_norms.xlsx   Regional IGO norms
*   raw_sources/16_total_ingo_counts.dta           Total INGOs (Schofer)
*   raw_sources/17_anti_lgbti_demonstrations.dta   Anti-LGBTI demonstrations
*   raw_sources/18_domestic_anti_lgbti_orgs.dta    Domestic anti-LGBTI orgs
*   raw_sources/19_immigrant_stock.dta             UN immigrant stock
*   raw_sources/20_world_bank_extended.dta         WB extended controls
*   raw_sources/21_world_bank_hiv_aid.dta          HIV prevalence, foreign aid
*   raw_sources/22_ethnic_fractionalization.dta    Ethnic frac. (Drazanova)
*   raw_sources/23_lgbti_ngo_restrictions.dta      LGBTI NGO restrictions
*
* ANALYSIS DATASET VARIABLES:
*   Identifiers:
*     country, year, countrycode, region, uniqueid
*
*   Dependent Variables:
*     policy_index      Continuous LGBTI Policy Index (~-6 to +13).
*                       Constructed from 18 progressive and regressive
*                       policies weighted by scope, severity, and
*                       enforcement. See Section 1 for construction.
*     policy_change_3   Multinomial DV: 3-year pooled policy change.
*                       1=Contestation (both expansion & contraction),
*                       2=Contraction (defiance/backlash only),
*                       3=Expansion (compliance only),
*                       4=Status Quo (reference category).
*
*   Main Independent Variables:
*     lag_global_context   Global LGBTI Context. Latent factor score
*                          from INGO counts, UN statements, and
*                          newspaper volume. Rescaled to 0 in 1990.
*     z_e_tot_lingo        Pro-LGBTI network ties (z-scored in-degree
*                          of country ties to pro-LGBTI INGOs via
*                          Yearbook of International Organizations).
*     z_e_tot_faic         Anti-LGBTI network ties (z-scored in-degree
*                          of country ties to anti-LGBTI INGOs).
*
*   Control Variables (all lagged 1 year):
*     lag_regional_norms        Pro-LGBTI statements by regional IGOs
*     lag_cum_lgbti_ngo_ln      Domestic LGBTI movement (cum. orgs, ln)
*     lag_lgbtq_mps             LGBTQ members of parliament
*     lag_anti_demo             Anti-LGBTI demonstrations (cum.)
*     lag_ptpnpc                % Protestant (incl. Pentecostal)
*     lag_catpc                 % Catholic
*     lag_muspc                 % Muslim
*     lag_population_ln         Population (logged)
*     lag_trade                 Trade as % of GDP
*     lag_e_fh_ipolity2         Democracy (FH/Polity combined, V-Dem)
*     lag_gdppercap_ln          GDP per capita (logged, constant 2015 USD)
*
*   Appendix Controls:
*     lag_ngoi                  Total INGOs (Schofer data)
*     lag_policy_index          Lagged dependent variable
*     lag_relpc                 Overall religiosity
*     lag_imm_percent           % immigrant population
*     lag_fertility_rate        Fertility rate
*     lag_percent_urban         % urban population
*     lag_unemployment_ilo      Unemployment rate (ILO)
*     lag_teriary_enrollment    Tertiary education enrollment
*     lag_net_migration         Net migration
*     lag_hiv_percent           HIV prevalence
*     lag_unaids_ln             UNAIDS funding (logged)
*     lag_aid_ln                Net bilateral aid (logged)
*
*   Cross-Lagged Panel Variables:
*     e_tot_lingo, e_tot_faic   Unstandardized network ties
*     lag_internet_users         Internet users
*     lag_population_density_ln  Population density (logged)
*     lag_regional_norms_cum     Cumulative regional norms
*
*   Appendix Demographic Robustness:
*     efindex                   Ethnic fractionalization (Drazanova)
*     ngo_restriction           Indicator for LGBTI NGO restrictions
*     lag_ethnic                Lagged ethnic fractionalization
*     lag_restriction           Lagged NGO restriction indicator
*
*   Individual Policy Indicators (for CFA and Figure 1):
*     equal_age, unequal_age, constitution, conversion_therapies,
*     death_penalty, employment_discrim, gender_surgery, hate_crimes,
*     incite_hate, joint_adoption, lgb_military, lgb_military_ban,
*     marriage_equality, marriage_ban, samesex_legal, samesex_illegal,
*     third_gender, trans_military, civil_unions, gendermarker, propaganda
*
*   Sample Restriction:
*     pop_flag                  1 = avg. pop < 500,000 (excluded)
*
* REQUIRED PACKAGES:
*   ssc install outreg2
*   ssc install combomarginsplot
*
********************************************************************************


clear all
set more off
version 16


* ==============================================================================
* PART 1: DATA CONSTRUCTION DOCUMENTATION
* ==============================================================================
* This section documents how velasco_2023_ajs_main.dta was constructed.
* It is NOT executable without the original raw source datasets.
* To replicate the analyses, skip to PART 2 below.
* ==============================================================================

/*

The analysis dataset was constructed through six sequential stages:

--------------------------------------------------------------------------------
STAGE 1: LGBTI Policy Index Construction
  Source file: LGBTI Policy Data.xlsx (original data collection)

  Procedure:
  - Imported policy-level data on 18 LGBTI-related laws across 195 countries,
    1991-2018. Each law is coded by: proportion of population covered, severity
    of punishment (for criminal laws), evidence of enforcement, scope of
    gender coverage, and additional attributes (ease of access, benefit
    strength, fine indicators).

  - Created composite scores for four multi-attribute policy domains using
    structural equation modeling (SEM):
      * Sodomy criminalization: sexes covered, punishment, enforcement, fines,
        illegality proportion -> latent factor "Sodomy"
      * Civil unions: benefits strength, proportion, enforcement -> "Civil_Unions"
      * Gender marker change: ease of access, legality, enforcement -> "Gender_Marker"
      * Morality/propaganda laws: sexes covered, punishment, fines, proportion,
        enforcement -> "Propaganda"

  - Normalized factor scores to 0-1 range.

  - Constructed the LGBTI Policy Index as:
      policy_index = (sum of 15 progressive policies) - (sum of 6 regressive policies)
    where progressive policies include: equal age of consent, civil unions,
    constitutional protections, conversion therapy bans, employment discrimination
    protections, bans on gender assignment surgery on children, gender marker
    change, hate crime protections, incitement to hatred protections, joint
    adoption, LGB military service, marriage equality, same-sex act legality,
    third-gender recognition, transgender military service.
    Regressive policies include: unequal age of consent, death penalty, LGB
    military ban, marriage ban, same-sex act criminalization (factor-scored),
    morality/propaganda laws (factor-scored).

  Output: policy_index and 21 individual policy indicators per country-year.

--------------------------------------------------------------------------------
STAGE 2: Master Merge
  Base dataset: World Bank World Development Indicators (all country-years)

  Sequential merges (1:1 on country-year identifier):
    1. Anti-LGBTI INGO eigenvector centrality (FAIC, 1991-2018)
    2. Anti-LGBTI INGO in-degree counts
    3. Pro-LGBTI INGO eigenvector centrality
    4. Pro-LGBTI INGO in-degree counts
    5. LGBTI Policy Index (from Stage 1)
    6. Global LGBTI Context factor scores (year-level merge)
    7. Cumulative domestic LGBTI organizations
    8. V-Dem v8 democracy and governance indicators
    9. State religion data
   10. Colonial power legacy (country-level merge)
   11. World Christian Database charismatic populations
   12. LGBTQ members of parliament
   13. Religious Characteristics of States (RCS-Dem 2.0)

  Output: dissertation_master_unclean_v7.dta

--------------------------------------------------------------------------------
STAGE 3: Variable Cleaning and Transformation

  Key operations:
  a) Population filter: flagged countries with avg. pop < 500,000
  b) Linear interpolation of 30+ variables with missing values (religious
     demographics, GDP, internet users, population, trade, etc.)
  c) Carry-forward imputation for women in parliament and domestic NGO counts
  d) Separate interpolation of LINGO and FAIC network in-degree scores
  e) Z-score standardization of network measures (tot_lingo, tot_faic,
     e_tot_lingo, e_tot_faic)
  f) Log transformations: GDP per capita, population density, domestic NGOs,
     network ties
  g) Global LGBTI Context: SEM latent factor from global INGO counts,
     cumulative newspaper articles mentioning LGBTI communities, and UN
     documents on SOGI. Rescaled so 1990 = 0.
  h) One-year lags of all predictor variables

  Output: dissertation_master_clean_v7.dta

--------------------------------------------------------------------------------
STAGE 4: Policy Change Categories

  Created annual binary indicators for policy expansion, contraction, and
  status quo by comparing policy_index to its prior-year value. Pooled into
  3-year windows (1991-93, 1994-96, ..., 2015-17) and classified:
    - Expansion: at least one expansion, no contractions
    - Contraction: at least one contraction, no expansions
    - Contestation: both expansion and contraction within the window
    - Status Quo: no changes
  Encoded as policy_change_3 (factor variable, 1-4).

--------------------------------------------------------------------------------
STAGE 5: Post-AJS Reviewer Variables

  Added based on reviewer requests during the AJS review process:
  a) Regional pro-LGBTI norms: count of pro-LGBTI statements/decisions by
     regional IGOs (Council of Europe, OAS, ASEAN, AU, etc.)
  b) Total INGO counts (from Schofer's data, 1952-2018)
  c) Anti-LGBTI counter-mobilization: cumulative demonstrations, domestic
     anti-LGBTI organizations
  d) Demographic controls: immigrant stock (%), fertility rate, net migration,
     urban population (%), tertiary enrollment, unemployment (ILO)
  e) Health/aid controls: HIV prevalence, UNAIDS funding, bilateral DAC aid
  f) Ethnic fractionalization
  All new variables lagged one year.

  Output: dissertation_master_policy_v6.dta

--------------------------------------------------------------------------------
STAGE 6: Trim to Analysis Variables

  keep country year countrycode region uniqueid pop_flag ///
      policy_index policy_change_3 ///
      global_context lag_global_context ///
      e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
      lag_regional_norms lag_regional_norms_cum ///
      lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
      lag_ptpnpc lag_catpc lag_muspc ///
      lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
      lag_ngoi lag_policy_index lag_relpc ///
      lag_imm_percent lag_fertility_rate lag_percent_urban ///
      lag_unemployment_ilo lag_teriary_enrollment lag_net_migration ///
      lag_hiv_percent lag_unaids_ln lag_aid_ln ///
      lag_internet_users lag_population_density_ln ///
      equal_age unequal_age constitution conversion_therapies ///
      death_penalty employment_discrim gender_surgery hate_crimes ///
      incite_hate joint_adoption lgb_military lgb_military_ban ///
      marriage_equality marriage_ban samesex_legal samesex_illegal ///
      third_gender trans_military civil_unions gendermarker propaganda ///
      efindex ngo_restriction lag_ethnic lag_restriction

  compress
  save "velasco_2023_ajs_main.dta", replace

  Result: 5,760 observations x 65 variables.

*/


* ==============================================================================
* PART 2: ANALYSIS
* ==============================================================================
* This section replicates all tables, figures, and appendix models.
* Place all provided files in the same directory and set it below.
* ==============================================================================


********************************************************************************
* 7. SETUP
********************************************************************************

* USERS: Set this to the directory containing the replication files.
* cd "/path/to/replication/files"

use "velasco_2023_ajs_main.dta", clear
encode country, generate(countryid)
xtset countryid year


********************************************************************************
* 8. FIGURE 1: Global Expansion and Contraction of Select LGBTI Rights
*    Panel A: Progressive policies (mean proportion covered, by year)
*    Panel B: Regressive policies
********************************************************************************

preserve
	collapse (mean) marriage_equality marriage_ban propaganda ///
		employment_discrim lgb_military third_gender hate_crimes, by(year)

	* Panel B: Regressive
	twoway (connected propaganda year, sort(year)) ///
		   (connected marriage_ban year, sort(year)), ///
		   title("Regressive LGBTI Policies Over Time") ///
		   ytitle("Mean Proportion of Population Covered") xtitle("Year") ///
		   legend(order(1 "Morality/Propaganda Laws" 2 "Marriage Bans"))
	graph export "figure1_panelB.pdf", replace

	* Panel A: Progressive
	twoway (connected marriage_equality year, sort(year)) ///
		   (connected employment_discrim year, sort(year)) ///
		   (connected lgb_military year, sort(year)) ///
		   (connected third_gender year, sort(year)) ///
		   (connected hate_crimes year, sort(year)), ///
		   title("Progressive LGBTI Policies Over Time") ///
		   ytitle("Mean Proportion of Population Covered") xtitle("Year") ///
		   legend(order(1 "Marriage Equality" 2 "Employment Discrim." ///
			3 "LGB Military" 4 "Third Gender" 5 "Hate Crimes"))
	graph export "figure1_panelA.pdf", replace
restore


********************************************************************************
* 9. FIGURE 4: Change in the Global LGBTI Context Over Time
********************************************************************************

twoway line global_context year, sort(year) ///
	title("Change in Global LGBTI Context Over Time") ///
	ytitle("Global LGBTI Context (Factor Score)") xtitle("Year")
graph export "figure4.pdf", replace


********************************************************************************
* 10. TABLE 3: Pooled TSCS Predicting LGBTI Policy Index (Country FE)
*     Model 1: Controls only
*     Model 2: + Global LGBTI Context (linear + squared)
*     Model 3: + Pro- and Anti-LGBTI Networks (full model)
********************************************************************************

* Model 1: Controls Only
xtreg policy_index ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, fe vce(cluster countryid)
outreg2 using table3, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Table 3: Pooled TSCS Predicting LGBTI Policy Index (FE)")

* Model 2: + Global Context
xtreg policy_index ///
	c.lag_global_context##c.lag_global_context ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, fe vce(cluster countryid)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 3: Full Model (+ Networks)
xtreg policy_index ///
	c.lag_global_context##c.lag_global_context ///
	z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, fe vce(cluster countryid)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)


********************************************************************************
* 11. FIGURE 5: Margins Plot — Global Context on Policy Index
*     From Model 3. Other predictors held at means.
********************************************************************************

quietly xtreg policy_index ///
	c.lag_global_context##c.lag_global_context ///
	z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, fe vce(cluster countryid)

margins, at(lag_global_context = (0(5)40))
marginsplot, recast(line) recastci(rline) ciopts(lpattern(dash) lcolor(red)) ///
	title("Effects of Global LGBTI Context on LGBTI Policy Index") ///
	ytitle("Predicted LGBTI Policy Index") xtitle("Global LGBTI Context")
graph export "figure5.pdf", replace


********************************************************************************
* 12. TABLE 4: Multinomial Logistic Regressions Predicting Policy Change
*     DV: policy_change_3 (Status Quo = reference)
*     Columns: Compliance, Contestation, Defiance
********************************************************************************

mlogit policy_change_3 ///
	c.lag_global_context##c.lag_global_context ///
	z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, vce(cluster countryid) rrr
outreg2 using table4, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Table 4: Multinomial Logistic Regressions")


********************************************************************************
* 12b. FIGURE 6: Predicted Probabilities from Multinomials
*      Panel A: Global Context -> each outcome
*      Panel B: Pro/Anti Networks -> each outcome
********************************************************************************

quietly mlogit policy_change_3 ///
	c.lag_global_context##c.lag_global_context ///
	z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, vce(cluster countryid) rrr

* Panel A: Global Context
margins, at(lag_global_context = (0(1)40)) predict(outcome(1)) ///
	atmeans saving(contestation, replace) vsquish
margins, at(lag_global_context = (0(1)40)) predict(outcome(2)) ///
	atmeans saving(contraction, replace) vsquish
margins, at(lag_global_context = (0(1)40)) predict(outcome(3)) ///
	atmeans saving(expansion, replace) vsquish
combomarginsplot contestation contraction expansion, ///
	title("Predicted Probability by Global LGBTI Context") ///
	labels("Contestation" "Contraction" "Expansion")
graph export "figure6_panelA.pdf", replace

* Panel B: Network Effects
quietly mlogit policy_change_3 ///
	c.lag_global_context##c.lag_global_context ///
	z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, vce(cluster countryid) rrr

margins, at(z_e_tot_faic = (-2(1)4)) predict(outcome(3)) ///
	atmeans saving(anti_expansion, replace) vsquish
margins, at(z_e_tot_faic = (-2(1)4)) predict(outcome(2)) ///
	atmeans saving(anti_contraction, replace) vsquish
margins, at(z_e_tot_faic = (-2(1)4)) predict(outcome(1)) ///
	atmeans saving(anti_contestation, replace) vsquish
margins, at(z_e_tot_lingo = (-2(1)4)) predict(outcome(3)) ///
	atmeans saving(pro_expansion, replace) vsquish
margins, at(z_e_tot_lingo = (-2(1)4)) predict(outcome(2)) ///
	atmeans saving(pro_contraction, replace) vsquish
margins, at(z_e_tot_lingo = (-2(1)4)) predict(outcome(1)) ///
	atmeans saving(pro_contestation, replace) vsquish

combomarginsplot anti_expansion anti_contraction anti_contestation, ///
	noci title("Anti-LGBTI Networks: Predicted Probabilities") ///
	labels("Expansion" "Contraction" "Contestation")
graph export "figure6_panelB_anti.pdf", replace

combomarginsplot pro_expansion pro_contraction pro_contestation, ///
	noci title("Pro-LGBTI Networks: Predicted Probabilities") ///
	labels("Expansion" "Contraction" "Contestation")
graph export "figure6_panelB_pro.pdf", replace


********************************************************************************
* 13. FIGURE 7: Cross-Lagged Panel Models
*     Wave 1 = 1995, Wave 2 = 2015. SEM with MLMV.
*     Primary model + alternative model with total INGOs.
********************************************************************************

use "velasco_2023_ajs_main.dta", clear
drop if pop_flag == 1

keep lag_global_context lag_anti_demo policy_index ///
	e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
	lag_cum_lgbti_ngo_ln lag_relpc lag_gdppercap_ln ///
	lag_internet_users lag_population_density_ln lag_population_ln ///
	lag_trade lag_e_fh_ipolity2 lag_lgbtq_mps ///
	lag_ngoi lag_regional_norms lag_regional_norms_cum ///
	country year

reshape wide lag_global_context policy_index ///
	e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
	lag_cum_lgbti_ngo_ln lag_relpc lag_gdppercap_ln ///
	lag_internet_users lag_population_density_ln lag_population_ln ///
	lag_trade lag_e_fh_ipolity2 lag_lgbtq_mps ///
	lag_ngoi lag_regional_norms lag_regional_norms_cum lag_anti_demo, ///
	i(country) j(year)

* Primary: Policy Index, Anti Networks, Pro Networks
sem (policy_index2015 <- policy_index1995 z_e_tot_faic1995 z_e_tot_lingo1995 ///
	lag_anti_demo1995 lag_cum_lgbti_ngo_ln1995 lag_relpc1995 ///
	lag_gdppercap_ln1995 lag_population_ln1995 lag_trade1995 ///
	lag_e_fh_ipolity21995 lag_lgbtq_mps1995) ///
    (z_e_tot_faic2015 <- policy_index1995 z_e_tot_faic1995 z_e_tot_lingo1995 ///
	lag_anti_demo1995 lag_cum_lgbti_ngo_ln1995 lag_relpc1995 ///
	lag_gdppercap_ln1995 lag_population_ln1995 lag_trade1995 ///
	lag_e_fh_ipolity21995 lag_lgbtq_mps1995) ///
    (z_e_tot_lingo2015 <- policy_index1995 z_e_tot_faic1995 z_e_tot_lingo1995 ///
	lag_anti_demo1995 lag_cum_lgbti_ngo_ln1995 lag_relpc1995 ///
	lag_gdppercap_ln1995 lag_population_ln1995 lag_trade1995 ///
	lag_e_fh_ipolity21995 lag_lgbtq_mps1995), ///
	cov(e.policy_index2015*e.z_e_tot_lingo2015) ///
	cov(e.policy_index2015*e.z_e_tot_faic2015) ///
	cov(e.z_e_tot_lingo2015*e.z_e_tot_faic2015) ///
	method(mlmv)
outreg2 using table_crosslagged, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Figure 7: Cross-Lagged Panel Model")

* Alternative: Total INGOs
sem (policy_index2015 <- policy_index1995 lag_ngoi1995 lag_anti_demo1995 ///
	lag_population_ln1995 lag_cum_lgbti_ngo_ln1995 lag_relpc1995 ///
	lag_gdppercap_ln1995 lag_trade1995 lag_e_fh_ipolity21995 ///
	lag_lgbtq_mps1995) ///
    (lag_ngoi2015 <- policy_index1995 lag_ngoi1995 lag_anti_demo1995 ///
	lag_population_ln1995 lag_cum_lgbti_ngo_ln1995 lag_relpc1995 ///
	lag_gdppercap_ln1995 lag_trade1995 lag_e_fh_ipolity21995 ///
	lag_lgbtq_mps1995), ///
	cov(e.policy_index2015*e.lag_ngoi2015) ///
	method(mlmv)
outreg2 using table_crosslagged, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)


********************************************************************************
* 14. ONLINE APPENDIX
********************************************************************************

use "velasco_2023_ajs_main.dta", clear
encode country, generate(countryid)
xtset countryid year

* --- 14a. Alternative INGO Measure: Total INGO Counts ---
xtreg policy_index ///
	c.lag_global_context##c.lag_global_context lag_ngoi ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, fe vce(cluster countryid)
outreg2 using appendix_auxiliary, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Online Appendix: Auxiliary Models")

mlogit policy_change_3 ///
	c.lag_global_context##c.lag_global_context lag_ngoi ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc lag_aid_ln ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, vce(cluster countryid) rrr
outreg2 using appendix_auxiliary, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- 14b. Lagged Dependent Variable ---
xtreg policy_index lag_policy_index ///
	c.lag_global_context##c.lag_global_context z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, fe vce(cluster countryid)
outreg2 using appendix_auxiliary, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

mlogit policy_change_3 lag_policy_index ///
	c.lag_global_context##c.lag_global_context z_e_tot_lingo z_e_tot_faic ///
	c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, vce(cluster countryid) rrr
outreg2 using appendix_auxiliary, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- 14c. Arellano-Bond GMM ---
gen context_sq = lag_global_context * lag_global_context

xtabond policy_index ///
	lag_global_context context_sq z_e_tot_lingo z_e_tot_faic ///
	lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
	lag_ptpnpc lag_catpc lag_muspc ///
	lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln ///
	if pop_flag != 1, vce(robust)
outreg2 using appendix_gmm, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Online Appendix: Arellano-Bond GMM")

* --- 14d. Demographic Controls (11 additional variables) ---
* lag_ethnic and lag_restriction are pre-computed in the analysis dataset
* (from ethnic fractionalization and LGBTI NGO restriction indicators).

encode country, generate(countryid2)
xtset countryid2 year

foreach var in lag_imm_percent lag_fertility_rate lag_percent_urban ///
	lag_unemployment_ilo lag_teriary_enrollment lag_net_migration ///
	lag_hiv_percent lag_unaids_ln lag_aid_ln lag_ethnic lag_restriction {

	display _newline(2)
	display "=== Continuous FE with: `var' ==="
	xtreg policy_index ///
		c.lag_global_context##c.lag_global_context z_e_tot_lingo z_e_tot_faic ///
		c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
		lag_ptpnpc lag_catpc lag_muspc ///
		lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln `var' ///
		if pop_flag != 1, fe vce(cluster countryid2)
	outreg2 using appendix_demographic_fe, excel append dec(3) ///
		alpha(.001, .01, .05, .1) symbol(***, **, *, +)
}

foreach var in lag_imm_percent lag_fertility_rate lag_percent_urban ///
	lag_unemployment_ilo lag_teriary_enrollment lag_net_migration ///
	lag_hiv_percent lag_unaids_ln lag_aid_ln lag_ethnic lag_restriction {

	display _newline(2)
	display "=== Multinomial with: `var' ==="
	mlogit policy_change_3 ///
		c.lag_global_context##c.lag_global_context z_e_tot_lingo z_e_tot_faic ///
		c.lag_regional_norms lag_cum_lgbti_ngo_ln lag_lgbtq_mps lag_anti_demo ///
		lag_ptpnpc lag_catpc lag_muspc ///
		lag_population_ln lag_trade lag_e_fh_ipolity2 lag_gdppercap_ln `var' ///
		if pop_flag != 1, vce(cluster countryid2) rrr
	outreg2 using appendix_demographic_mlogit, excel append dec(3) ///
		alpha(.001, .01, .05, .1) symbol(***, **, *, +)
}

* --- 14e. CFA for LGBTI Policy Index ---
use "velasco_2023_ajs_main.dta", clear

sem (Index -> equal_age unequal_age constitution conversion_therapies ///
	death_penalty employment_discrim gender_surgery hate_crimes ///
	incite_hate joint_adoption lgb_military lgb_military_ban ///
	marriage_equality marriage_ban samesex_legal third_gender ///
	trans_military samesex_illegal civil_unions gendermarker propaganda), ///
	cov(e.equal_age*e.unequal_age) ///
	cov(e.marriage_ban*e.marriage_equality) ///
	cov(e.samesex_illegal*e.samesex_legal)
outreg2 using appendix_cfa, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Online Appendix: CFA for LGBTI Policy Index")


********************************************************************************
* END OF REPLICATION FILE
********************************************************************************

display _newline(2)
display "============================================================"
display "  Replication complete."
display "  Output files saved in the current working directory."
display "============================================================"
