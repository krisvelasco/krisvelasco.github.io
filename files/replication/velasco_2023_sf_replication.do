********************************************************************************
* Replication Do-File for:
*
* Velasco, Kristopher. 2023. "Opposition Avoidance or Mutual Engagement?
*   The Interdependent Dynamics between Opposing Transnational LGBT+
*   Networks." Social Forces 101(4): 2087-2116.
*   https://doi.org/10.1093/sf/soac068
*
* Author: Kristopher Velasco
* Date Created: April 2026
*
* OVERVIEW:
*   PART 1: DATA CONSTRUCTION DOCUMENTATION (Sections 1-7)
*     Documents the full pipeline used to construct the analysis dataset.
*     NOT executable without original raw source datasets (described below).
*     The analysis dataset (velasco_2023_sf_main.dta) is provided pre-built.
*
*   PART 2: ANALYSIS (Sections 8-14)
*     Replicates all tables, figures, and appendix models.
*     >>> TO REPLICATE, START HERE (search "PART 2") <<<
*
* HOW TO USE:
*   This repository contains two files:
*     velasco_2023_sf_main.dta           Analysis dataset
*     velasco_2023_sf_replication.do     This file
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
*   --- Shared pipeline (Stages 1-4, same as AJS 2023) ---
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
*   raw_sources/21_ethnic_fractionalization.dta    Ethnic frac. (Drazanova)
*   --- SF-specific datasets (Stage 5) ---
*   raw_sources/22_corrected_anti_lgbti_ingo_counts.dta  Corrected anti counts
*   raw_sources/23_corrected_pro_lgbti_ingo_counts.dta   Corrected pro counts
*   raw_sources/24_secondary_education.dta         Secondary enrollment (WB)
*   raw_sources/25_wcf_only_anti_lgbti_counts.dta  WCF-affiliated INGOs only
*   raw_sources/26_global_anti_lgbti_context.dta   Global anti context inputs
*   raw_sources/27_regional_conferences.dta        Regional conference data
*   raw_sources/28_un_votes_lgbti.dta              UN votes on LGBTI issues
*   raw_sources/29_ingo_founding_counts.xlsx       INGO founding/membership
*   Note: V-Dem v11.1 (for v2xcs_ccsi) available from v-dem.net
*
* ANALYSIS DATASET VARIABLES:
*   Identifiers:
*     country, year, countryid, countrycode, region
*
*   Dependent Variables (Network Embeddedness):
*     pro_2          Country ties to pro-LGBT+ INGOs (interpolated count)
*     anti_2         Country ties to anti-LGBT+ INGOs (interpolated count)
*     e_tot_lingo    Pro-LGBT+ network embeddedness (in-degree)
*     e_tot_faic     Anti-LGBT+ network embeddedness (in-degree)
*     z_e_tot_lingo  Standardized pro-LGBT+ embeddedness
*     z_e_tot_faic   Standardized anti-LGBT+ embeddedness
*     wcf            WCF-affiliated anti-LGBT+ INGOs (alternative measure)
*     anti_limited   Limited anti-LGBT+ count (alternative measure)
*
*   Transnational Normative Environments:
*     lag_global_context   Global pro-LGBT+ context (factor score from
*                          INGO counts, UN statements, newspaper volume)
*     lag_AntiContext       Global anti-LGBT+ context (factor score from
*                          total anti ties, WCF conferences, UN statements,
*                          consultative status organizations)
*     lag_Anti_Region      Regional anti-LGBT+ context (factor score from
*                          avg regional anti ties, regional anti orgs,
*                          WCF conferences held in region)
*     lag_Pro_Region       Regional pro-LGBT+ context (factor score from
*                          avg regional pro ties, ILGA conferences,
*                          cumulative regional pro-LGBT norms)
*
*   Controls (lagged 1 year):
*     lag_policy_index          LGBTI Policy Index
*     policy_change_up          Binary: progressive policy change
*     policy_change_down        Binary: regressive policy change
*     lag_cum_lgbti_ngo_ln      Domestic LGBT+ movement (cum. orgs, ln)
*     lag_anti_demo             Anti-LGBT+ demonstrations
*     lag_relpc                 Religiosity (% religious)
*     lag_fertility_rate        Fertility rate
*     lag_population_ln         Population (logged)
*     lag_efindex               Ethnic fractionalization
*     post2008                  Post-2008 financial crisis indicator
*     lag_e_fh_ipolity2         Democracy (FH/Polity, V-Dem)
*     lag_gdppercap_ln          GDP per capita (logged)
*     ingo_ln                   Total INGOs (logged)
*     lag_v2xcs_ccsi            Civil society index (V-Dem v11)
*     lag_secondary_percent2    Secondary education enrollment
*
*   Lagged Dependent Variables:
*     lag5_pro_2, lag5_anti_2, lag5_wcf, lag5_anti_limited (5-year lags)
*     lag_pro_2, lag_anti_2, lag_wcf, lag_anti_limited (1-year lags)
*
*   Cross-Lagged Panel Variables (unlagged, for reshape to wide):
*     policy_index, cum_lgbti_ngo_ln, anti_demo, fertility_rate,
*     imm_percent, population_ln, unemployment_ilo, efindex, trade,
*     e_fh_ipolity2, gdppercap_ln, v2xcs_ccsi, relpc, secondary_percent2,
*     Anti_Region, Pro_Region
*
*   INGO Founding Data (for Figures 3 and Appendix 1A):
*     pro_foundings           Annual pro-LGBT+ INGO foundings
*     anti_foundings          Annual anti-LGBT+ INGO foundings
*     cum_pro_foundings       Cumulative pro-LGBT+ INGO foundings
*     cum_anti_foundings      Cumulative anti-LGBT+ INGO foundings
*     pro_ingo                Avg member countries per pro-LGBT+ INGO
*     anti_ingo               Avg member countries per anti-LGBT+ INGO
*     Note: These variables extend back to 1899. Pre-1990 rows have
*     country = missing (INGO data only, not panel observations).
*
*   Sample: Countries with avg pop >= 500,000 (N = 143, 3,221 obs.)
*
* REQUIRED PACKAGES:
*   ssc install outreg2
*
********************************************************************************


clear all
set more off
version 16


* ==============================================================================
* PART 1: DATA CONSTRUCTION DOCUMENTATION
* ==============================================================================
* This section documents how velasco_2023_sf_main.dta was constructed.
* NOT executable without the original raw source datasets.
* To replicate the analyses, skip to PART 2 below.
* ==============================================================================

/*

The analysis dataset was constructed through seven sequential stages.
Stages 1-4 are shared with the AJS 2023 replication (Velasco 2023, AJS)
and produce the base policy dataset. Stages 5-7 add SF-specific variables.

--------------------------------------------------------------------------------
STAGE 1: LGBTI Policy Index Construction
  (Same as AJS replication — see that file for full details.)
  Creates policy_index from 18 LGBTI-related laws using SEM factor scores
  for composite policy domains (sodomy, civil unions, gender marker, propaganda).
  Output: policy_index and 21 individual policy indicators per country-year.

--------------------------------------------------------------------------------
STAGE 2: Master Merge
  (Same as AJS replication.)
  Merges policy index with World Bank controls, INGO network measures,
  V-Dem democracy indicators, religious demographics, LGBTQ MPs, etc.
  Output: dissertation_master_unclean_v7.dta

--------------------------------------------------------------------------------
STAGE 3: Variable Cleaning
  (Same as AJS replication.)
  Interpolation, standardization, log transforms, global context factor
  scores, one-year lags.
  Output: dissertation_master_clean_v7.dta

--------------------------------------------------------------------------------
STAGE 4: Post-AJS Variables
  (Same as AJS replication.)
  Regional pro-LGBTI norms, total INGOs, counter-mobilization, demographics.
  Output: dissertation_master_policy_v5.dta

--------------------------------------------------------------------------------
STAGE 5: SF-Specific Merges
  Starting from dissertation_master_policy_v5.dta:

  a) Excluded countries with avg pop < 500,000
  b) Merged corrected anti-LGBTI INGO counts (anti_orgs_corrected.dta)
     -> interpolated to create anti_2
  c) Merged corrected pro-LGBTI INGO counts (pro_ingo_corrected.dta)
     -> interpolated to create pro_2
  d) Merged ethnic fractionalization (ethnic.dta), carry-forwarded
  e) Merged V-Dem v11.1 for v2xcs_ccsi (Core Civil Society Index)
     Note: V-Dem v8 does not include this variable
  f) Merged secondary education enrollment (Secondary Education.dta)
  g) Merged WCF-only anti-LGBTI counts (Anti-LGBT Counts - WCF Only.dta)
     -> interpolated to create wcf and anti_limited
  h) Merged Global Anti-LGBTI Context indicators (year-level)
  i) Merged Regional Conference data (region-year level)
  j) Merged UN Vote data on LGBTI issues

--------------------------------------------------------------------------------
STAGE 6: SF-Specific Variable Construction

  a) Policy change indicators:
     - policy_change_up: binary, policy_index increased from prior year
     - policy_change_down: binary, policy_index decreased from prior year

  b) SEM latent factors for normative contexts:
     - AntiContext: Global anti-LGBTI context
       Manifest indicators: tot_anti (from Global Anti Context file),
       wcf_conferences, un_statements, anti_consultative,
       tot_anti_ties (year-level sum of e_tot_faic)
     - Anti_Region: Regional anti-LGBTI context
       Manifest indicators: ave_anti (regional mean of e_tot_faic),
       regional_anti, wcf_conferences
     - Pro_Region: Regional pro-LGBTI context
       Manifest indicators: ave_pro (regional mean of e_tot_lingo),
       ilga_conferences, regional_norms_cum

  c) Additional variables: ingo_ln = ln(total INGOs + 1), post2008 indicator

  d) Lagged variables: 1-year lags for all predictors, 5-year lags for
     dependent variables (pro_2, anti_2, wcf, anti_limited)

  e) Factor score rescaling: Added constants to shift minimum to 0
     (Pro_Region + 1.03, Anti_Region + 19.07, AntiContext + 40.95)

--------------------------------------------------------------------------------
STAGE 7: Trim to Analysis Variables

  keep country year countryid countrycode region ///
      pro_2 anti_2 wcf anti_limited ///
      e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
      policy_index policy_change_up policy_change_down policy_change_any ///
      lag_global_context global_context ///
      AntiContext lag_AntiContext Anti_Region lag_Anti_Region ///
      Pro_Region lag_Pro_Region ///
      lag_policy_index lag_cum_lgbti_ngo_ln cum_lgbti_ngo_ln ///
      lag_anti_demo anti_demo lag_relpc relpc ///
      lag_fertility_rate fertility_rate ///
      lag_population_ln population_ln ///
      lag_efindex efindex ///
      post2008 ///
      lag_e_fh_ipolity2 e_fh_ipolity2 ///
      lag_gdppercap_ln gdppercap_ln ///
      ingo_ln lag_v2xcs_ccsi v2xcs_ccsi ///
      lag_secondary_percent2 secondary_percent2 ///
      lag_pro_2 lag_anti_2 lag_wcf lag_anti_limited ///
      lag5_pro_2 lag5_anti_2 lag5_wcf lag5_anti_limited ///
      imm_percent unemployment_ilo trade net_migration ///
      pro_foundings anti_foundings cum_pro_foundings cum_anti_foundings ///
      pro_ingo anti_ingo

  compress
  save "velasco_2023_sf_main.dta", replace

  Result: 4,951 observations x 66 variables.
  Note: Observations include pre-1990 rows with INGO founding data only
  (country is missing for these rows). The panel analysis subset starts
  at year 1991.

  INGO founding data (pro_foundings, anti_foundings, cum_pro_foundings,
  cum_anti_foundings) come from the Yearbook of International Organizations
  and extend back to 1899. Average membership data (pro_ingo, anti_ingo)
  track mean member countries per pro/anti-LGBTI INGO over time.

*/


* ==============================================================================
* PART 2: ANALYSIS
* ==============================================================================
* Place all provided files in the same directory and set it below.
* ==============================================================================


********************************************************************************
* 8. SETUP
********************************************************************************

* USERS: Set this to the directory containing the replication files.
* cd "/path/to/replication/files"

use "velasco_2023_sf_main.dta", clear


********************************************************************************
* 9. FIGURE 2: Average Country-Level Ties Over Time
********************************************************************************

preserve
	collapse (mean) pro_2 anti_2, by(year)
	twoway (line pro_2 year, lcolor(blue)) ///
		   (line anti_2 year, lcolor(red)), ///
		   title("Average Country-Level Ties to Transnational Networks") ///
		   ytitle("Average Number of Ties") xtitle("Year") ///
		   legend(order(1 "Pro-LGBT+ Networks" 2 "Anti-LGBT+ Networks"))
	graph export "figure2.pdf", replace
restore


********************************************************************************
* 10. FIGURE 3: Growth in Pro- and Anti-LGBT+ INGOs, 1900-2018
*     INGO founding data is integrated into the main dataset with pre-1990
*     rows (country is missing for those rows).
********************************************************************************

preserve
	keep year pro_foundings anti_foundings cum_pro_foundings cum_anti_foundings
	duplicates drop year, force
	drop if year == .

	twoway (line cum_pro_foundings year, lcolor(blue)) ///
		   (line cum_anti_foundings year, lcolor(red)) if year > 1899, ///
		   title("Growth in Pro- and Anti-LGBT+ INGOs, 1900-2018") ///
		   ytitle("Cumulative INGOs") xtitle("Year") ///
		   legend(order(1 "Pro-LGBT+" 2 "Anti-LGBT+"))
	graph export "figure3.pdf", replace
restore


********************************************************************************
* 11. FIGURE 4: Three-Wave Cross-Lagged Panel (1994 -> 2006 -> 2018)
*     SEM with correlated contemporaneous errors.
*     Primary (anti_2), plus robustness (wcf, anti_limited).
********************************************************************************

preserve

	rename Anti_Region anti_region
	rename Pro_Region pro_region

	keep country year e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
		policy_index cum_lgbti_ngo_ln anti_demo ///
		fertility_rate imm_percent population_ln unemployment_ilo efindex ///
		trade e_fh_ipolity2 gdppercap_ln ingo_ln v2xcs_ccsi relpc ///
		secondary_percent2 anti_region pro_region anti_2 pro_2 wcf anti_limited

	reshape wide e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
		policy_index cum_lgbti_ngo_ln anti_demo ///
		fertility_rate imm_percent population_ln unemployment_ilo efindex ///
		trade e_fh_ipolity2 gdppercap_ln ingo_ln v2xcs_ccsi relpc ///
		anti_region pro_region secondary_percent2 pro_2 anti_2 wcf anti_limited, ///
		i(country) j(year)

	* Primary Model (anti_2)
	sem ///
		(anti_22006 <- anti_21994 pro_21994 ///
			cum_lgbti_ngo_ln1994 anti_demo1994 fertility_rate1994 ///
			population_ln1994 efindex1994 gdppercap_ln1994 ///
			ingo_ln1994 anti_region1994 pro_region1994) ///
		(pro_22006 <- anti_21994 pro_21994 ///
			cum_lgbti_ngo_ln1994 anti_demo1994 fertility_rate1994 ///
			population_ln1994 efindex1994 gdppercap_ln1994 ///
			ingo_ln1994 anti_region1994 pro_region1994) ///
		(anti_22018 <- anti_22006 pro_22006 ///
			cum_lgbti_ngo_ln2006 anti_demo2006 fertility_rate2006 ///
			population_ln2006 efindex2006 gdppercap_ln2006 ///
			ingo_ln2006 anti_region2006 pro_region2006) ///
		(pro_22018 <- anti_22006 pro_22006 ///
			cum_lgbti_ngo_ln2006 anti_demo2006 fertility_rate2006 ///
			population_ln2006 efindex2006 gdppercap_ln2006 ///
			ingo_ln2006 anti_region2006 pro_region2006), ///
		cov(e.anti_22006*e.pro_22006) ///
		cov(e.anti_22018*e.pro_22018)
	outreg2 using figure4_crosslagged, excel replace dec(3) ///
		alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
		title("Figure 4: Three-Wave Cross-Lagged Panel")

	* Robustness: WCF-Only
	sem ///
		(wcf2006 <- wcf1994 pro_21994 ///
			cum_lgbti_ngo_ln1994 anti_demo1994 fertility_rate1994 ///
			population_ln1994 efindex1994 gdppercap_ln1994 ///
			ingo_ln1994 anti_region1994 pro_region1994) ///
		(pro_22006 <- wcf1994 pro_21994 ///
			cum_lgbti_ngo_ln1994 anti_demo1994 fertility_rate1994 ///
			population_ln1994 efindex1994 gdppercap_ln1994 ///
			ingo_ln1994 anti_region1994 pro_region1994) ///
		(wcf2018 <- wcf2006 pro_22006 ///
			cum_lgbti_ngo_ln2006 anti_demo2006 fertility_rate2006 ///
			population_ln2006 efindex2006 gdppercap_ln2006 ///
			ingo_ln2006 anti_region2006 pro_region2006) ///
		(pro_22018 <- wcf2006 pro_22006 ///
			cum_lgbti_ngo_ln2006 anti_demo2006 fertility_rate2006 ///
			population_ln2006 efindex2006 gdppercap_ln2006 ///
			ingo_ln2006 anti_region2006 pro_region2006), ///
		cov(e.wcf2006*e.pro_22006) ///
		cov(e.wcf2018*e.pro_22018)
	outreg2 using figure4_crosslagged, excel append dec(3) ///
		alpha(.001, .01, .05, .1) symbol(***, **, *, +)

restore


********************************************************************************
* 12. TABLE 2: Dynamic Panel Models (1990-2018)
*     Models 1-2: Predicting pro-LGBT+ ties
*     Models 3-4: Predicting anti-LGBT+ ties
*     Country FE, cluster-robust SEs, 5-year lagged DV.
********************************************************************************

* Model 1: Pro-LGBT+ (Main)
xtreg pro_2 ///
	c.lag_global_context c.lag_AntiContext ///
	c.lag_anti_2 c.lag_Anti_Region lag_Pro_Region ///
	c.lag5_pro_2 ///
	lag_policy_index i.policy_change_up i.policy_change_down ///
	lag_cum_lgbti_ngo_ln lag_anti_demo lag_relpc ///
	lag_fertility_rate lag_population_ln lag_efindex i.post2008 ///
	lag_e_fh_ipolity2 lag_gdppercap_ln ingo_ln ///
	lag_v2xcs_ccsi lag_secondary_percent2, ///
	fe vce(cluster countryid)
outreg2 using table2, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Table 2: Dynamic Panel Models")

* Model 2: Pro-LGBT+ (Interaction: Opposing x Anti_Region)
xtreg pro_2 ///
	c.lag_global_context c.lag_AntiContext ///
	c.lag_anti_2##c.lag_Anti_Region lag_Pro_Region ///
	c.lag5_pro_2 ///
	lag_policy_index i.policy_change_up i.policy_change_down ///
	lag_cum_lgbti_ngo_ln lag_anti_demo lag_relpc ///
	lag_fertility_rate lag_population_ln lag_efindex i.post2008 ///
	lag_e_fh_ipolity2 lag_gdppercap_ln ingo_ln ///
	lag_v2xcs_ccsi lag_secondary_percent2, ///
	fe vce(cluster countryid)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 3: Anti-LGBT+ (Main)
xtreg anti_2 ///
	c.lag_pro_2 c.lag_Pro_Region ///
	c.lag_global_context lag_AntiContext lag_Anti_Region ///
	c.lag5_anti_2 ///
	lag_policy_index i.policy_change_up i.policy_change_down ///
	lag_cum_lgbti_ngo_ln lag_anti_demo lag_relpc ///
	lag_fertility_rate lag_population_ln lag_efindex i.post2008 ///
	lag_e_fh_ipolity2 lag_gdppercap_ln ingo_ln ///
	lag_v2xcs_ccsi lag_secondary_percent2, ///
	fe vce(cluster countryid)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 4: Anti-LGBT+ (Interaction: Opposing x Pro_Region)
xtreg anti_2 ///
	c.lag_pro_2##c.lag_Pro_Region ///
	c.lag_global_context lag_AntiContext lag_Anti_Region ///
	c.lag5_anti_2 ///
	lag_policy_index i.policy_change_up i.policy_change_down ///
	lag_cum_lgbti_ngo_ln lag_anti_demo lag_relpc ///
	lag_fertility_rate lag_population_ln lag_efindex i.post2008 ///
	lag_e_fh_ipolity2 lag_gdppercap_ln ingo_ln ///
	lag_v2xcs_ccsi lag_secondary_percent2, ///
	fe vce(cluster countryid)
outreg2 using table2, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)


********************************************************************************
* 13. FIGURE 5: Average Marginal Effects (Interaction Plots)
*     Panel A: AME of anti-LGBT+ on pro-LGBT+ by regional anti context
*     Panel B: AME of pro-LGBT+ on anti-LGBT+ by regional pro context
********************************************************************************

* Panel A
quietly xtreg pro_2 ///
	c.lag_global_context c.lag_AntiContext ///
	c.lag_anti_2##c.lag_Anti_Region lag_Pro_Region ///
	c.lag5_pro_2 ///
	lag_policy_index i.policy_change_up i.policy_change_down ///
	lag_cum_lgbti_ngo_ln lag_anti_demo lag_relpc ///
	lag_fertility_rate lag_population_ln lag_efindex i.post2008 ///
	lag_e_fh_ipolity2 lag_gdppercap_ln ingo_ln ///
	lag_v2xcs_ccsi lag_secondary_percent2, ///
	fe vce(cluster countryid)
margins, dydx(lag_anti_2) at(lag_Anti_Region = (0(10)140)) atmeans
marginsplot, ///
	title("AME of Anti-LGBT+ Networks on Pro-LGBT+ Ties") ///
	ytitle("Average Marginal Effect") xtitle("Regional Anti-LGBT+ Context")
graph export "figure5_panelA.pdf", replace

* Panel B
quietly xtreg anti_2 ///
	c.lag_pro_2##c.lag_Pro_Region ///
	c.lag_global_context c.lag5_anti_2 lag_AntiContext lag_Anti_Region ///
	lag_policy_index i.policy_change_up i.policy_change_down ///
	lag_cum_lgbti_ngo_ln lag_anti_demo lag_relpc ///
	lag_fertility_rate lag_population_ln lag_efindex i.post2008 ///
	lag_e_fh_ipolity2 lag_gdppercap_ln ingo_ln ///
	lag_v2xcs_ccsi lag_secondary_percent2, ///
	fe vce(cluster countryid)
margins, dydx(lag_pro_2) at(lag_Pro_Region = (0(1)7)) atmeans
marginsplot, ///
	title("AME of Pro-LGBT+ Networks on Anti-LGBT+ Ties") ///
	ytitle("Average Marginal Effect") xtitle("Regional Pro-LGBT+ Context")
graph export "figure5_panelB.pdf", replace


********************************************************************************
* 14. APPENDIX: Alternative Cross-Lagged Specifications
********************************************************************************

use "velasco_2023_sf_main.dta", clear

rename Anti_Region anti_region
rename Pro_Region pro_region

keep country year e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
	policy_index cum_lgbti_ngo_ln anti_demo ///
	fertility_rate imm_percent population_ln unemployment_ilo efindex ///
	trade e_fh_ipolity2 gdppercap_ln ingo_ln v2xcs_ccsi relpc ///
	secondary_percent2 anti_region pro_region anti_2 pro_2 wcf anti_limited

reshape wide e_tot_lingo e_tot_faic z_e_tot_lingo z_e_tot_faic ///
	policy_index cum_lgbti_ngo_ln anti_demo ///
	fertility_rate imm_percent population_ln unemployment_ilo efindex ///
	trade e_fh_ipolity2 gdppercap_ln ingo_ln v2xcs_ccsi relpc ///
	anti_region pro_region secondary_percent2 pro_2 anti_2 wcf anti_limited, ///
	i(country) j(year)

* --- Four-Wave Cross-Lagged (1991 -> 1999 -> 2007 -> 2015) ---
sem ///
	(e_tot_faic1999 <- e_tot_faic1991 e_tot_lingo1991 ///
		cum_lgbti_ngo_ln1991 anti_demo1991 fertility_rate1991 ///
		population_ln1991 efindex1991 gdppercap_ln1991 ingo_ln1991) ///
	(e_tot_lingo1999 <- e_tot_faic1991 e_tot_lingo1991 ///
		cum_lgbti_ngo_ln1991 anti_demo1991 fertility_rate1991 ///
		population_ln1991 efindex1991 gdppercap_ln1991 ingo_ln1991) ///
	(e_tot_faic2007 <- e_tot_faic1999 e_tot_lingo1999 ///
		cum_lgbti_ngo_ln1999 anti_demo1999 fertility_rate1999 ///
		population_ln1999 efindex1999 gdppercap_ln1999 ingo_ln1999) ///
	(e_tot_lingo2007 <- e_tot_faic1999 e_tot_lingo1999 ///
		cum_lgbti_ngo_ln1999 anti_demo1999 fertility_rate1999 ///
		population_ln1999 efindex1999 gdppercap_ln1999 ingo_ln1999) ///
	(e_tot_faic2015 <- e_tot_faic2007 e_tot_lingo2007 ///
		cum_lgbti_ngo_ln2007 anti_demo2007 fertility_rate2007 ///
		population_ln2007 efindex2007 gdppercap_ln2007 ingo_ln2007) ///
	(e_tot_lingo2015 <- e_tot_faic2007 e_tot_lingo2007 ///
		cum_lgbti_ngo_ln2007 anti_demo2007 fertility_rate2007 ///
		population_ln2007 efindex2007 gdppercap_ln2007 ingo_ln2007), ///
	cov(e.e_tot_faic1999*e.e_tot_lingo1999) ///
	cov(e.e_tot_faic2007*e.e_tot_lingo2007) ///
	cov(e.e_tot_faic2015*e.e_tot_lingo2015)
outreg2 using appendix_crosslagged, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Appendix: Four-Wave Cross-Lagged Panels")

* --- Four-Wave Standardized with Full Controls ---
sem ///
	(z_e_tot_faic1999 <- z_e_tot_faic1991 z_e_tot_lingo1991 ///
		policy_index1991 cum_lgbti_ngo_ln1991 anti_demo1991 ///
		fertility_rate1991 imm_percent1991 population_ln1991 ///
		unemployment_ilo1991 trade1991 e_fh_ipolity21991 ///
		gdppercap_ln1991 ingo_ln1991) ///
	(z_e_tot_lingo1999 <- z_e_tot_faic1991 z_e_tot_lingo1991 ///
		policy_index1991 cum_lgbti_ngo_ln1991 anti_demo1991 ///
		fertility_rate1991 imm_percent1991 population_ln1991 ///
		unemployment_ilo1991 trade1991 e_fh_ipolity21991 ///
		gdppercap_ln1991 ingo_ln1991) ///
	(z_e_tot_faic2007 <- z_e_tot_faic1999 z_e_tot_lingo1999 ///
		policy_index1999 cum_lgbti_ngo_ln1999 anti_demo1999 ///
		fertility_rate1999 imm_percent1999 population_ln1999 ///
		unemployment_ilo1999 trade1999 e_fh_ipolity21999 ///
		gdppercap_ln1999 ingo_ln1999) ///
	(z_e_tot_lingo2007 <- z_e_tot_faic1999 z_e_tot_lingo1999 ///
		policy_index1999 cum_lgbti_ngo_ln1999 anti_demo1999 ///
		fertility_rate1999 imm_percent1999 population_ln1999 ///
		unemployment_ilo1999 trade1999 e_fh_ipolity21999 ///
		gdppercap_ln1999 ingo_ln1999) ///
	(z_e_tot_faic2015 <- z_e_tot_faic2007 z_e_tot_lingo2007 ///
		policy_index2007 cum_lgbti_ngo_ln2007 anti_demo2007 ///
		fertility_rate2007 imm_percent2007 population_ln2007 ///
		unemployment_ilo2007 trade2007 e_fh_ipolity22007 ///
		gdppercap_ln2007 ingo_ln2007) ///
	(z_e_tot_lingo2015 <- z_e_tot_faic2007 z_e_tot_lingo2007 ///
		policy_index2007 cum_lgbti_ngo_ln2007 anti_demo2007 ///
		fertility_rate2007 imm_percent2007 population_ln2007 ///
		unemployment_ilo2007 trade2007 e_fh_ipolity22007 ///
		gdppercap_ln2007 ingo_ln2007), ///
	cov(e.z_e_tot_faic1999*e.z_e_tot_lingo1999) ///
	cov(e.z_e_tot_faic2007*e.z_e_tot_lingo2007) ///
	cov(e.z_e_tot_faic2015*e.z_e_tot_lingo2015)
outreg2 using appendix_crosslagged, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Appendix Figure 1A: Average Membership per INGO ---
use "velasco_2023_sf_main.dta", clear
keep year pro_ingo anti_ingo
duplicates drop year, force
drop if year == . | pro_ingo == .

twoway (line pro_ingo year, lcolor(blue)) ///
	   (line anti_ingo year, lcolor(red)), ///
	   title("Average Membership per INGO Over Time") ///
	   ytitle("Average Member Countries") xtitle("Year") ///
	   legend(order(1 "Pro-LGBT+ INGOs" 2 "Anti-LGBT+ INGOs"))
graph export "appendix_figure1A.pdf", replace


********************************************************************************
* END OF REPLICATION FILE
********************************************************************************

display _newline(2)
display "============================================================"
display "  Replication complete."
display "============================================================"
