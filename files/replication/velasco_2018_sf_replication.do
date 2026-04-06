********************************************************************************
* Replication Do-File for:
*
* Velasco, Kristopher. 2018. "Human Rights INGOs, LGBT INGOs, and LGBT
*   Policy Diffusion, 1991-2015." Social Forces 97(1): 377-404.
*   https://doi.org/10.1093/sf/soy030
*
* Author: Kristopher Velasco
* Date Created: April 2026
*
* OVERVIEW:
*   PART 1: DATA CONSTRUCTION DOCUMENTATION (Sections 1-4)
*     Documents the full pipeline used to construct the analysis dataset.
*     NOT executable without original raw source datasets (described below).
*     The analysis dataset (velasco_2018_sf_main.dta) is provided pre-built.
*
*   PART 2: ANALYSIS (Sections 5-9)
*     Replicates all tables, figures, and robustness models.
*     >>> TO REPLICATE, START HERE (search "PART 2") <<<
*
* HOW TO USE:
*   This repository contains two files:
*     velasco_2018_sf_main.dta           Analysis dataset
*     velasco_2018_sf_replication.do     This file
*
*   Part 1 documents how the analysis dataset was constructed from raw
*   source datasets. Part 2 (search "PART 2") is executable and replicates
*   all published tables, figures, and robustness models.
*
*   For access to any individual raw source dataset described in Part 1,
*   please contact the author: Kristopher Velasco (kvelasco@princeton.edu).
*
* RAW SOURCE DATASETS (used to construct the analysis dataset):
*   Available from the author upon request. Copies are stored in the
*   raw_sources/ subfolder (not uploaded to the repository) with the
*   following numbering corresponding to the data construction stages:
*
*   raw_sources/01_yio_ingo_data.xlsx         LINGO + HRINGO country ties
*                                             (Yearbook of Intl Organizations)
*   raw_sources/02_ilga_policy_data.xlsx      14 LGBT policy indicators
*                                             (ILGA State-Sponsored Homophobia)
*   raw_sources/03_freedom_house.xlsx         Freedom House ratings 1972-2016
*   raw_sources/04_gdp_per_capita.xlsx        GDP per capita (World Bank)
*   raw_sources/05_female_parliament.xlsx     % women in parliament (V-Dem)
*   raw_sources/06_democracy_timeseries.xlsx  Polity/democracy measures
*   raw_sources/07_kof_globalization_index.xlsx  KOF Globalization Index
*   raw_sources/08_population_density.xlsx    Population density (World Bank)
*   raw_sources/09_trade_percent_gdp.xlsx     Trade as % GDP (World Bank)
*   raw_sources/10_country_population.xlsx    Country population (World Bank)
*   raw_sources/11_global_lingo_counts.xlsx   Annual global LINGO count
*   raw_sources/12_un_statements_lgbti.xlsx   UN SOGI statements (ICJ)
*   raw_sources/13_yio_anti_lingos.xlsx       Anti-homosexuality INGOs (YIO)
*
* ANALYSIS DATASET VARIABLES:
*   Identifiers:
*     Country         Country name (string)
*     Year            Observation year (1978-2015, analysis restricted 1991+)
*     CountryID       Numeric country identifier (for xtset)
*     uniqueid        Country-year concatenation (merge key)
*
*   Dependent Variables:
*     simple_index    LGBT Policy Index (equal weights). Sum of 14 policies
*                     coded +1 (progressive) or -1 (regressive). Range: -6 to 8.
*                     Policies: same-sex acts legal/illegal, death penalty,
*                     equal/unequal age of consent, propaganda laws, employment
*                     discrimination ban, constitutional protections, hate
*                     crime protections, incitement to hatred ban, full
*                     marriage, civil unions, partial union rights, joint
*                     adoption.
*     weighted_index  LGBT Policy Index (differential weights). Same 14
*                     policies with weights reflecting severity: death
*                     penalty (-3), constitutional protections (+3), full
*                     marriage (+3), etc. Range: -11 to 18.
*
*   Main Independent Variables:
*     zpressure       Global LGBT Context. Composite z-score of cumulative
*                     UN SOGI statements + cumulative global LINGO count.
*                     Cronbach's alpha = .93, r = .88 between components.
*     TotalHRINGOs    Country ties to human rights INGOs (YIO in-degree)
*     TotalLINGOs     Country ties to LGBT INGOs (YIO in-degree)
*     anti_LINGOs     Country ties to anti-homosexuality INGOs (YIO)
*
*   Control Variables:
*     logGDP              GDP per capita (logged, constant 2015 USD)
*     Female              % women in national parliament (V-Dem)
*     logDensity          Population density (logged, World Bank)
*     socialglobalization  KOF Social Globalization Index (0-100)
*     Trade               Trade as % GDP (World Bank)
*     inverse_mean        Inverted Freedom House score (7=most democratic)
*
*   Individual Policy Indicators (for Figure 1):
*     SameSexActsLegal, SameSexActsIllegal, DeathPenalty,
*     EqualAgeofConsent, UnequalAgeofConsent, PropogandaLaws,
*     ProhibitionofEmploymentDiscim, ConstitutionalProhibitionofDi,
*     HateCrimeProtection, IncitementtoHatredProhibited,
*     FullMarriageRights, CivilUnions, PartialUnionRights, JointAdoption
*
*   Global Context Components (for Figure 2):
*     UNdocs          Cumulative UN SOGI statements (ICJ data)
*     GlobalLINGOs    Cumulative global count of LGBT INGOs (YIO)
*
*   Sample: 156 countries with population >= 500,000, 1991-2015.
*           1,323 country-year observations in published models.
*
* REQUIRED PACKAGES:
*   ssc install outreg2
*   ssc install zscore
*
********************************************************************************


clear all
set more off
version 13


* ==============================================================================
* PART 1: DATA CONSTRUCTION DOCUMENTATION
* ==============================================================================
* This section documents how velasco_2018_sf_main.dta was constructed.
* NOT executable without the original raw source datasets.
* To replicate the analyses, skip to PART 2 below.
* ==============================================================================

/*

The analysis dataset was constructed through four sequential stages.

--------------------------------------------------------------------------------
STAGE 1: Master Merge (0a.Merging datasets.Sept17.do)

  Built a country-year panel by sequentially merging raw Excel files:

  a) Imported LINGO ties from YIO Data.xlsx (sheet: "LINGOs") and HRINGO
     ties from YIO Data.xlsx (sheet: "HRINGOs"). Each records the number
     of member-country ties to pro-LGBT and human rights INGOs per year.
     Saved as intermediate .dta files, then merged 1:1 on uniqueid.

  b) Merged 14 LGBT policy indicators from ILGA Policy Data.xlsx.
     Binary indicators for each policy (0/1): same-sex acts legal/illegal,
     death penalty, equal/unequal age of consent, propaganda laws,
     employment discrimination ban, constitutional protections, hate crime
     protections, incitement to hatred, full marriage, civil unions,
     partial union rights, joint adoption.

  c) Sequential 1:1 merges for country-year controls:
     - Freedom House 1972-2016.xlsx -> Freedom House ratings
     - GDP.xlsx -> GDP per capita
     - Percent Female Parliament.xlsx -> % women in parliament
     - Democracy Timeseries.xlsx -> Polity scores
     - KOF Index.xlsx -> Social/economic/political globalization indices
     - Population Density.xlsx -> Population density
     - %Trade.xlsx -> Trade as % GDP
     - Country Population.xlsx -> Country population

  d) Year-level (m:1) merges for global variables:
     - Global LINGO Total.xlsx -> Cumulative global LINGO count
     - UN Statements on LGBT Issues.xlsx -> Cumulative UN SOGI statements

  Output: Thesis_Merged_Uncleaned.dta

--------------------------------------------------------------------------------
STAGE 2: Variable Cleaning (0b.Cleaning datasets.Sept17.do)

  a) Destrung PopDensity, Trade, GDPperCap from string to numeric.

  b) Imputed missing values:
     - KOF globalization indices: forward-fill, then backward-fill
     - Female parliament: forward-fill, then backward-fill
     - Trade: forward-fill, then backward-fill, then linear interpolation
     - GDP per capita: linear interpolation, then forward/backward fill

  c) Excluded countries with population < 500,000.

  d) Constructed dependent variables:
     - simple_index = sum of 14 binary policies * equal weights (+1/-1)
     - weighted_index = sum of 14 binary policies * differential weights

  e) Constructed independent variables:
     - logGDP = ln(GDPperCap + 1)
     - logDensity = ln(PopDensity + 1)
     - zpressure = zscore(UNdocs) + zscore(GlobalLINGOs)
       Cronbach's alpha = .9345, Pearson r = .8771

  f) Encoded CountryID for panel structure.

  g) Merged anti-homosexuality INGOs from YIO Anti-LINGOs.xlsx.
     Missing values replaced with 0.

  Output: Thesis_cleaned_sept17.dta

--------------------------------------------------------------------------------
STAGE 3: Trim to Analysis Variables

  keep Country Year CountryID uniqueid ///
      simple_index weighted_index ///
      zpressure TotalHRINGOs TotalLINGOs anti_LINGOs ///
      logGDP Female logDensity socialglobalization Trade inverse_mean ///
      SameSexActsLegal SameSexActsIllegal DeathPenalty ///
      EqualAgeofConsent UnequalAgeofConsent PropogandaLaws ///
      ProhibitionofEmploymentDiscim ConstitutionalProhibitionofDi ///
      HateCrimeProtection IncitementtoHatredProhibited ///
      FullMarriageRights CivilUnions PartialUnionRights JointAdoption ///
      UNdocs GlobalLINGOs

  compress
  save "velasco_2018_sf_main.dta", replace

  Result: 1,972 observations x 32 variables.
  (Published models restrict to Year > 1990, yielding 1,323 observations.)

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

use "velasco_2018_sf_main.dta", clear
xtset CountryID Year, yearly


********************************************************************************
* 6. FIGURE 1: Growth of Selected LGBT Policies, 1991-2015
*    Shows cross-national averages of policy adoption over time.
********************************************************************************

preserve
	collapse (mean) SameSexActsLegal FullMarriageRights CivilUnions ///
		ProhibitionofEmploymentDiscim HateCrimeProtection ///
		SameSexActsIllegal DeathPenalty PropogandaLaws, by(Year)

	twoway (connected SameSexActsLegal Year, sort(Year)) ///
		   (connected FullMarriageRights Year, sort(Year)) ///
		   (connected CivilUnions Year, sort(Year)) ///
		   (connected ProhibitionofEmploymentDiscim Year, sort(Year)) ///
		   (connected HateCrimeProtection Year, sort(Year)) ///
		   if Year > 1990, ///
		   title("Growth of Selected LGBT Policies, 1991-2015") ///
		   ytitle("Proportion of Countries") xtitle("Year") ///
		   legend(order(1 "Same-Sex Acts Legal" 2 "Full Marriage" ///
			3 "Civil Unions" 4 "Employment Discrim. Ban" 5 "Hate Crime Prot.") ///
			size(small))
	graph export "figure1.pdf", replace
restore


********************************************************************************
* 7. FIGURE 2: Growth of LGBT Policy Index and Global LGBT Context
********************************************************************************

preserve
	collapse (mean) simple_index zpressure, by(Year)

	twoway (connected simple_index Year, sort(Year) yaxis(1)) ///
		   (connected zpressure Year, sort(Year) yaxis(2)) ///
		   if Year > 1990, ///
		   title("Growth of LGBT Policy Index and Global LGBT Context") ///
		   ytitle("Mean LGBT Policy Index", axis(1)) ///
		   ytitle("Global LGBT Context (z-score)", axis(2)) ///
		   xtitle("Year") ///
		   legend(order(1 "LGBT Policy Index" 2 "Global LGBT Context"))
	graph export "figure2.pdf", replace
restore


********************************************************************************
* 8. TABLE 3: Cross-Sectional Time-Series of LGBT Policy Index, 1991-2015
*    Fixed effects with country-clustered robust SEs.
*    6 models published in the paper.
********************************************************************************

* --- Model 1: Baseline Global Context ---
xtreg simple_index zpressure ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using table3, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Table 3: TSCS of LGBT Policy Index, 1991-2015")

* --- Model 2: + HRINGOs ---
xtreg simple_index zpressure TotalHRINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 3: + LINGOs ---
xtreg simple_index zpressure TotalLINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 4: + LINGOs + anti-LINGOs ---
xtreg simple_index zpressure TotalLINGOs anti_LINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 5: HRINGOs x Global Context Interaction ---
xtreg simple_index c.TotalHRINGOs##c.zpressure ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* --- Model 6: LINGOs x Global Context Interaction ---
xtreg simple_index c.TotalLINGOs##c.zpressure anti_LINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using table3, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)


********************************************************************************
* 8b. FIGURES 3 & 4: Marginal Interaction Effects
*     Figure 3: HRINGO ties conditional on Global LGBT Context
*     Figure 4: LINGO ties conditional on Global LGBT Context
********************************************************************************

* Figure 3: HRINGOs x Global Context
quietly xtreg simple_index c.TotalHRINGOs##c.zpressure ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
margins, dydx(TotalHRINGOs) at(zpressure = (-3(0.5)4)) atmeans
marginsplot, ///
	title("Marginal Effect of HRINGO Ties on Global LGBT Context") ///
	ytitle("Marginal Effect of HRINGOs") xtitle("Global LGBT Context")
graph export "figure3.pdf", replace

* Figure 4: LINGOs x Global Context
quietly xtreg simple_index c.TotalLINGOs##c.zpressure anti_LINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
margins, dydx(TotalLINGOs) at(zpressure = (-3(0.5)4)) atmeans
marginsplot, ///
	title("Marginal Effect of LINGO Ties on Global LGBT Context") ///
	ytitle("Marginal Effect of LINGOs") xtitle("Global LGBT Context")
graph export "figure4.pdf", replace


********************************************************************************
* 9. ROBUSTNESS: Weighted Index Models (reported in text/footnotes)
*    Same 6 model specifications using the weighted_index DV.
********************************************************************************

* Model 1: Baseline
xtreg weighted_index zpressure ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using robustness_weighted, excel replace dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +) ///
	title("Robustness: Weighted Index Models, 1991-2015")

* Model 2: + HRINGOs
xtreg weighted_index zpressure TotalHRINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using robustness_weighted, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 3: + LINGOs
xtreg weighted_index zpressure TotalLINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using robustness_weighted, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 4: + LINGOs + anti-LINGOs
xtreg weighted_index zpressure TotalLINGOs anti_LINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using robustness_weighted, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 5: HRINGOs x Global Context
xtreg weighted_index c.TotalHRINGOs##c.zpressure ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using robustness_weighted, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)

* Model 6: LINGOs x Global Context
xtreg weighted_index c.TotalLINGOs##c.zpressure anti_LINGOs ///
	logGDP Female logDensity socialglobalization Trade inverse_mean ///
	if Year > 1990, fe vce(cluster Country)
outreg2 using robustness_weighted, excel append dec(3) ///
	alpha(.001, .01, .05, .1) symbol(***, **, *, +)


********************************************************************************
* END OF REPLICATION FILE
********************************************************************************

display _newline(2)
display "============================================================"
display "  Replication complete."
display "============================================================"
