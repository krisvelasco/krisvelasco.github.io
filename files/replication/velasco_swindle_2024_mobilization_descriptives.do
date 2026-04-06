/*
Project: WFC Network Over Time
Last Edited By: Kristopher Velasco
Overview:
	1) Figure 1: total conferences and total participants, by congresses and conferences
	2) Figure 2: regional participation percentages and religious participation percentages
	3) Figure 3: graph showing participants by organizational type

Date  of last edit: December 17, 2023
*/

*1) Figure 1: total conferences and total participants
	*a) Total Conferences
		*)i First, importing the data that has *all* Congresses/Conferences
		import excel "/Users/kv7379/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230804.xlsx", sheet("List of Conferences") firstrow clear
		
		*ii) Creating a generic count variable to create conference sums over time
			gen count=1
			
		*iii) Creating a variable to differentiate between event types
			foreach var in Regional Congress{
				gen `var'=regexm(EventType, "`var'")
			}
			
			gen meeting_type="congress" if Congress==1
				replace meeting_type="conference" if Regional==1
		
		*iv) Creating first figure that is total conferences by types
			*i) Getting annual totals
			collapse (sum) count, by(Year meeting_type)
			
			*ii) getting cumulative totals
			bysort meeting_type (Year) : gen cum_count = sum(count)
			
			*iii) going to reshape wide and back so that I can carryforward totals so every year has a cumulative count
			reshape wide cum_count count, i(meeting_type) j(Year)
			reshape long cum_count count, i(meeting_type) j(Year)
			
			by meeting_type: carryforward cum_count, replace
			replace count=0 if count==.
			
		*v) Creating the figure
		twoway (connected cum_count Year if meeting_type=="congress", lcolor(black) lwidth(medium) msymbol(o) mcolor(black)) ///
				(connected cum_count Year if meeting_type=="conference", lcolor(gs10) lwidth(medium) msymbol(s) mcolor(gs10)) ///
			, ///
			title("Conferences", size(medsmall) pos(12)) ///
			ytitle("Cumulative Total", size(medsmall)) ///
			xtitle("") ///
			yscale(range(0 70) lwidth(medium) lc(black)) ///
			xscale(range(1997 2022) lwidth(medium) lc(black)) ///
			ylabel(0(10)70, labcolor(black) labsize(small)) ///
			xlabel(1995(10)2025, labsize(small) labcolor(black)) ///
			legend(position(6)) ///
			legend(cols(1)) ///
			legend(size(vsmall)) ///
			legend(order(1 "Global" 2 "Regional")) ///
			note("") name(trend1, replace) 
			graph display trend1, xsize(15) ysize(20) 
	
	*b) Total Speakers
	clear
		*i) 
		import excel "/Users/kv7379/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230804.xlsx", sheet("WCF Presenters") firstrow clear
		
		*ii) Creating a generic count variable to create conference sums over time
			gen count=1
			
		*iii) Creating a variable to differentiate between event types
			foreach var in Conference Congress{
				gen `var'=regexm(Conference_Type, "`var'")
			}
			
			gen meeting_type="congress" if Congress==1
				replace meeting_type="conference" if Conference==1
	
		*iv) Getting total participant counts by conference type 
			collapse (sum) count, by(Year meeting_type)
			drop if Year==.
		
		*v) getting cumulative totals
			bysort meeting_type (Year) : gen cum_count = sum(count)
			
		*vi) going to reshape wide and back so that I can carryforward totals so every year has a cumulative count
			reshape wide cum_count count, i(meeting_type) j(Year)
			reshape long cum_count count, i(meeting_type) j(Year)
			
			by meeting_type: carryforward count cum_count, replace

			
		*v) Creating the figure first
		twoway (connected cum_count Year if meeting_type=="congress", lcolor(black) lwidth(medium) msymbol(o) mcolor(black)) ///
				(connected cum_count Year if meeting_type=="conference", lcolor(gs10) lwidth(medium) msymbol(s) mcolor(gs10)) ///
			, ///
			title("Speakers", size(medsmall) pos(12)) ///
			ytitle("Cumulative Total", size(medsmall)) ///
			xtitle("") ///
			yscale(range(0 1500) lwidth(medium) lc(black)) ///
			xscale(range(1997 2022) lwidth(medium) lc(black)) ///
			ylabel(0(250)1500, labcolor(black) labsize(small)) ///
			xlabel(1995(10)2025, labsize(small) labcolor(black)) ///
			legend(position(6)) ///
			note("") name(trend2, replace) 
			graph display trend2, xsize(15) ysize(20) 
			
	grc1leg trend1 trend2, legendfrom(trend1)

*2) Figure 2: Graph showing participants by organizational type
		clear
		*i) 
		import excel "/Users/kv7379/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230804.xlsx", sheet("WCF Presenters") firstrow clear
		
		*ii) Creating a generic count variable to create conference sums over time
			gen count=1
			
		*iii) Recording Conference Region
				
	
		*iv) Getting total participant counts by region
			drop if Organization_Type==""
			collapse (sum) count, by(Year Organization_Type)
			drop if Year==.
			
		*v) Creating an annnual total count so I can get percentages
				bysort Year: egen total=total(count)
				
				gen percent_orgtype=(count/total)*100
		
		*vi) Going to reshape wide then long to have observation for each region-year
			drop if Organization_Type==""
			reshape wide count total percent_orgtype, i(Organization_Type) j(Year)
			reshape long count total percent_orgtype, i(Organization_Type) j(Year)
			replace percent_orgtype=0 if percent_orgtype==.
			replace count=0 if count==.
			replace total=0 if total==.
			by Organization_Type: generate ave_count = (count[_n+1] + count + count[_n-1]) / 3
			by Organization_Type: generate ave_total = (total[_n+1] + total + total[_n-1]) / 3
			gen ave_percent=(ave_count/ave_total)*100

		*v) Creating the figure first
			twoway (connected ave_count Year if Organization_Type=="Business", lcolor(black) lwidth(medium) msymbol(Oh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="NGO", lcolor(black) lwidth(medium) msymbol(Sh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="Education", lcolor(gs10) lwidth(medium) msymbol(square) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="Religious", lcolor(gs10) lwidth(medium) msymbol(o) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="State", lcolor(black) lwidth(medium) msymbol(diamond) mcolor(black)) ///
							if Year<2022, ///
				ytitle("Total Number of Participanting Organizations", size(medsmall)) ///
				xtitle("") ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				yscale(range(0 150) lwidth(medsmall) lc(black)) ///
				ylabel(0(25)150, labsize(small) labcolor(black)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Business" 2 "NGO" 3 "Education" ///
								4 "Religious" 5 "State")) ///
				note("") name(trend5, replace) 
				graph display trend5, xsize(15) ysize(8) 

				
*3) Appendix Figure A1: Graph showing participants by organizational type by Region
		clear
		*i) 
		import excel "/Users/kv7379/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230804.xlsx", sheet("WCF Presenters") firstrow clear
		
		*ii) Creating a generic count variable to create conference sums over time
			gen count=1
			
		*iii) Recording Conference Region
				
	
		*iv) Getting total participant counts by region
			drop if Organization_Type==""
			collapse (sum) count, by(Year Organization_Type Organization_Region)
			drop if Year==.
			
		*v) Creating an annnual total count so I can get percentages
				bysort Year Organization_Region: egen total=total(count)
				
				gen percent_orgtype=(count/total)*100
		
		*vi) Going to reshape wide then long to have observation for each region-year
			drop if Organization_Type==""
			reshape wide count total percent_orgtype, i(Organization_Type Organization_Region) j(Year)
			reshape long count total percent_orgtype, i(Organization_Type Organization_Region) j(Year)
			replace percent_orgtype=0 if percent_orgtype==.
			replace count=0 if count==.
			replace total=0 if total==.
			
			by Organization_Type Organization_Region: generate ave_count = (count[_n+1] + count + count[_n-1]) / 3
			
		*v) Recreating this by region -- using three-year rolling averages to smooth it out
			twoway (connected ave_count Year if Organization_Type=="Business", lcolor(black) lwidth(medium) msymbol(Oh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="NGO", lcolor(black) lwidth(medium) msymbol(Sh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="Education", lcolor(gs10) lwidth(medium) msymbol(square) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="Religious", lcolor(gs10) lwidth(medium) msymbol(o) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="State", lcolor(black) lwidth(medium) msymbol(diamond) mcolor(black)) ///
							if Year<2022 & Organization_Region=="Western Europe & North America", ///
				title("Western Europe & North America", size(medsmall) pos(12)) ///
				ytitle("Number of Total Participants", size(medsmall)) ///
				xtitle("") ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Business" 2 "NGO" 3 "Education" ///
								4 "Religious" 5 "State")) ///
				note("") name(trend5, replace) 
				graph display trend5, xsize(15) ysize(8) 
				
				graph export "/Users/kv7379/Documents/Research Projects/WCF Network/Figures/For Mobilization Publication/Figure A1. Western Europe & North America.png", as(png) name("trend5")

				
				
		twoway (connected ave_count Year if Organization_Type=="Business", lcolor(black) lwidth(medium) msymbol(Oh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="NGO", lcolor(black) lwidth(medium) msymbol(Sh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="Education", lcolor(gs10) lwidth(medium) msymbol(square) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="Religious", lcolor(gs10) lwidth(medium) msymbol(o) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="State", lcolor(black) lwidth(medium) msymbol(diamond) mcolor(black)) ///
							if Year<2022 & Organization_Region=="Africa", ///
				title("Africa", size(medsmall) pos(12)) ///
				ytitle("Number of Total Participants", size(medsmall)) ///
				xtitle("") ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Business" 2 "NGO" 3 "Education" ///
								4 "Religious" 5 "State")) ///
				note("") name(trend5, replace) 
				graph display trend5, xsize(15) ysize(8) 
				
				graph export "/Users/kv7379/Documents/Research Projects/WCF Network/Figures/For Mobilization Publication/Figure A1. Africa.png", as(png) name("trend5")
				
				
		twoway (connected ave_count Year if Organization_Type=="Business", lcolor(black) lwidth(medium) msymbol(Oh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="NGO", lcolor(black) lwidth(medium) msymbol(Sh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="Education", lcolor(gs10) lwidth(medium) msymbol(square) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="Religious", lcolor(gs10) lwidth(medium) msymbol(o) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="State", lcolor(black) lwidth(medium) msymbol(diamond) mcolor(black)) ///
							if Year<2022 & Organization_Region=="Asia and the Pacific", ///
				title("Asia and the Pacific", size(medsmall) pos(12)) ///
				ytitle("Number of Total Participants", size(medsmall)) ///
				xtitle("") ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Business" 2 "NGO" 3 "Education" ///
								4 "Religious" 5 "State")) ///
				note("") name(trend5, replace) 
				graph display trend5, xsize(15) ysize(8) 
				
				graph export "/Users/kv7379/Documents/Research Projects/WCF Network/Figures/For Mobilization Publication/Figure A1. Asia and the Pacific.png", as(png) name("trend5")
				
				
			twoway (connected ave_count Year if Organization_Type=="Business", lcolor(black) lwidth(medium) msymbol(Oh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="NGO", lcolor(black) lwidth(medium) msymbol(Sh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="Education", lcolor(gs10) lwidth(medium) msymbol(square) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="Religious", lcolor(gs10) lwidth(medium) msymbol(o) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="State", lcolor(black) lwidth(medium) msymbol(diamond) mcolor(black)) ///
							if Year<2022 & Organization_Region=="Eastern Europe & Central Asia", ///
				title("Post-Soviet", size(medsmall) pos(12)) ///
				ytitle("Number of Total Participants", size(medsmall)) ///
				xtitle("") ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Business" 2 "NGO" 3 "Education" ///
								4 "Religious" 5 "State")) ///
				note("") name(trend5, replace) 
				graph display trend5, xsize(15) ysize(8) 
				
				graph export "/Users/kv7379/Documents/Research Projects/WCF Network/Figures/For Mobilization Publication/Figure A1. Post-Soviet.png", as(png) name("trend5") replace
				
			twoway (connected ave_count Year if Organization_Type=="Business", lcolor(black) lwidth(medium) msymbol(Oh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="NGO", lcolor(black) lwidth(medium) msymbol(Sh) mcolor(black)) ///
				(connected ave_count Year if Organization_Type=="Education", lcolor(gs10) lwidth(medium) msymbol(square) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="Religious", lcolor(gs10) lwidth(medium) msymbol(o) mcolor(gs10)) ///
				(connected ave_count Year if Organization_Type=="State", lcolor(black) lwidth(medium) msymbol(diamond) mcolor(black)) ///
							if Year<2022 & Organization_Region=="Latin America and the Caribbean", ///
				title("Latin America and the Caribbean", size(medsmall) pos(12)) ///
				ytitle("Number of Total Participants", size(medsmall)) ///
				xtitle("") ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Business" 2 "NGO" 3 "Education" ///
								4 "Religious" 5 "State")) ///
				note("") name(trend5, replace) 
				graph display trend5, xsize(15) ysize(8) 
				
				graph export "/Users/kv7379/Documents/Research Projects/WCF Network/Figures/For Mobilization Publication/Figure A1. Latin America and the Caribbean.png", as(png) name("trend5") replace
					
********************************************************************************	
********************************************************************************					
********************************************************************************					
********************************************************************************	
/*Note that we never ended up using this figures in the Mobilization paper*/				
				
*3) Figure 3: Regional participation and reigious participation 
	*a) Regional participation 
		clear 
		import excel "/Users/kvelasco/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230624.xlsx", sheet("WCF Presenters") firstrow clear

		*i) Creating a generic count variable to create conference sums over time.
			gen count=1

		/*ii) Creating two new regions -- U.S. and Russia specifically
			replace Organization_Region="United States" if Organization_Country=="United States"
			replace Organization_Region="Russia" if Organization_Country=="Russia" */
		
		*ii) Getting total participant counts by region
			collapse (sum) count, by(Year Organization_Region)
			drop if Year==.
			
		*iii) Creating an annnual total count so I can get percentages
				bysort Year: egen total=total(count)
				
				gen percent_region=(count/total)*100
		
		*iv) Going to reshape wide then long to have observation for each region-year
			drop if Organization_Region==""
			reshape wide count total percent_region, i(Organization_Region) j(Year)
			reshape long count total percent_region, i(Organization_Region) j(Year)
			replace percent_region=0 if percent_region==.
			
		*v) Creating figure
			twoway (connected percent_region Year if Organization_Region=="Africa") ///
				(connected percent_region Year if Organization_Region=="Asia and the Pacific") ///
				(connected percent_region Year if Organization_Region=="Eastern Europe & Central Asia") ///
				(connected percent_region Year if Organization_Region=="Latin America and the Caribbean") ///
				(connected percent_region Year if Organization_Region=="Western Europe & North America") ///
							if Year<2020, ///
				title("Regional Representation", size(medsmall) pos(12)) ///
				ytitle("Percent of Total Participants", size(medsmall)) ///
				xtitle("") ///
				yscale(range(0 100) lwidth(medsmall) lc(black)) ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				ylabel(0(10)100, labcolor(black) labsize(small)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Africa" 2 "Asia/Pacific" 3 "Post-Soviet" ///
								4 "L. America" 5 "The West")) ///
				note("") name(trend3, replace) 
				graph display trend3, xsize(15) ysize(5) 

	*b) Religious Percentages
		*a) Regional participation 
		clear 
		import excel "/Users/kvelasco/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230624.xlsx", sheet("WCF Presenters") firstrow clear

		*i) Creating a generic count variable to create conference sums over time.
			gen count=1
			
		
		*ii) Creating a string of text that are people's titles, positions, and organization to get religion
			egen text=concat(Title Position Organization), punct(" ")
			replace text=lower(text)

		*iii) Creating religion variables
			gen mormon=regexm(text, "byu|elder |brigham|world family policy|latter-day")
			gen catholic=regexm(text, "catholic|diocese|father|vatican|pontif|priests for life|sacramento bishop|john paul")
			gen orthodox=regexm(text, "orthodox|ortodoxe|patriarch")
			gen jewish=regexm(text, "rabbi|jew|judaism|chabad")
			gen islam=regexm(text, "imam|islam|muslim")
			gen protestant=regexm(text,"anglican|lutheran|assemblies of god|christian|ministr|church|baptist|protestant|pastor|reverend|evangelical|episcopal|seventh-day")
		
		
			gen religion=""
			replace religion="mormon" if mormon==1
			replace religion="catholic" if catholic==1
			replace religion="orthodox" if orthodox==1
			replace religion="jewish" if jewish==1
			replace religion="islam" if islam==1
			replace religion="protestant" if protestant==1 & religion==""
			
			replace religion="mormon" if ///
				Organization=="American Heritage School" | ///
				Organization=="Angel Studios" | ///
				Organization=="Association of Mormon Counselors and Psychologists " | ///
				Organization=="BYU Management SocietyÊ" | ///
				Organization=="Brigham Young University" | ///
				Organization=="Church of Jesus Christ of Latter-Day Saints" | ///
				Organization=="Deseret Book" | ///
				Organization=="Deseret News" | ///
				Organization=="Family First Foundation" | ///
				Organization=="Family Watch International - FWI" | ///
				Organization=="Fight the New Drug" | ///
				Organization=="Joseph F. Smith Family Association" | ///
				Organization=="NGO Family Voice" | ///
				Organization=="NuSkin Enterprises" | ///
				Organization=="Power of Mothers" | /// 
				Organization=="Sutherland Institute" | ///
				Organization=="The Church of Jesus Christ of Latter-Day Saints" | ///
				Organization=="United Families International" | ///
				Organization=="Utah Eagle Forum" | ///
				Organization=="Utah House of Representatives" | ///
				Organization=="World Family Policy Center"
			
			
			replace religion="catholic" if ///
				Organization=="Open Hearts Charitable Foundation" | ///
				Organization=="American Society for the Defense of Tradition, Family and Property" | ///
				Organization=="Archbishop of Esztergom-Budapest and Primate of Hungary" | ///
				Organization=="Archbishop of Manila" | ///
				Organization=="Archbishop of San Francisco" | ///
				Organization=="Archbishop of Utrecht" | ///
				Organization=="Asociatia Provita Bucuresti" | ///
				Organization=="Association Ai.Bi Amici dei Bambini" | ///
				Organization=="Associazione per La Difesa dei Valori Cristiani – Luci sull'Est" | ///
				Organization=="Ayuda a la Iglesia Necesitada" | ///
				Organization=="C-FAM" 
						
			replace religion="protestant" if ///
				Organization=="777 blog" | ///
				Organization=="8Global Life Campagne" | ///
				Organization=="Advocates International" | ///
				Organization=="All of Life" | ///
				Organization=="Alliance Defending Freedom" | ///
				Organization=="Alliance Defense Fund" | ///
				Organization=="American Family" | ///
				Organization=="Apologia Mission" | ///
				Organization=="Arrow Leadership" | ///
				Organization=="Australian Familiy Association" | ///
				Organization=="Awaken" | ///
				Organization=="Elpis Center" | Organization=="Elpis Centre" | ///
				Organization=="Global Institute for Family Intergrity" | ///
				Organization=="Home School Legal Defense Association"
			
		*iv) note that at this point, I exported the list of organizations out to an excel and manually checked religious affiliations. 
				*I'm now merging that list in.
				merge m:1 Organization Organization_Country using  "/Users/kvelasco/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/Organizations to Categorize.dta"
				drop if _merge==2
				drop _merge
				
			replace religion=Organization_Religion if religion=="" & Organization_Region!=""

		*iv) Getting total participant counts by region
			drop if religion==""
			collapse (sum) count, by(Year religion)
			drop if Year==.
			
		*v) Creating an annnual total count so I can get percentages
				bysort Year: egen total=total(count)
				
				gen percent_religion=(count/total)*100
		
		*vi) Going to reshape wide then long to have observation for each region-year
			drop if religion==""
			reshape wide count total percent_religion, i(religion) j(Year)
			reshape long count total percent_religion, i(religion) j(Year)
			replace percent_religion=0 if percent_religion==.

		*v)ii Creating figure
			twoway (connected percent_religion Year if religion=="mormon") ///
				(connected percent_religion Year if religion=="catholic") ///
				(connected percent_religion Year if religion=="orthodox") ///
				(connected percent_religion Year if religion=="jewish") ///
				(connected percent_religion Year if religion=="islam") ///
				(connected percent_religion Year if religion=="protestant") ///
							if Year<2020, ///
				title("Religious Representation", size(medsmall) pos(12)) ///
				ytitle("Percent of Total Participants", size(medsmall)) ///
				xtitle("") ///
				yscale(range(0 100) lwidth(medsmall) lc(black)) ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				ylabel(0(10)100, labcolor(black) labsize(small)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Mormon" 2 "Catholic" 3 "Orthodox" ///
								4 "Jewish" 5 "Islamic" 6 "Protestant")) ///
				note("") name(trend4, replace) 
				graph display trend4, xsize(15) ysize(5) 
			
				graph combine trend3 trend4, col(1)

			
			
			

	*c) Religious vs. Political at Congresses
		*a) Regional participation 
		clear 
		import excel "/Users/kvelasco/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230624.xlsx", sheet("WCF Presenters") firstrow clear

		*i) Creating a generic count variable to create conference sums over time.
			gen count=1
			
		
		*ii) Creating a string of text that are people's titles, positions, and organization to get religion
			egen text=concat(Title Position Organization), punct(" ")
			replace text=lower(text)

		*iii) Creating religion variables
			gen mormon=regexm(text, "byu|elder |brigham|world family policy|latter-day")
			gen catholic=regexm(text, "catholic|diocese|father|vatican|pontif|priests for life|sacramento bishop|john paul")
			gen orthodox=regexm(text, "orthodox|ortodoxe|patriarch")
			gen jewish=regexm(text, "rabbi|jew|judaism|chabad")
			gen islam=regexm(text, "imam|islam|muslim")
			gen protestant=regexm(text,"anglican|lutheran|assemblies of god|christian|ministr|church|baptist|protestant|pastor|reverend|evangelical|episcopal|seventh-day")
		
		
			gen religion=""
			replace religion="mormon" if mormon==1
			replace religion="catholic" if catholic==1
			replace religion="orthodox" if orthodox==1
			replace religion="jewish" if jewish==1
			replace religion="islam" if islam==1
			replace religion="protestant" if protestant==1 & religion==""
			
			replace religion="mormon" if ///
				Organization=="American Heritage School" | ///
				Organization=="Angel Studios" | ///
				Organization=="Association of Mormon Counselors and Psychologists " | ///
				Organization=="BYU Management SocietyÊ" | ///
				Organization=="Brigham Young University" | ///
				Organization=="Church of Jesus Christ of Latter-Day Saints" | ///
				Organization=="Deseret Book" | ///
				Organization=="Deseret News" | ///
				Organization=="Family First Foundation" | ///
				Organization=="Family Watch International - FWI" | ///
				Organization=="Fight the New Drug" | ///
				Organization=="Joseph F. Smith Family Association" | ///
				Organization=="NGO Family Voice" | ///
				Organization=="NuSkin Enterprises" | ///
				Organization=="Power of Mothers" | /// 
				Organization=="Sutherland Institute" | ///
				Organization=="The Church of Jesus Christ of Latter-Day Saints" | ///
				Organization=="United Families International" | ///
				Organization=="Utah Eagle Forum" | ///
				Organization=="Utah House of Representatives" | ///
				Organization=="World Family Policy Center"
			
			
			replace religion="catholic" if ///
				Organization=="Open Hearts Charitable Foundation" | ///
				Organization=="American Society for the Defense of Tradition, Family and Property" | ///
				Organization=="Archbishop of Esztergom-Budapest and Primate of Hungary" | ///
				Organization=="Archbishop of Manila" | ///
				Organization=="Archbishop of San Francisco" | ///
				Organization=="Archbishop of Utrecht" | ///
				Organization=="Asociatia Provita Bucuresti" | ///
				Organization=="Association Ai.Bi Amici dei Bambini" | ///
				Organization=="Associazione per La Difesa dei Valori Cristiani – Luci sull'Est" | ///
				Organization=="Ayuda a la Iglesia Necesitada" | ///
				Organization=="C-FAM" 
						
			replace religion="protestant" if ///
				Organization=="777 blog" | ///
				Organization=="8Global Life Campagne" | ///
				Organization=="Advocates International" | ///
				Organization=="All of Life" | ///
				Organization=="Alliance Defending Freedom" | ///
				Organization=="Alliance Defense Fund" | ///
				Organization=="American Family" | ///
				Organization=="Apologia Mission" | ///
				Organization=="Arrow Leadership" | ///
				Organization=="Australian Familiy Association" | ///
				Organization=="Awaken" | ///
				Organization=="Elpis Center" | Organization=="Elpis Centre" | ///
				Organization=="Global Institute for Family Intergrity" | ///
				Organization=="Home School Legal Defense Association"
			
		*iv) note that at this point, I exported the list of organizations out to an excel and manually checked religious affiliations. 
				*I'm now merging that list in.
				merge m:1 Organization Organization_Country using  "/Users/kvelasco/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/Organizations to Categorize.dta"
				drop if _merge==2
				drop _merge
				
			replace religion=Organization_Religion if religion=="" & Organization_Region!=""

		*iv) Getting total participant counts by actor type
			replace religion="unaffiliated" if religion==""
			gen religious_actor=1 if religion!="unaffiliated" 
			replace religious_actor=0 if religion=="unaffiliated"
			
			gen political_actor=1 if Organization_Type=="State" 
			replace political_actor=0 if political_actor==.
			
			keep if Conference_Type=="Congress"
			drop if Year==.
			
			collapse (mean) political_actor religious_actor, by(Year)
			replace political_actor=political_actor*100
			replace religious_actor=religious_actor*100


		*v)ii Creating figure
			twoway (connected political_actor Year) ///
					(connected religious_actor Year) ///
							if Year<2020, ///
				ytitle("Percent of Total Participants", size(medsmall)) ///
				xtitle("") ///
				yscale(range(0 70) lwidth(medsmall) lc(black)) ///
				xscale(range(1995 2020) lwidth(medsmall) lc(black)) ///
				ylabel(0(10)70, labcolor(black) labsize(small)) ///
				xlabel(1995(5)2020, labsize(small) labcolor(black)) ///
				legend(position(3)) ///
				legend(cols(1)) ///
				legend(size(vsmall)) ///
				legend(order(1 "Political Actor" 2 "Religious Actor")) ///
				note("") name(trend4, replace) 
				graph display trend4, xsize(15) ysize(8) 
			