/*==============================================================================

					// Build Number of Layers within Firms
					
	// Author(s): - Martim Leitão (martim.leitao@kellogg.northwestern.edu);
				  - Joana Silva (joana.silva@ucp.pt);
	// Date: 1/1/2023
	// Data: QP data, 2004-2019
	// Goal: Construct within firm Layers. 
	// Dependencies: - (i) egenmore; 
					 - (ii) gtools; 
	// Software: Stata 17.0, MP
	// Note: In the Oppromola paper, qualif==53 | qualif==55 are in occup=0.
			 We include them in occup=1 due to aggregation limitations, consistent with INE. 

==============================================================================*/


	// 1. Preliminaries, Globals, Setting and Directory

	{
		cls
		clear all
		set more off
		set varabbrev off
		cd "C:\Users\Martim Leitão\OneDrive\Northwestern University\exchange_rate_shocks"
		global qp "C:\Users\Martim Leitão\OneDrive\Northwestern University\exchange_rate_shocks\QP_data\"
	}
	
	// 2. Bring in the data and quick selection
	
	forvalues v=2004(1)2018 {
	
	{
		qui: import spss using "${qp}QP_Trab_`v'.sav", clear
		
		// Adjust variable names before 2010
		
		forvalues i=4(1)9 {
		cap: ren (ano_0`i' ntrab_0`i' nuemp_0`i' nqua1_0`i') (ANO ntrab NUEMP nqual1)
		}
		
		// Sorting and Selection
		
		sort NUEMP
		keep ntrab NUEMP ANO nqual1
		
		// Labeling the qualification variable - 1 digit
		
		# delimit ;
		label define qualif_label
		1 "Quadros Superiores" 
		2 "Quadros Médios" 
		3 "Encarregados, Contramestres, Mestres e Chefes de Equipa" 
		4 "Profissionais Altamente Qualificados"
		5 "Profissionais Qualificados"
		6 "Profissionais Semi-Qualificados"
		7 "Profissionais não Qualificados"
		8 "Estagiários, Praticantes e Aprendizes"
		9 "Ignorado";
		#delimit cr
		label values nqual1 qualif_label
	}
	
	// 3. Create Number of Workers "brute force"
	
	bys NUEMP: egen nb_workers = nvals(ntrab)
	sum nb_workers, de	
	
/*==============================================================================

					// 4. Define Position and Layers
	
==============================================================================*/
	
	// A. Occupations
	
	gen occup = 3 if nqual1 == 1
	replace occup = 2 if nqual1 == 2 | nqual1 == 3
	replace occup = 1 if nqual1 == 4 | nqual1 == 5
	replace occup = 0 if nqual1 == 6 | nqual1 == 7 | nqual1 == 8 
	drop if occup == . 
	
	// B. Layers
	
	* Layer O
	bys NUEMP ANO: egen min_occup = min(occup)
	gen layer = 0 if occup==min_occup & occup!=.
	drop min_occup
	
	*Layer 1
	bys NUEMP ANO: egen min_occup = min(occup) if layer==.
	replace layer = 1 if occup==min_occup & occup!=.
	drop min_occup
	
	*Layer 2
	bys NUEMP ANO: egen min_occup = min(occup) if layer==.
	replace layer = 2 if occup==min_occup & occup!=.
	drop min_occup
	
	*Layer 3
	bys NUEMP ANO: egen min_occup = min(occup) if layer==.
	replace layer = 3 if occup==min_occup & occup!=.
	drop if layer==.
	drop min_occup
	
	* Generate layers and collapse data
	bys NUEMP ANO: egen num_mlayers = max(layer)
	label var num_mlayers "Number of management layers t"
	
	gcollapse (max) num_mlayers, by(NUEMP ANO)
	sum num_mlayers, de

	sort NUEMP ANO
	
	save "${qp}layers_`v'", replace
	
	}
	
	// Append layer datasets 
	
	use "${qp}layers_2018", clear
	forvalues v=2004(1)2017 {
	append using "${qp}layers_`v'"
	erase "${qp}layers_`v'.dta"
	}
	
	sort NUEMP ANO
	save "${qp}layers_portugal_04_18", replace
	
/*==============================================================================

								// End of do-file

==============================================================================*/
	