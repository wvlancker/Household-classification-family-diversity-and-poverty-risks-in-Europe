********************************************************************************
* SCRIPT DESCRIPTION
* Author: Wim Van Lancker, wim.vanlancker@kuleuven.be
* Script to produce the output for the paper 'Household classification, family 
* diversity and poverty risks in Europe: addressing a North-Western bias ' based
* on the Families in Households Typology (FHT)  
********************************************************************************

/* 
_README

Input data: EU-SILC 2022 verion 2023 release 2

The input data needs to include the variable 'fht' (Families in Households Typology). The replication code to operationalize this variable can be found here: 

Alzbeta Bartova, & Thaning, M. (2025). Families in Households Typology (FHT) v.1.0 (rEUsilience). Zenodo. https://doi.org/10.5281/zenodo.15854220

Figures are created manually based on the generated Excelsheets.

*/ 

clear
version 17
set more off
program drop _all

//SETUP  
	
	//set input path
	global PATH = "C:\Temp\FHT 2022"

	//set using data
	use "$PATH/SILC2022_FHT.dta", clear

	//set location to store results (note: not dropbox to avoid issues when there's a bad connection)
	global OUTPATH = "C:\Temp\FHT 2022"

	//set weight
	local weight = "db090"
	
	//set input and output variables
	local outputvars = "hh_eurostat hb110 fht5 fht"
	local inputvars = "pov matdep"
	local year = "2022"

//PREPARE DATASET

	//go to path
	cd "$OUTPATH"

	//all variables to lowercase

	foreach var of varlist _all {
		local newname = lower("`var'")
		cap rename `var' `newname'
	}

	// create standard variables sex and age 
	
	cap rename rb090 sex
	cap ge int age = rx020
	cap replace age = rb010-rb080 - 1 if rx020==.
	cap recode age (-1 = 0)
	
	// Italy not present in hb110 variable
	drop if country=="IT" | country == "SK" | country=="NL"
	
//CREATE VARIABLES 

	// adjust the pre-defined household typology 

	recode hx060 (5=1) (6/7=2) (9=3) (10/12=4) (else=5), gen(hh_eurostat)

	lab var hh_eurostat "Eurostat household typology (hx060)"
	lab def eurostatlb 1 "Single person" 2 "Couple without children" 3 "Single parent" 4 "Couple with children" 5 "Other" 
	lab val hh_eurostat eurostatlb
	
	// create fht5 
	/* 
	Creating an aggregated, simplified version of the FHT. 
	
	1. Single person = 1 in FHT, single person living alone in a household
	2. Couples = 2 and 11 in FHT, including couples living in a household and couples living in a household with (one of their) adult parents, so always couples without children
	3. Single parent = 3,4,7,8 in FHT, including single parents with dependent children, with adult children, and in multigenerational families
	4. Couples with children = 5,6,9,10 in FHT, including couples with dep children, adult children, and in multigenerational families
	5. Other = 12 in FHT, living arrangements in households that cannot be classified elsewhere. 
	*/
	
	recode fht (1=1)(2=2)(11=2)(3/4=3)(7/8=3)(5/6=4)(9/10=4)(12=5), gen(fht5)
		
	lab var fht5 "Families in Household Typology (5 categories)"
	lab def fht5lbl 1 "Single person" 2 "Couple without children" 3 "Single parent" 4 "Couple with children" 5 "Other" 
	lab val fht5 fht5lbl
	
	// poverty and deprivation (SMSD), using the pre-defined variables in EU-SILC

	cap gen pov = hx080
	cap gen matdep = rx060

	// define the program to create column names in the output Excel sheet
	
	program def addcolnames
		args x 
		qui su `x'
		local j = r(max)
		forvalues k = 1(1)`j' {
			local xlabel: label (`x') `k'
			loc cnt = `k' + 1
			loc a : word `cnt' of `c(ALPHA)'
			putexcel `a'2 = "`xlabel'"
		}
	end 

// SAVE WORKING FILE 

save tmpFHT.dta, replace	
	
// START GENERATING OUTPUT

// Create appendices with observations by country

putdocx clear

putdocx begin
table cid hx060
putdocx collect

table cid hb110
putdocx collect

table cid fht
putdocx collect

table cid hh_eurostat
putdocx collect

table cid fht5
putdocx collect

putdocx save "TableA1-A5.docx", replace
putdocx clear

// Additional analyses in text: inspect how households change classification

tab hh_eurostat fht5 [aw=`weight'], row
tab hh_eurostat fht5 if country=="SE" [aw=`weight'], row


// The code loops over output and inputvariables and generates tables of weighted prevalences and risks for each country as well as for the EU as a whole
// Writes it into separate Excelsheets
 	
putexcel clear

foreach x of varlist `outputvars' {

	local varlabel : var l `x'
	di "Prevalence: `varlabel'"
	

	putexcel set myresults, sheet("`x' `year'", replace) modify
	
	putexcel A1 = "Country"
	putexcel A2 = "Prevalence"
	putexcel B1 = "`varlabel'"
	
	// add column names by calling the program
	addcolnames `x'
	
	
	// define row to start adding results 
	local i = 3
	
	levelsof country
	foreach c in `r(levels)' {
		
		qui tab `x' if country=="`c'" [aw=`weight'], matcell(freq) matrow(names)
		mat a =  matrix(freq/r(N))'
		putexcel A`i' = "`c'" 
		putexcel B`i' = mat(a)
		local ++i
		
	}
	qui tab `x' [aw=`weight'], matcell(freq) matrow(names)
	mat a =  matrix(freq/r(N))'
	putexcel A`i' = "EU (weighted)"
	putexcel B`i' = mat(a)
}
	
foreach y of varlist `inputvars' {
	foreach x of varlist `outputvars' {

	local varlabel : var l `x'
	di "Risk `y' for `varlabel'"
	
	putexcel set myresults, sheet("`y' `x' `year'", replace) modify

	putexcel A1 = "Country"
	putexcel A2 = "`y'"
	putexcel B1 = "`varlabel'"
	
	// add column names by calling the program
	addcolnames `x'
	
	// define row to start adding results 
	local i = 3

	levelsof country
	foreach c in `r(levels)' {
		qui mean `y' if country=="`c'" [aw=`weight'], over(`x')
		mat a = e(b) 
		putexcel A`i' = "`c'" 
		putexcel B`i' = mat(a)
		local ++i
	}
	qui mean `y' [aw=`weight'], over(`x')
	mat a =  e(b)
	putexcel A`i' = "EU (weighted)"
	putexcel B`i' = mat(a)
	}
}

clear all