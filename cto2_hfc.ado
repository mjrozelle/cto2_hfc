*! cto2_hfc.ado - Stata module to write template hfc for SurveyCTO data
*! Author: Michael Rozelle <michael.rozelle@wur.nl>
*! Version 0.0.1  Modified:  March 2023

// to do: change to CHAR LIST

// Drop the cto_read program if it already exists
cap program drop cto2_hfc
// Define the cto_read program, which takes three arguments
program define cto2_hfc
// instrument, then dataset, then dofile
syntax, ///
	SAVEfolder(string) /// filepath to the folder where datasets have been saved
	TEXDIRectory(string) /// filepath to .tex file 
	SURVEYNAME(string) /// Name of survey (for document)
	STATUS(name) /// Survey status variable
	SUCCESS(numlist) /// Value(s) of status variable indicating successful survey
	ENUM(name) /// Enumerator name variable ///
	VCETYPE(string) ///
	[INCLUDE(namelist) /// Specify only the variables you want to include
	IGNORE(namelist) /// Specify the variables you want to exclude 
	TREATMENT(name) /// Variable, levels of which correspond to different treatments
	]
	
pause on
set graphics off
version 16
local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65
local brek = char(10)
local tab = char(9)
local skip = "///" + char(10) + char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}


// Create a quiet environment to prevent unnecessary output
qui { 
	
preserve 
local original_frame `c(frame)'

cap mkdir "`texdirectory'"

cap mkdir "`texdirectory'/tables"
cap mkdir "`texdirectory'/graphs"

*------------------------------------------------------------------
*	Load Metadata and Survey Datasets
*------------------------------------------------------------------

tempname meta meta_groups meta_repeats
frame create `meta'

frame create `meta_groups'
frame `meta_groups' {
	
	use "`savefolder'/group_metadata.dta", clear
	keep if type == 1
	
} 

frame create `meta_repeats'
frame `meta_repeats' {
	
	use "`savefolder'/group_metadata.dta", clear
	keep if type == 2 & repetitions > 0
	
	local num_repeats = `c(N)'
	local num_levels = `num_repeats' + 1
	
	forvalues i = 0/`num_repeats' {
		
		local linkframes
		local all_keys key
		if `i' == 0 local frame survey 
		else {
			
			local frame = name[`i']
			local layers = layers_nested[`i']
			
			forvalues j = `layers'(-1)0 {
				
				if `j' == 0 local linkframes survey `linkframes'
				else { 
					
					local nextframe = nest_level_`j'[`i']
					levelsof name if index == `nextframe', local(nf_name) clean
					local linkframes `nf_name' `linkframes'
					
				}
				
			}
			
		}
		
		tempname `frame'
		frame create ``frame''
		frame ``frame'' { 
			
			use "`savefolder'/`frame'.dta", clear
			
			foreach f in `linkframes' {
				
				if "`f'" != "survey" local all_keys `all_keys' `f'_key
				frlink m:1 `all_keys', frame(``f'')
				
			}
		
		}
		
	}
	
	valuesof index 
	local replevels `r(values)'
	
} 

cwf `meta'
use "`savefolder'/survey_metadata.dta", clear
frlink m:1 repeat_group, frame(`meta_repeats' index)
frlink m:1 group, frame(`meta_groups' index)

if "`include'" != "" {
	
	local include_var_regex = ustrregexra("`include'", " ", ", ", .)
	gen include = ustrregexm(name, "^(`include_var_regex')$")
	bysort var_uid: egen include2 = max(include)
	keep if include2 == 1
	drop include include2
	
}
if "`ignore'" != "" {
	
	local ignore_var_regex = ustrregexra("`ignore'", " ", "|", .)
	gen ignore = ustrregexm(name, "^(`ignore_var_regex')$")
	bysort var_uid: egen ignore2 = max(ignore)
	drop if ignore2 == 1
	drop ignore ignore2
	
}

sort order within_order
gen row_n = _n 

*===============================================================================
* 	Initalise Dofile
*===============================================================================

cap file close myfile
file open myfile using "`texdirectory'/template_hfc.do", write replace 
	
file write myfile ///
	"/*" ///
	_n "Title: Template HFC Dofile for `surveyname'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Note: " ///
	_n "*/" _n(2) ///
	"`hbanner'" ///
	_n "* 	Setup" _n /// 
	"`hbanner'" ///
	_n(2) "clear all" _n "version `version'" _n "set more off" _n "set maxvar 120000" ///
	_n "cap log close" _n "set trace off" _n "set graphics off" _n ///
	"set linesize 200" _n(2) ///
	"`hbanner'" ///
	_n "* 	Macros" _n /// 
	"`hbanner'" ///
	_n(2) ///
	"local" _tab `"today = date(c(current_date), "DMY")"' _n ///
	"local" _tab `"todaystr=string(\`today', "%td")"' _n(2) ///
	"`hbanner'" ///
	_n "* 	Programs for LateX" _n /// 
	"`hbanner'" _n(2) ///	
	`"file open mytex using "`macval(texdirectory)'/manuscript.tex", append write"' _n(2) ///
	`"cap prog drop latex_table"' _n ///
	`"prog define latex_table "' _n ///
	`"syntax , TABLE(string) [SIDEWAYS]"' _n ///
	`"qui {"' _n(2) ///
	`"if "\`sideways'" != "" local type sidewaystable"' _n ///
	`"else local type table"' _n(2) ///
	`"file write mytex _n(2) ///"' _n ///
	`"	`"\clearpage"' _n ///"' _n ///
	`"	`"\centering"' _n ///"' _n ///
	`"	`"\input{\`table'}"' _n ///"' _n ///
	`"	"' _n ///
	`"}"' _n ///
	`"end"' _n(2) ///
	`"cap prog drop latex_figure"' _n ///
	`"prog define latex_figure "' _n ///
	`"syntax , FIGURE(string) CAPTION(string)"' _n ///
	`"qui {"' _n(2) ///
	`"file write mytex _n(2) ///"' _n ///
	`"	`"\clearpage"' _n ///"' _n ///
	`"   `"\begin{figure}[htbp]"' _n ///"' _n ///
	`"	`"\caption{\`caption'}"' _n ///"' _n ///
	`"	`"\includegraphics[width=\linewidth,keepaspectratio]{\`figure'}"' _n ///"' _n ///
	`"	`"\end{figure}"' _n(2)"' _n(2) ///
	`"}"' _n ///
	`"end"' _n(2) ///
	"`hbanner'" ///
	_n "* 	Import" _n /// 
	"`hbanner'" _n(2) ///
	`"frame create meta"' _n(2) ///
	`"frame create meta_groups"' _n ///
	`"frame meta_groups {"' _n(2) ///
	_tab `"use "`macval(savefolder)'/group_metadata.dta", clear"' _n ///
	_tab `"keep if type == 1"' _n(2) "}" _n(2) ///
	`"frame create meta_repeats"' _n ///
	`"frame meta_repeats {"' _n(2) ///
	_tab `"use "`macval(savefolder)'/group_metadata.dta", clear"' _n ///
	_tab `"keep if type == 2 & repetitions > 0"' _n(2) ///
	_tab `"local num_repeats = \`c(N)'"' _n ///
	_tab `"local num_levels = \`num_repeats' + 1"' _n(2) ///
	_tab `"forvalues i = 0/\`num_repeats' {"' _n(2) ///
	_tab _tab `"local linkframes"' _n ///
	_tab _tab `"local all_keys key"' _n(2) ///
	_tab _tab `"if \`i' == 0 local frame survey"' _n ///
	_tab _tab `"else {"' _n(2) ///
	_tab _tab _tab `"local frame = name[\`i']"' _n ///
	_tab _tab _tab `"local layers = layers_nested[\`i']"' _n(2) ///
	_tab _tab _tab `"forvalues j = \`layers'(-1)0 {"' _n(2) ///
	_tab _tab _tab _tab `"if \`j' == 0 local linkframes survey \`linkframes'"' _n ///
	_tab _tab _tab _tab `"else {"' _n(2) ///
	_tab _tab _tab _tab _tab `"local nextframe = nest_level_\`j'[\`i']"' _n ///
	_tab _tab _tab _tab _tab `"levelsof name if index == \`nextframe', local(nf_name) clean"' _n ///
	_tab _tab _tab _tab _tab `"local linkframes \`nf_name' \`linkframes'"' _n(2) ///
	_tab _tab _tab _tab `"}"' _n(2) ///
	_tab _tab _tab `"}"' _n(2) ///
	_tab _tab `"}"' _n(2) ///
	_tab _tab `"frame create \`frame'"' _n ///
	_tab _tab `"frame \`frame' {"' _n(2) ///
	_tab _tab _tab `"use "`macval(savefolder)'/\`frame'.dta", clear"' _n ///
	_tab _tab _tab `"foreach f in \`linkframes' {"' _n(2) ///
	_tab _tab _tab _tab `"if "\`f'" != "survey" local all_keys \`all_keys' \`f'_key"' _n ///
	_tab _tab _tab _tab `"frlink m:1 \`all_keys', frame(\`f')"' _n(2) ///
	_tab _tab _tab `"}"' _n(2) ///
	_tab _tab `"}"' _n(2) ///
	_tab `"}"' _n(2) ///
	`"}"' _n(2) ///
	`"cwf meta"' _n `"use "`macval(savefolder)'/survey_metadata.dta", clear"' _n(2) ///
	`"frlink m:1 repeat_group, frame(meta_repeats index)"' _n ///
	`"frlink m:1 group, frame(meta_groups index)"' _n(2) ///
	`"sort order within_order"' _n `"gen row_n = _n"'
	
*===============================================================================
* 	Initialise LateX file
*===============================================================================	
	
cap file close mytex
file open mytex using "`texdirectory'/manuscript.tex", write replace 
write_preamble, title("`surveyname'") 
file close mytex 
	
*===============================================================================
* 	Generate Basic Overview Command
*===============================================================================

clonevar tex_varlabel = varlabel
replace tex_varlabel = ustrto(tex_varlabel, "ascii", 1)
foreach i in 92 35 36 37 38 95 94 123 125 126 {

	replace tex_varlabel = subinstr(tex_varlabel, `"`=char(`i')'"', `"\\`=char(`i')'"', .)
	
}

gen basic_overview = `"`lbanner'"' ///
                + "`brek'" + "*" + "`tab'" + varlabel + "`brek'" + ///
				"`lbanner'" + "`brek2'"

forvalues i = 1/`c(N)' {
	
	local type = question_type[`i']
	local repeat = repeat_group[`i']
	local name = name[`i']
	local label = varlabel[`i']
	
	if `repeat' == 0 {
		
		local r_groupname survey
		local r_grouplabel Survey-Level
		
	}
	else {
		
		frame `meta_repeats' { 
			
			levelsof name if index == `repeat', clean local(r_groupname)
			levelsof label if index == `repeat', clean local(r_grouplabel)
			
		}
		
	}
	
	if `type' == 2 {
		
		overview_select_one, rownum(`i') varname(`name') ///
			label("`label'") frame(``r_groupname'') ///
			rgroup(`r_groupname') ///
			texdirectory("`macval(texdirectory)'")
			
		
	}
	else if `type' == 3 & within_order[`i'] == 1 {
		
		overview_select_multiple, frame(``r_groupname'') rownum(`i') ///
			varname(`name') label("`label'") ///
			rgroup(`r_groupname') ///
			texdirectory("`macval(texdirectory)'")
			
	}
	else {
		
		replace basic_overview = "" in `i'
		
	}

}

foreach r in 0 `replevels' {
	
	if `r' == 0 {
		
		local r_groupname survey
		local r_grouplabel Survey-Level
		
	}
	else {
		
 		frame `meta_repeats' { 
			
			levelsof name if index == `r', clean local(r_groupname)
			levelsof label if index == `r', clean local(r_grouplabel)
			
		}
		
	}
	
	count if repeat_group == `r'
	if `r(N)' == 0 continue
	
	file write myfile _n(2) "`hbanner'" _n "*" ///
		_tab "`r_grouplabel'" _n "`hbanner'" ///
		_n(2) "cwf `r_groupname'" _n(2) ///
		`"file write mytex _n(2) `"\newpage"' _n ///"' _n ///
		`"`"\section{`r_grouplabel'}"'"'
		
	sumstats_check, frame(`r_groupname') replevel(`r') ///
		texdirectory("`macval(texdirectory)'") treatment(`treatment') ///
		meta(`meta') metagroups(`meta_groups') metarepeats(`meta_repeats') ///
		vcetype(`vcetype')
	
	levelsof row_n if repeat_group == `r' & !missing(basic_overview), clean local(rows)
	foreach c in `rows' {
		
		file write myfile _n(2) (basic_overview[`c'])
		
	}
		
	cap mkdir "`texdirectory'/tables/`r_groupname'"
	cap mkdir "`texdirectory'/graphs/`r_groupname'"
	
	
}

file write myfile _n(2) ///
	`"file write mytex _n(2) `"\end{document}"'"' _n ///
	`"file close mytex"'
file close myfile

}

end

// *------------------------------------------------------------------
// *	Survey Progress
// *------------------------------------------------------------------
//
// levelsof name if repeat_group == 0 & type == "today", local(survey_date) clean
// frame `survey' {
//	
// 	local stat_label : variable label `status'
//	
// 	estpost tab `status'
// 	esttab using "`texdirectory'/tables/survey_progress/status_table.tex", replace ///
// 		cell("b pct(fmt(a))") collabels("Freq." "Percent") noobs ///
// 		nonumb nomtitle nostar unstack nonumber ///
// 		compress nomtitle nonote label booktabs
//	
// 	// preserve 
// 	tempname dum
// 	gen `dum' = 1
// 	collapse (count) `dum', by(`status' `survey_date')
//	
// 	tempvar cumulative
// 	bysort `status' (`survey_date'): gen `cumulative' = sum(`dum')
//	
// 	local graph_cmd
// 	local lgnd
// 	local i = 0
// 	local vallabel : value label `status'
//	
// 	levelsof `status', clean local(status_levels)
// 	local num_levels = `r(r)'
// 	foreach s in `status_levels' {
//		
// 		local ++ i
//		
// 		local graph_cmd `graph_cmd' ///
// 			(line `cumulative' `survey_date' if `status' == `s')
//			
// 		local lgnd `lgnd' `i' "`: label `vallabel' `s''"
//		
// 	}
//	
// 	local lgnd_rows = ceil(`num_levels' / 3)
//	
// 	twoway `graph_cmd', ///
// 		xlabel(, gmin gmax labsize(7pt) format(%tdDayname,_dd_Month)) ///
// 		xtitle("Survey Date", size(8pt)) ///
// 		ytitle("Number of Surveys") ///
// 		legend(pos(6) order(`lgnd') row(`lgnd_rows'))
// 	graph export "`texdirectory'/graphs/survey_progress/cumulative_surveys.pdf", replace as(pdf)	
//	
// }
//
// file write mytex _n(2) `"\newpage"' _n ///
// 	`"\section{Survey Progress}"' _n(2) ///
// 	`"\begin{figure}[htbp]"' _n ///
// 	`"\caption{`stat_label'}"' _n ///
// 	`"\includegraphics[width=\linewidth,keepaspectratio]{graphs/survey_progress/cumulative_surveys.pdf}"' _n ///
// 	`"\end{figure}"' _n(2) ///
// 	`"\begin{table}[H]"' _n ///
// 	`"\centering"' _n ///
// 	`"\caption{`stat_label'}"' _n ///
// 	`"\input{tables/survey_progress/status_table.tex}"' _n ///
// 	`"\end{table}"'

cap prog drop write_preamble
prog define write_preamble 
syntax, TITLE(string)
qui {
	
local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65
local brek = char(10)
local tab = char(9)
local skip = "///" + char(10) + char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}
	
local date : display %tdDayname,_dd_Month_CCYY date("`c(current_date)'", "DMY")
local date `date'

file write mytex `"%%%%%%%%%%%%%%%%%%%%%%%%%%"' _n ///
	`"%%%"' _n ///
	`"%%%      Preamble"' _n ///
	`"%%%"' _n ///
	`"%%%%%%%%%%%%%%%%%%%%%%%%%%"' _n(2) ///
	`"\documentclass[11pt]{article}"' _n(2) ///
	`"%%% packages"' _n ///
	`"\usepackage[colorlinks=true, allcolors=blue, obeyspaces]{hyperref}"' _n ///
	`"\usepackage{amssymb, amsfonts, amsmath}"' _n ///
	`"\usepackage{bm}"' _n ///
	`"\usepackage[margin=1in]{geometry} % full-width"' _n ///
    `"\topskip        =   20pt"' _n ///
    `"\parskip        =   10pt"' _n ///
    `"\parindent      =   0 pt"' _n ///
    `"\baselineskip   =   15pt"' _n(2) ///
	`"\usepackage{setspace}"' _n ///
    `"\onehalfspacing"' _n ///
	`"\usepackage{adjustbox}"' _n ///
	`"\usepackage{rotating}"' _n ///
    `"\numberwithin{table}{section}"' _n ///
	`"\usepackage{longtable}"' _n ///
	`"\usepackage{booktabs}"' _n ///
	`"\usepackage{dcolumn}"' _n ///
    `"\newcolumntype{d}[1]{D{.}{.}{#1}}"' _n ///
	`"\usepackage{natbib}"' _n ///
    `"\bibliographystyle{plainnat}"' _n ///
	`"\usepackage{pdflscape}"' _n ///
	`"\usepackage{appendix}"' _n ///
	`"\usepackage{graphicx}"' _n ///
	`"\usepackage{float}"' _n ///
	`"\usepackage{tablefootnote}"' _n ///
	`"\usepackage{threeparttable}"' _n ///
	`"\usepackage{comment}"' _n ///
	`"\usepackage{csquotes}"' _n ///
	`"\usepackage{tikz}"' _n ///
	`"\usetikzlibrary{calc}"' _n ///
	`"\usepackage{caption}"' _n ///
	`"\captionsetup{width=.9\linewidth, font=small}"' _n ///
	`"\usepackage{etoolbox}"' _n(2) ///
	`"\newcommand{\sym}[1]{\rlap{#1}} % for the stars"' _n(2) ///
	`"%%%%%%%%%%%%%%%%%%%%%%%%%%"' _n ///
	`"%%%"' _n ///
	`"%%%     Front page"' _n ///
	`"%%%"' _n ///
	`"%%%%%%%%%%%%%%%%%%%%%%%%%%"' _n(2) ///
	`"\title{`title' High Frequency Report}"' _n ///
	`"\author{`c(username)'}"' _n ///
	`"\date{`date'}"' _n(2) ///
	`"\begin{document}"' _n ///
	`"\maketitle"' _n(2) ///
	`"\begin{abstract}"' _n ///
	`"\centering"' _n ///
	`"This is a high frequency check report produced for the `title' Survey."' _n ///
	`"\end{abstract}"' _n(2) ///
	`"\newpage"' _n(2) ///
	`"%%%%%%%%%%%%%%%%%%%%%%%%%%"' _n ///
	`"%%%"' _n ///
	`"%%%   Introduction"' _n ///
	`"%%%"' _n ///
	`"%%%%%%%%%%%%%%%%%%%%%%%%%%"' _n(2) ///
	`"\tableofcontents"' _n(2) ///
	`"\clearpage"' _n(2) ///
	`"\listoftables"' _n ///
	`"\listoffigures"'
	
}

end

cap prog drop overview_select_one 
program define overview_select_one
syntax , ROWNUM(integer) VARNAME(string) TEXDIRECTORY(string) LABEL(string) ///
	FRAME(string) RGROUP(string) 
qui {


local tex_varlabel = tex_varlabel[`rownum']
local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65
local brek = char(10)
local tab = char(9)
local skip = "///" + char(10) + char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}

local title = substr("`label'", 1, 80)

frame `frame': cap count if !missing(`varname')
if _rc | "`r(N)'" == "0" {
	
	replace basic_overview = "" in `rownum'
	
}
else {

replace basic_overview = basic_overview + ///
	"graph hbar (count), over(`varname', sort(1)) `skip'" + ///
	`"title("{bf}`title'", pos(11) size(2.75)) `skip'"' + ///
	`"subtitle("`r(N)' observations", pos(11) size(2.5)) `skip'"' + ///
	`"name(`varname', replace) `skip'"' + ///
	`"note(`""\``varname'[qtext]'""', size(2)) `skip'"' + ///
	`"ytitle("Frequency", size(2.5)) `skip'"' + ///
	`"ylabel(, gmin gmax)"' + ///
	`"`brek'graph export `skip'"`macval(texdirectory)'/graphs/`rgroup'/`varname'.pdf""' + ///
	`", `skip' name(`varname') as(pdf) replace`brek'"' + ///
	`"latex_figure, figure("graphs/`rgroup'/`varname'.pdf") caption("`title'")"' ///
	in `rownum'

}
}
end

cap prog drop overview_select_multiple 
program define overview_select_multiple
syntax , ROWNUM(integer) VARNAME(string) LABEL(string) FRAME(string) ///
	TEXDIRECTORY(string) RGROUP(string)
qui {

local tex_varlabel = tex_varlabel[`rownum']
local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65
local brek = char(10)
local tab = char(9)
local skip = "///" + char(10) + char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}

local uid = var_uid[`rownum']

local j = 0
valuesof row_n if var_uid == `uid'
local rows `r(values)'
local levels : word count `rows'
local ysize = ceil(`levels'/4)
local title = substr("`label'", 1, 80)

foreach m in `rows' {
	
	if within_order[`m'] == 1 continue
	local ++j
	
	if mod(`j', 3) == 0 local doskip `skip'
	else local doskip
	
	local n_varname = name[`m']
	local graph_vars `graph_vars' `n_varname' `doskip'
	local n_label = varlabel[`m']
	local n_label: subinstr local n_label "#{`varname'}: " ""
	local lgnd `lgnd' `j' "`n_label'" `skip'
	
}

frame `frame': cap count if !missing(`varname')
if _rc | "`r(N)'" == "0" {
	
	replace basic_overview = "" in `rownum'
	
}
else {
	
	replace basic_overview = basic_overview + ///
		`"graph hbar (sum) `skip'"' + ///
		`"`graph_vars', ascategory `skip'"' + ///
		`"yvaroptions(relabel(`lgnd') `skip'"' + ///
		`"label(labsize(6pt))) `skip'"' + ///
		`"title("{bf}`title'", pos(11) size(2.75)) `skip'"' + ///
		`"subtitle("`r(N)' observations", pos(11) size(2.5)) `skip'"' + ///
		`"name(`varname', replace) `skip'ysize(`ysize') `skip'"' + ///
		`"note(`""\``varname'[qtext]'""', size(2)) `skip'"' + ///
		`"ytitle("Frequency", size(2.5)) `skip'"' + ///
		`"ylabel(, gmin gmax)"' + ///
		`"`brek'graph export `skip'"`macval(texdirectory)'/graphs/`rgroup'/`varname'.pdf""' + ///
		`", `skip' name(`varname') as(pdf) replace`brek'"' + ///
		`"latex_figure, figure(graphs/`rgroup'/`varname'.pdf) caption(`title')"' ///
		in `rownum'
		
}
	
}
end

cap prog drop missingness_check
program define missingness_check 
syntax, TEXDIRECTORY(string) ENUM(string)
qui {
	
local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65
local brek = char(10)
local tab = char(9)
local skip = "///" + char(10) + char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}
	
foreach r in 0 `replevels' {
	
	if `r' == 0 {
		
		local r_groupname survey
		local r_grouplabel Survey-Level
		
	}
	else {
		
 		frame `meta_repeats' { 
			
			levelsof name if index == `r', clean local(r_groupname)
			levelsof label if index == `r', clean local(r_grouplabel)
			
		}
		
	}
			
count if repeat_group == `replevel'
local repeats = ceil(`r(N)' / 50)

file write myfile _n(2) `"`lbanner'"' _n ///
    "*" _tab "Missingness" _n ///
	"`lbanner'"
	
if `replevel' == 0 {
	
	file write myfile _n(2) `"cap frame drop missingness"' _n ///
	`"frame create missingness strL varname strL varlabel int value int type int n ///"' _n ///
	_tab `"int enum strL enum_str int total_surveys int repeat_group"'
	
}

file write myfile _n(2) ///
	`"frame meta: valuesof name if repeat_group == `replevel' & `skip'"' ///
	`"question_type != 3 | question_type == 3 & within_order == 1"' _n ///
	`"local wanted_varlist \`r(values)'"' _n ///
	`"ds"' _n ///
	`"local available_varlist \`r(varlist)'"' _n ///
	`"local varlist : list available_varlist & wanted_varlist"' _n ///
	`"cap assert ustrregexm("\`: type `enum''", "str") == 0"' _n ///
	`"if _rc sencode `enum', replace"' _n(2) ///
	`"levelsof `enum', clean local(enums)"' _n ///
	`"local vals : value label `enum'"' _n ///
	`"local j = 0"' _n ///
	`"quietly foreach var of varlist \`varlist' {"' _n(2) ///
	_tab `"local ++j"' _n ///
	_tab `"local type : type \`var'"' _n ///
	_tab `"local conds "\`var' == .d" ///"' _n ///
	_tab `""\`var' == .r" ///"' _n ///
	_tab `""\`var' == .o" ///"' _n ///
	_tab `""missing(\`var')" ///"' _n ///
	_tab `""!missing(`var')""' _n(2) ///
	_tab `"local i = 0"' _n ///
	_tab `"foreach c in "\`conds'" {"' _n(2) ///
	_tab _tab `"local ++i"' _n ///
	_tab _tab `"if ustrregexm("\`: type \`var''", "^str") == 1 & \`i' <= 3 continue"' _n(2) ///
	_tab _tab `"count if \`c'"' _n ///
	_tab _tab `"local post = \`r(N)'"' _n(2) ///
	_tab _tab `"frame post missingness ("\`var'") ("\`: variable label \`var''") ///"' _n ///
	_tab _tab _tab `"(\`post') (\`i') (\`j') (.) ("") (\`c(N)') (`replevel')"' _n(2) ///
	_tab _tab `"foreach k in \`enums' {"' _n(2) ///
	_tab _tab _tab `"count if \`c' & `enum' == \`k'"' _n ///
	_tab _tab _tab `"local post = \`r(N)'"' _n(2) ///
	_tab _tab _tab `"count if `enum' == \`k'"' _n ///
	_tab _tab _tab `"local total_surveys = \`r(N)'"' _n(2) ///
	_tab _tab _tab `"frame post missingness ("\`var'") ("\`: variable label \`var''") ///"' _n ///
	_tab _tab _tab _tab `"(\`post') (\`i') (\`j') (\`k') ("\`: label \`vals' \`k''") ///"' _n ///
	_tab _tab _tab _tab `"(\`total_surveys')"' _n(2) ///
	_tab _tab `"}"' _n(2) _tab `"}"' _n(2) `"}"'
	
// 	`"frame missingness { "' _n ///
// 	`"	"' _n ///
// 	`"	label define type 1 `""Don't Know""' 2 `""Refused""' 3 `""Other (specify)""' ///"' _n ///
// 	`"		4 "Missing" 5 "Nonmissing""' _n ///
// 	`"	label values type type"' _n(2) ///
// 	`"	labmask n, values(varname)"' _n _n ///
// 	`"	labmask enum, values(enum_str)"' _n ///
// 	`"	bysort type: egen max = max(value)"' _n ///
// 	`"	gen relative_rate = value / max"' _n(2) ///
// 	`"	egen tag = group(varname)"' _n _n ///
// 	`"	label variable relative_rate "Proportion of all submissions""' _n ///
// 	`"	sum n"' _n ///
// 	`"	local num_vars = \`r(max)'"' _n ///
// 	`"	local display_num = 50"' _n ///
// 	`"	local rounds = ceil(\`num_vars' / \`display_num')"' _n ///
// 	`"	quietly forvalues i = 1/\`rounds' {"' _n _n ///
// 	`"		local min = (\`i' * \`display_num') - (\`display_num' - 1)"' _n ///
// 	`"		local max = min(\`num_vars', \`min' + (\`display_num' - 1))"' _n ///
// 	`"		"' _n ///
// 	`"		heatplot relative_rate i.n i.type if inrange(n, \`min', \`max') ///"' _n ///
// 	`"			& inrange(type, 1, 4) & missing(enum), ///"' _n ///
// 	`"				cuts(0(0.05)1) ///"' _n ///
// 	`"				ylabel(, labsize(7pt) nogrid) ///"' _n ///
// 	`"				xlabel(, angle(45) labsize(7pt)) ///"' _n ///
// 	`"				ysize(7) values(label(value) format(%1.0f) size(6pt)) legend(off) ///"' _n ///
// 	`"				title(`"{bf}Missing responses in survey Data"', pos(11) size(2.75)) ///"' _n ///
// 	`"				colors(plasma, intensity(.5) reverse) ///"' _n ///
// 	`"				p(lc(black) lalign(center) lwidth(thin)) ///"' _n ///
// 	`"				name(miss, replace) ///"' _n ///
// 	`"				ramp(title("Proportion", size(8pt)) labels(0(.2)1) ///"' _n ///
// 	`"				format(%2.1g))"' _n ///
// 	`"		graph export "`macval(texdirectory)'/graphs/`frame'/missingness_\`i'.pdf", as(pdf) replace"' _n ///
// 	`"				"' _n ///
// 	`"	}"' _n ///
// 	`"	"' _n ///
// 	`"}"' _n _n
/*
forvalues j = 1/5 {

	local label : label type `j'
	preserve

	keep if type == `j' & !missing(enum)
	if `c(N)' == 0 continue

	heatplot relative_rate i.enum i.n if inrange(n, `min', `max'), ///
		cuts(0(0.05)1) ///
		ylabel(, labsize(7pt) nogrid) ///
		xlabel(, angle(45) labsize(7pt)) ///
		ysize(7) values(label(value) format(%1.0f) size(6pt)) legend(off) ///
		title(`"{bf}`label' responses in survey Data"', pos(11) size(2.75)) ///
		colors(plasma, intensity(.5) reverse) ///
		p(lc(black) lalign(center) lwidth(thin)) ///
		name(miss_`i'_`j', replace) ///
		ramp(title("Proportion", size(8pt)) labels(0(.2)1) ///
		format(%2.1g))

	graph export ///
		"/Users/michaelrozelle/Library/CloudStorage/Dropbox/My Mac (Michaels-MBP)/Downloads/example/unops/3.tex/graphs/survey/missingness.pdf", ///
		name(miss_`i'_`j') as(pdf) replace

	restore

}
*/

// forvalues j = 1/`repeats' {
//	
// 	file write mytex _n(2) `"\begin{figure}[h!]"' _n ///
// 		`"\centering"' _n ///
// 		`"\caption{Missingness in `framename'}"' _n ///
// 		`"\includegraphics[width=\linewidth, height=\textheight, keepaspectratio]{graphs/`frame'/missingness_`j'.pdf}"' _n ///
// 		`"\end{figure}"'
//		
// }
			
}
end

cap prog drop sumstats_check 
prog define sumstats_check 
syntax , FRAME(string) REPLEVEL(integer) TEXDIRECTORY(string) ///
	TREATMENT(string) META(string) METAREPEATS(string) METAGROUPS(string) ///
	VCETYPE(string)
qui {

local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65
local brek = char(10)
local tab = char(9)
local skip = "///" + char(10) + char(9)
local dol = char(36)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}

levelsof group if repeat_group == `replevel', clean local(groups)
foreach g in `groups' {
	
	cwf `meta'
	local grouplabel : label group_label `g'
		
	if `replevel' > 0 {
			
		frame `metarepeats': levelsof name if index == `replevel', clean local(frame)
		
	}
	else local frame survey
	
	frame `metagroups': levelsof name if index == `g', clean local(groupname)
	if "`groupname'" == "" local groupname unnamed_group
	
	frame `meta' { 
		
		valuesof name if group == `g' & repeat_group == `replevel' ///
			& preloaded == 0 & !inlist(question_type, 1, 5, 6) ///
			& !(within_order == 1 & question_type == 3)
		local table `r(values)'
		
	}
	
	frame `frame' {
		
		local groupvars
		local passthru
		foreach var of varlist `table' {
			
			levelsof `treatment' if !missing(`var')
			if `r(r)' == 2 local passthru `passthru' `var'
			
			count if !missing(`var')
			if `r(N)' > 0 local groupvars `groupvars' `var'
			
		}
		
	}
	
	if "`groupvars'" != "" {
	
	file write myfile _n(2) `"`hbanner'"' _n `"*"' _tab `"`grouplabel'"' _n ///
		`"`hbanner'"' _n(2) ///
		`"`lbanner'"' _n `"*"' _tab `"Summary Statistics"' _n ///
		`"`lbanner'"' _n(2) ///
		`"local groupvars `groupvars'"' _n(2) ///
		`"est clear"' _n ///
		`"estpost tabstat \`groupvars', ///"' _n ///
		`"	c(stat) stat(mean sd min max n)"' _n ///
		`"esttab using "`macval(texdirectory)'/tables/`frame'/`groupname'_totsum.tex", replace ///"' _n ///
		`"	cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0)) count(fmt(0))") ///"' _n ///
		`"	nostar unstack nonumber ///"' _n ///
		`"	compress nomtitle nonote noobs label booktabs ///"' _n ///
		`"	title("`grouplabel' Headline Summary Statistics") ///"' _n ///
		`"	collabels("Mean" "SD" "Min" "Max" "N")"' _n ///
		`"latex_table, table("tables/`frame'/`groupname'_totsum.tex")"'
		
	}
	
	if "`passthru'" != "" {
	
		file write myfile _n(2) ///
		`"local table `passthru'"' _n(2) ///
		`"	est clear"' _n ///
		`"		estpost ttest \`table', by(`treatment')"' _n ///
		`"		esttab using "`macval(texdirectory)'/tables/`frame'/`groupname'.tex", replace ///"' _n ///
		`"		cells("mu_1(fmt(2)) mu_2(fmt(2))  b(star) se(par) count(fmt(0))") ///"' _n ///
		`"		collabels("Control" "Treatment" "Diff. (Control - Treatment)" "s.e." "obs." ) ///"' _n ///
		`"		star(* 0.10 ** 0.05 *** 0.01) ///"' _n ///
		`"		label booktabs nonum gaps noobs compress ///"' _n ///
		`"		title("`grouplabel' Summary Statistics across Treatments")"' _n(2) ///
		`"latex_table, table("tables/`frame'/`groupname'.tex")"' _n(2) ///
		`"local modelnames"' _n ///
		`"est clear"' _n ///
		`"foreach o in \`table' {"' _n(2) ///
		`"	eststo: reg \`o' i.`treatment', vce(`vcetype')"' _n(2) ///
		`"	sum \`o' if `treatment' == 0"' _n ///
		`"	local ctlm : display %3.2fc \`r(mean)'"' _n ///
		`"	estadd local ctlm \`ctlm'"' _n(2) ///
		`"	sum \`o' if `treatment' == 1"' _n ///
		`"	local trtm : display %3.2fc \`r(mean)'"' _n ///
		`"	estadd local trtm \`trtm'"' _n(2) ///
		`"	local varlabel : variable label \`o'"' _n ///
		`"	local varlabel = proper(strtrim(stritrim("\`varlabel'")))"' _n ///
		`"	local varlabel = ustrregexra("\`varlabel'", "(.{10,}?)\s", "\$1 \\\\")"' _n ///
		`"	local mlabel \shortstack{\`varlabel'}"' _n(2) ///
		`"	local modelnames `"\`modelnames' "\`mlabel'""'"' _n(2) ///
		`"}"' _n _n ///
		`"esttab using "`macval(texdirectory)'/tables/`frame'/`groupname'_teffects.tex", ///"' _n ///
		`"		b(3) se(3) ///"' _n ///
		`"		keep(1.`treatment') ///"' _n ///
		`"		star(* 0.10 ** 0.05 *** 0.01) ///"' _n ///
		`"		scalars( ///"' _n ///
		`"		"ctlm Mean in Control" ///"' _n ///
		`"		"trtm Mean in Treatment" ///"' _n ///
		`"		"N No. of Observations" "r2 \\`macval(dol)'R^2\\`macval(dol)'") ///"' _n ///
		`"		sfmt(%10.2f %10.2f  %10.0f %10.2f) ///"' _n ///
		`"		coef(1.`treatment' "Treatment") ///"' _n ///
		`"		mtitle(\`modelnames') ///"' _n ///
		`"		title("`grouplabel' Treatment Effects") ///"' _n ///
		`"		label booktabs noobs nonotes collabels(none) compress replace"' _n ///
		`"latex_table, table("tables/`frame'/`groupname'_teffects.tex")"'
		
	}
	
}
	
set graphics on 
	
}

end
