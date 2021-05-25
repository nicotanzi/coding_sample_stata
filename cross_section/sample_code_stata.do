*************************
*   Sample Code STATA   *
*     Nicolás Tanzi     *
*************************

*** SET UP ***

* WorkingDirectory
cd "D:\Estudio\Programacion\STATA\Rusia"
* Log-File 
log using registro, replace
* Packages
*net install grc1leg, from(http://www.stata.com/users/vwiggins) /* many graphs, one legend */
* Load data
use russia, clear

*** A LITTLE BIT OF LOOPS, MACRO USAGE AND GRAPHING ***

* Among different variables we got these:
describe satlif marsta* gender

* I'm curious about satisfaction with life
* Does marital status plays a role? Does gender?
* Let's do some plots

egen temp = rowtotal(marsta*)
gen marsta0 = 1-temp
label variable marsta0 "Forever alone" /* XD */
drop temp

forvalues k = 0(1)4{
local titulo : variable label marsta`k'
twoway (histogram satlif if gender==1 & marsta`k'==1, bin(5) color(red%30)) ///        
       (histogram satlif if gender==0 & marsta`k'==1, bin(5) color(green%30)), ///   
        name(H_`k') title("`titulo'") legend(order(1 "Male" 2 "Female" ))
}
quietly 
graph dir H_*
grc1leg `r(list)', l1(Density) legendfrom(H_1)
graph close H_*

* It looks interesting.


* Another interesting looking thing, is the variable:
describe hhpres 

* Does it have a relationship with any variable?
* Let's see:
ds hhpres, not 
foreach v of varlist `r(varlist)' {
	quietly ttest `v', by(hhpres)
	if r(p)<0.01 {
		display "`: var label `v''"
	}
} 


*** SOME CODE LINES WITH THE GOOD OLD OLS ***

* Suppose that we want to explain a continuos variable using another
* continuos variable, so we don't have many options 
describe totexp monage htself tincm_r
reg totexp monage

* Let's do bad things and overfit the regression using a polinomial
* Infact, let's compare the infamous R^2 across the degree of the
* polinomial

matrix mat_r = J(10,2,.)
forvalues k = 1(1)10{
	quietly gen monage_`k' = monage^`k'
	quietly ds monage_*
	quietly reg totexp `r(varlist)'
	quietly matrix mat_r[`k',2] = e(r2)
	quietly matrix mat_r[`k',1] = `k'
}
matrix list mat_r

drop _all
svmat double mat_r
matrix drop mat_r
twoway connected mat_r2 mat_r1, ytitle("R{sup:2}") xtitle("Polynomial degree")

use russia, clear

* Let's do a logit probit regression




* Let's use clustered standard errors


log close
