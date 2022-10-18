cls
clear all

*Directorio

if "`c(username)'"=="Mateo" {
    cd "C:\Economia\Octavo\Econ avanzada\Taller_2_Avanzada"
	gl path "C:\Economia\Octavo\Econ avanzada\Taller_2_Avanzada"
}

else{
	cd "C:\Users\caaya\OneDrive - Universidad de los Andes\universidad\8 semestre\Econometría avanzada\Talleres\Taller_2_Avanzada"
	
}

// Punto 1

* a.

*Importar los datos


gl punto "Punto 1"
gl data "data\\${punto}"
gl resultados "resultados\\${punto}"


use "${data}\Base_ENSIN",clear
rename Talla_niño Talla
*Revisión 
describe

tab Grupo_1

*Limpieza de datos
*Dado que solo se plantea un modelo especificado al grupo 1 E1, se pueden
*eliminar las observaciones que pertenecen a otros grupos dado que es excluyente.
drop if Grupo_1==.

drop Grupo_2 Grupo_3 Grupo_4 Grupo_5

save "${data}\Grupo_1.dta", replace
use "${data}\Grupo_1.dta", clear

use "${data}\Base_ENSIN",clear
rename Talla_niño Talla
drop if Grupo_2==.
drop Grupo_1 Grupo_1_E1 Grupo_1_E2 Grupo_3 Grupo_4 Grupo_5
save "${data}\Grupo_2.dta", replace

use "${data}\Base_ENSIN",clear
rename Talla_niño Talla
drop if Grupo_3==.
drop Grupo_1 Grupo_1_E1 Grupo_1_E2 Grupo_2 Grupo_4 Grupo_5
save "${data}\Grupo_3.dta", replace


use "${data}\Base_ENSIN",clear
rename Talla_niño Talla
drop if Grupo_4==.
drop Grupo_1 Grupo_1_E1 Grupo_1_E2 Grupo_3 Grupo_2 Grupo_5
save "${data}\Grupo_4.dta", replace


use "${data}\Base_ENSIN",clear
rename Talla_niño Talla
drop if Grupo_5==.
drop Grupo_1 Grupo_1_E1 Grupo_1_E2 Grupo_3 Grupo_4 Grupo_2
save "${data}\Grupo_5.dta", replace

use "${data}\Grupo_1", clear

*Generación de variables ^2
gen EducJefe2=Educacion_jefe^2
gen Ing_jefe2=Ingreso_jefe^2
gen ln_ingr_jefe=ln(Ingreso_jefe)
replace ln_ingr_jefe=0 if ln_ingr_jefe==.
gen ln_ing_jefe2=ln_ingr_jefe^2

*Modelo planteado

gl resultados "resultados\\${punto}"

global X Educacion Ingreso Ocu Raza Sex Per

reg Talla Grupo_1 $X
outreg2 using "${resultados}\modelo_a_ii.doc", replace

//reg Talla Grupo_1 Educ* ln* Ocu Raza Sex Per  
//outreg2 using "${path}\resultados\Punto 1\puntoa_ii.doc", addt

*Revisar aleatorización

*Subrgrupo 1

global vars Educacion_jefe Ingreso_jefe Ocupado_jefe Personas_hogar Raza_afro_indig Sexo
global D Grupo_1

reg Grupo_1 $vars, robust
outreg2 using "${resultados}\Balanceo_reg.doc", replace

foreach i in $D{
	mat balanceo_muestral_$D=J(6,4,.)
	mat rownames balanceo_muestral_$D= "Educacion" "Ingreso" "Ocupado" "Personas" "Raza" "Sexo"
	mat colnames balanceo_muestral_$D= "Y1_mean" "Y0_mean" "Y1-Y0" "p-value"
	
	mat l balanceo_muestral_$D

}

local filas=1
local tratamiento=1
forvalues i=1(1)2 {
	foreach x in $vars{
		di "`x' con $D = `tratamiento'"
		qui sum `x' if $D ==`tratamiento'
		mat balanceo_muestral_$D[`filas',`i']=r(mean)
		
		local filas=`filas'+1
	}
	local tratamiento=0
	local filas=1
}



local filas=1
local a r(mu_2)-r(mu_1)
foreach x in $vars{
	di "ttest de `x'"
	qui ttest `x', by($D) 
	mat balanceo_muestral_$D[`filas',3]=(r(mu_2)-r(mu_1), r(p))
	local filas=`filas'+1
}

mat l balanceo_muestral_$D


frmttable using "${resultados}\Balanceo.doc", s(balanceo_muestral_$D) sd(4) t("Tabla de Balanceo Muestral $D") n("Tabla 1.1") replace


*Subgrupo 1, etapa 2

global D Grupo_1_E2

reg Grupo_1_E2 $vars, robust
outreg2 using "${resultados}\Balanceo_reg.doc"

foreach i in $D{
	mat balanceo_muestral_$D=J(6,4,.)
	mat rownames balanceo_muestral_$D= "Educacion" "Ingreso" "Ocupado" "Personas" "Raza" "Sexo"
	mat colnames balanceo_muestral_$D= "Y1_mean" "Y0_mean" "Y1-Y0" "p-value"
	
	mat l balanceo_muestral_$D

}

local filas=1
local tratamiento=1
forvalues i=1(1)2 {
	foreach x in $vars{
		di "`x' con $D = `tratamiento'"
		qui sum `x' if $D ==`tratamiento'
		mat balanceo_muestral_$D[`filas',`i']=r(mean)
		
		local filas=`filas'+1
	}
	local tratamiento=0
	local filas=1
}



local filas=1
local a r(mu_2)-r(mu_1)
foreach x in $vars{
	di "ttest de `x'"
	qui ttest `x', by($D) 
	mat balanceo_muestral_$D[`filas',3]=(r(mu_2)-r(mu_1), r(p))
	local filas=`filas'+1
}

mat l balanceo_muestral_$D


frmttable using "${resultados}\Balanceo.doc", s(balanceo_muestral_$D) sd(4) t("Tabla de Balanceo Muestral $D") n("Tabla 1.3") addt

* Subgrupo 1 etapa 1

global D Grupo_1_E1

reg Grupo_1_E1 $vars, r
outreg2 using "${resultados}\Balanceo_reg.doc"

foreach i in $D{
	mat balanceo_muestral_$D=J(6,4,.)
	mat rownames balanceo_muestral_$D= "Educacion" "Ingreso" "Ocupado" "Personas" "Raza" "Sexo"
	mat colnames balanceo_muestral_$D= "Y1_mean" "Y0_mean" "Y1-Y0" "p-value"
	
	mat l balanceo_muestral_$D

}

local filas=1
local tratamiento=1
forvalues i=1(1)2 {
	foreach x in $vars{
		di "`x' con $D = `tratamiento'"
		qui sum `x' if $D ==`tratamiento'
		mat balanceo_muestral_$D[`filas',`i']=r(mean)
		
		local filas=`filas'+1
	}
	local tratamiento=0
	local filas=1
}



local filas=1
local a r(mu_2)-r(mu_1)
foreach x in $vars{
	di "ttest de `x'"
	qui ttest `x', by($D) 
	mat balanceo_muestral_$D[`filas',3]=(r(mu_2)-r(mu_1), r(p))
	local filas=`filas'+1
}

mat l balanceo_muestral_$D


frmttable using "${resultados}\Balanceo.doc", s(balanceo_muestral_$D) sd(4) t("Tabla de Balanceo Muestral $D") n("Tabla 1.2") addt

reg Grupo_1_E1 $vars if Nivel==2, robust


//c

*i. 

gen E1 =.
replace E1 = 1 if Grupo_1_E1==0
replace E1 = 0 if Grupo_1_E2==0
tab E1, missing


global D E1

foreach i in $D{
	mat balanceo_muestral_$D=J(6,4,.)
	mat rownames balanceo_muestral_$D= "Educacion" "Ingreso" "Ocupado" "Personas" "Raza" "Sexo"
	mat colnames balanceo_muestral_$D= "Y1_mean" "Y0_mean" "Y1-Y0" "p-value"
	
	mat l balanceo_muestral_$D

}

local filas=1
local tratamiento=1
forvalues i=1(1)2 {
	foreach x in $vars{
		di "`x' con $D = `tratamiento'"
		qui sum `x' if $D ==`tratamiento'
		mat balanceo_muestral_$D[`filas',`i']=r(mean)
		
		local filas=`filas'+1
	}
	local tratamiento=0
	local filas=1
}


local filas=1
local a r(mu_2)-r(mu_1)
foreach x in $vars{
	di "ttest de `x'"
	qui ttest `x', by($D) 
	mat balanceo_muestral_$D[`filas',3]=(r(mu_2)-r(mu_1), r(p))
	local filas=`filas'+1
}

mat l balanceo_muestral_$D

frmttable using "${resultados}\Balanceo.doc", s(balanceo_muestral_$D) sd(4) t("Tabla de Balanceo Muestral $D") n("Tabla 1.4") addt

sum Talla if Grupo_1_E1==0
sum Talla if Grupo_1_E2==0

global D SUTVA

foreach i in $D{
	mat balanceo_$D=J(1,4,.)
	mat rownames balanceo_$D= "Talla"
	mat colnames balanceo_$D= "Y1_mean" "Y0_mean" "Y1-Y0" "p-value"
	
	mat l balanceo_$D

}

sum Talla if E1==1
mat balanceo_SUTVA[1,1]=r(mean)

sum Talla if E1==0
mat balanceo_SUTVA[1,2]=r(mean)

ttest Talla, by(E1)
mat balanceo_SUTVA[1,3]=(r(mu_2)-r(mu_1), r(p))

mat l balanceo_SUTVA

frmttable using "${resultados}\Balanceo.doc", s(balanceo_SUTVA) sd(4) t("Diferencia de medias para E1") n("Tabla 1.5") addt

save "${data}\Grupo_1.dta", replace
use "${data}\Grupo_1.dta", clear


//d

*i. Efecto placebo

use "${data}\Grupo_2.dta", clear
reg Talla Grupo_2 $X // coeficiente negativo. No efecto placebo.
outreg2 using "${resultados}\sesgos_comportamentales_a_d.doc", replace


*ii. Efecto Hawthorne

use "${data}\Grupo_3.dta", clear
reg Talla Grupo_3 $X // Coeficiente positivo. Tratados cambian su comportamiento si son observados. Hay efecto Hawthorne.
outreg2 using "${resultados}\sesgos_comportamentales_a_d.doc"


*iii. Efecto Jhon Henry

use "${data}\Grupo_4.dta", clear
reg Talla Grupo_4 $X // Coeficiente positivo. No tratados cambian comportamiento para compensar su estado de no tratados. Hey efecto Henry.
outreg2 using "${resultados}\sesgos_comportamentales_a_d.doc"

*iv. Efecto Demanda Experimental

use "${data}\Grupo_5.dta", clear
reg Talla Grupo_5 $X // Coeficiente negativo. Los participantes cambian su comportamiento si interpretan el objetivo del experimento. Hay efecto Demanda Experimental.
outreg2 using "${resultados}\sesgos_comportamentales_a_d.doc"


//

/// Punto 2

gl punto "Punto 2"
gl data "data\\${punto}"
gl resultados "resultados\\${punto}"

import delimited using "${data}\country_ids.csv", delimiter(",") clear
save "${data}\country", replace

*c.

use "${data}\Card.dta", clear

gl X ires80 nres80 logsize80 logsize90 coll80 coll90 mfg80 mfg90

reg resgap2 relshs $X [w=count90], r
outreg2 using "${resultados}\punto c.doc", replace

reg resgap4 relscoll $X [w=count90], r
outreg2 using "${resultados}\punto c.doc"

ivreg2 resgap2 (relshs=hsiv) $X [w=count90], r first
outreg2 using "${resultados}\punto c.doc", adds(F, e(rkf)) 

ivreg2 resgap4 (relscoll=colliv) $X [w=count90], r first
outreg2 using "${resultados}\punto c.doc", adds(F, e(rkf))

* Estimar t para HS
reg relshs hsiv $X [w=count90], r
scalar alpha1_hs = e(b)[1,1]

ivreg2 resgap2 (relshs=hsiv) $X [w=count90], r first
scalar phi_hs = e(b)[1,1]

scalar t_hs = phi_hs/alpha1_hs
sca l t_hs

* Estimar t para Coll

reg relscoll colliv $X [w=count90], r
scalar alpha1_coll = e(b)[1,1]

ivreg2 resgap4 (relscoll=colliv) $X [w=count90], r first
scalar phi_coll = e(b)[1,1]

scalar t_coll = phi_coll/alpha1_coll
sca l t_coll

*d.

frame create country
frame change country

use "${data}\country", clear

tab country ind
br

/*

mex=1
phill=2
salv=5
china=6
cuba=7
west eur=31
b_hs= hsiv
b_coll= colliv

*/

frame change default

gl X logsize80 coll80 nres80 ires80 mfg80

*Mexico
reg shric1 $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) replace

*Phillipines
reg shric1 $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 

*El Salvador
reg shric1 $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 

*China
reg shric1 $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 

*Cuba
reg shric1 $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 

*West europe + otros
reg shric1 $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 

*Bartik hs
reg hsiv $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 

*Bartik coll
reg colliv $X [w=count90], r
outreg2 using "${resultados}\punto d.doc", stats(coef se) ///
		stnum(replace coef=coef*10000000, replace se=se*10000000) 


/// Punto 3

gl punto "Punto 3"
gl data "data\\${punto}"
gl resultados "resultados\\${punto}"

use "data\Punto 3\init_anon.dta", clear

*b.

rdrobust wage clasificacion, c(7) masspoints(off) all


*c.

rdrandinf wage clasificacion, c(7).

rdrandinf wage clasificacion, c(7) bernoulli(clasificacion) d(a)

*use "data\Punto 3\full_program.dta", clear





















