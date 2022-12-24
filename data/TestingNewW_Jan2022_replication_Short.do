/********************************************************************/
/* Introducing New W measure and testing public/private goods */
/********************************************************************/
/* Bruce Bueno de Mesquita and Alastair Smith January 21, 2022 */
/* Paper forthcoming at Social Science Quarterly */ 
#delimit;
clear; set mem 50m;version 17;
cd "";/*insert your working directory here*/
capture mkdir output; global store "output"; /* create a subdirectory to store output*/ 
pwd; frame change default;

/******** The main data sources: v-dem v11 downloaded March 18th 2021 */
/* Additional data: 
note: lifeExp popWB safewater hygiene measles electric secondaryschool from World Bank WDI; 
note: parcomp xropen xrcomp  democ autoc polity polity2 from Polity Project;
note: dd_Przeworski is Przeworksi democracy measure;
note: gwf_demo gwf_demo gwf_party gwf_military gwf_monarchy gwf_personal gwf_nonautocracy are Geddes et al institutional measures; 
note: food from FAOSTAT_data_9-2-2020 Average dietary energy supply adequacy (percent) (3-year average); 
note: transparencyindex for Hollyer, Rosendorff and Vreeland ; 
note: pressRestrict if Freedom House restriction on press freedom;
note: cpiscore is  Corruption Perception index from Tranparency International;
note: pop is from Maddison population data; */

/********************************************************************************/
/************************ Construct Institutional Measures **********************/
/********************************************************************************/
use "NewWmeasure.dta", clear;

/********************************************************************************/
/************************ Construct New W measure *******************************/
/********************************************************************************/
sort ccode year;	
/**** Estimating W: higher values on these variables are better conditions that allow for a larger W****/
/* standarize variables so that mean=0 and sd=1*/
sum v2elembaut v2psoppaut v2psbars  ;

/*** Components for a restrictive regime **/
sort country_name;
by country_name: carryforward v2x_ex_hereditary, gen(Hereditary);
by country_name: carryforward v2x_ex_military, gen(Military);
by country_name: carryforward v2x_ex_party, gen(Party);

gen Big= Hereditary if Hereditary>=Party&Hereditary>=Military & Hereditary~=.;
replace Big=Party if Party>=Hereditary&Party>=Military&Party~=.;
replace Big=Military if Military>=Hereditary & Military>=Party & Military~=.;
gen biginst=(1-Big); sum biginst; 
egen stdbiginst=std(biginst); 

egen stdelembaut=std(v2elembaut);  /*** autonomy of election monitoring body***/
egen stdoppaut=std(v2psoppaut); /*** opposition parties' autonomy***/;
egen stdpsbars=std(v2psbars); /*** Barriers to political party partiipation***/;
gen extrapol= (stdbiginst~=. & (v2elembaut==. | v2psoppaut==.| v2psbars==.));
 
gen W4temp=(stdelembaut+stdoppaut+stdpsbars+stdbiginst)/4;
sum W4temp; /* standardize to 0,1 */
gen W4= (W4temp -r(min))/(r(max)-r(min));; 
 
/**** Extropolate for those institutions with no electoral history ****/ 
/*** When there is zero electoral information (long established monarchies for instance) then set the electoral to the 1st percentile ***/;
sum v2elembaut, detail; replace v2elembaut=r(p1) if v2elembaut==.&biginst~=.; 
sum v2psoppaut, detail; replace v2psoppaut=r(p1) if v2psoppaut==.&biginst~=.;
sum v2psbars, detail; replace v2psbars=r(p1) if v2psbars==.&biginst~=.;
egen Xstdelembaut=std(v2elembaut);  /*** autonomy of election monitoring body***/
egen Xstdoppaut=std(v2psoppaut); /*** opposition parties' autonomy***/;
egen Xstdpsbars=std(v2psbars); /*** Barriers to political party partiipation***/;

gen XW4temp=(Xstdelembaut+Xstdoppaut+Xstdpsbars+stdbiginst)/4;
sum XW4temp; 
/* standardize to 0,1 */ 
gen XW4= (XW4temp -r(min))/(r(max)-r(min));

drop XW4temp Xstdpsbars Xstdoppaut Xstdelembaut W4temp extrapol stdpsbars stdoppaut stdelembaut stdbiginst biginst Big Party Military Hereditary;
/******** XXXXXXXXXXXXXXXXXXXXXXXXXXXXX ***********/
/* Use extrapolated version */
rename W4 W4a; rename XW4 W4; 
label var W4 "Measure of Winning Coalition Size based on 4 components of from V-Dem.";

/* Export the basic W variable */
/**********************************************/
/* Make Other Institutional Variables */ 
gen demaut =(polity2+10)/20;
label var demaut "Polity: (democracy score - autocracy score +10)/20";
rename demaut normpolity; 
/* Selectorate */
gen S_old=(LegislativeSelection)/2  ;
label var S_old "Selectorate size";
/* Old Coalition Size: W  */ 
gen W_old=0;
replace W_old=W_old+1 if (xrcomp>=2);
replace W_old=W_old+1 if (xropen>2);
replace W_old=W_old+1 if parcomp==5;
/* Banks regime type recorded only thru 2016: uopdate thru 2018 assuming no change */ 
replace RegType =RegType[_n-1] if year==2017 & year[_n-1]==2016;
replace RegType =RegType[_n-1] if year==2018 & year[_n-1]==2017;
replace W_old=W_old + 1 if (RegType~=. & RegType~=2 & RegType~=3);
replace W_old=W_old/4;
label var W_old  " Winning Coalition size (old)";

gen Democracy6=polity2>=6; replace Democracy6=. if polity2==.;
gen Democracy10=polity2==10; replace Democracy10=. if polity2==.;
 #delimit;
gen DemAnoAuto = 0 if polity2~=. ;
replace DemAnoAuto = 1 if polity2~=. &polity2>=6;
replace DemAnoAuto = -1 if polity2~=. &polity2<= -6;
#delimit; gen S=S_old;
carryforward v2x_suffr, gen(Suffrage);
replace S=Suffrage; 
/*** new code **/;
replace S=log(S+1);
rename v2regsupgroupssize sup; 
sum sup; gen support= (sup-r(min))/(r(max)-r(min)); label var support "Support";


/* Teorell and Lindberg 2019 */ 
global TL " v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party "; 
sum $TL;
sort ccode year;
/* Graph of Measures for US */ 
twoway  (line W4 year if ccode==2, lwidth(thick) lcolor(black))  (line normpolity year if ccode==2 ,lwidth(thick) lcolor(red) lpattern(dash))  (line   support year if ccode==2,lwidth(thick) lcolor(blue) lpattern(shortdash)) /*(line   support year if ccode==2,lwidth(thick) lcolor(green) lpattern(shortdash_dot))*/ , ytitle("W, Polity, Support") title("Institutional Measures for United States") legend(label(1 W)   label(2 Polity2) label(3 Support)) name(USinstitutions,replace);
graph export "${store}/USfigure.pdf", replace;

/* Graph of Measures for Russia */ 
twoway  (line W4 year if ccode==365, lwidth(thick) lcolor(black))  (line normpolity year if ccode==365 ,lwidth(thick) lcolor(red) lpattern(dash))  (line   support year if ccode==365,lwidth(thick) lcolor(blue) lpattern(shortdash)) /*(line   support year if ccode==2,lwidth(thick) lcolor(green) lpattern(shortdash_dot))*/ , ytitle("W, Polity, Support") title("Institutional Measures for Russia") legend(label(1 W)   label(2 Polity2) label(3 Support)) name(Russiainstitutions,replace);
graph export "${store}/Russiafigure.pdf", replace;
/* Graph of Measures for China */ 

twoway  (line W4 year if ccode==710, lwidth(thick) lcolor(black))  (line normpolity year if ccode==710 ,lwidth(thick) lcolor(red) lpattern(dash))  (line   support year if ccode==710,lwidth(thick) lcolor(blue) lpattern(shortdash)) /*(line   support year if ccode==2,lwidth(thick) lcolor(green) lpattern(shortdash_dot))*/ , ytitle("W, Polity, Support") title("Institutional Measures for China") legend(label(1 W)   label(2 Polity2) label(3 Support)) name(ChinaInstitutions,replace);
graph export "${store}/Chinafigure.pdf", replace;


/******************************************************************/
/******************************************************************/
/******************** Organize Dependent Variables ****************/
/******************************************************************/
/******************************************************************/
sum v2excrptps;
gen normCorruption=1-(v2excrptps-r(min))/(r(max)-r(min));
sum v2peapspol; 
gen normPublicGoods=(v2peapspol-r(min))/(r(max)-r(min));
gen ratiopublicprivate=(normPublicGoods)/(normPublicGoods+normCorruption);
gen logpop=log(POP);

/**** Analysis***/;
label var v2elfrfair "Free, Fair Elections";
label var v2exrescon "Exec. Respects Constitution";
label var v2x_clpol "Civil Liberties" /** High is good**/;
label var v2peapspol "Equal Access to Public Goods";
label var e_peinfmor "Infant Mortality";
label var e_pelifeex "Life Expectancy";
label var v2casurv "Campus Freedom";
label var v2xca_academ "Academic Freedom";
label var v2juhcind "Judicial Independence";
label var v2pehealth "Access to Health Care";
label var  e_fh_pr "Political Rights";
label var e_peaveduc  "Yrs Ed|Age>15";
label var e_migdppcln "log(income pc)";
label var v2x_rule "Rule of Law";
label var v2xcl_dmove "Free Movement";
label var v2xcl_slave "Free from Forced Labor";
label var v2xcl_prpty "Property Rights";
label var v2x_freexp "Free Expression";
gen PR_Up=(e_fh_pr-7)*-1;

global PublicGoods v2peapspol ratiopublicprivate v2dlencmps v2peapspol v2cltrnslw  v2dlcommon v2dlengage v2exrescon v2x_rule v2elfrfair v2xcl_dmove v2xcl_slave v2x_clpol PR_Up v2cltort v2clrelig v2juhcind v2jucorrdc v2xcl_prpty e_peinfmor v2casurv v2xca_academ v2pehealth v2x_freexp; 

label var v2cltrnslw "Transparent Laws";
label var v2cltort "High No Torture";
label var v2jucorrdc "High No Judicial Corruption";
label var v2x_frassoc_thick "Freedom of Association";
label var v2dlcommon "Elite Deliberation Guided by Common Good";
label var v2dlengage "Public Deliberation Guided by Common Good";
label var v2dlencmps "Particularistic or Public Goods Spending";
label var v2excrptps "Corruption" /**High is good***/;
label var v2x_execorr "Executive Allows Corruption" /* "Media Censorship"; * High is bad***/;;
label var v2exthftps "Public Sector Theft" /**low is bad***/;
label var v2mecenefm "Media Censorship"; 
label var v2jupack "Exec. Influences Court";
label var v2xpe_exlpol "Exclude Political Groups" /*High is bad***/;
label var v2xnp_client "Clientelism --Public $ for Pol Gain";
/****** Rename and recode variable to sensible names *****/
gen SlaveLabor = - v2xcl_slave;
gen Torture =-v2cltort; 
gen JudgeCorrupt=-v2jucorrdc;
rename ratiopublicprivate GoodsRatio; rename v2elfrfair FreeElections;
rename v2exrescon RespectConstitution; rename v2x_clpol CivilLiberties;
rename v2peapspol AccessPubGoods; rename e_peinfmor InfantMortality;
rename v2casurv CampusFree; rename v2xca_acad AcademicFreedom;
rename v2juhcind JudicialIndep;
rename v2pehealth HealthCare; rename PR_Up PolRights;
rename v2x_rule RuleofLaw;
rename v2xcl_dmove FreeMovement; rename v2xcl_prpty PropertyRights;
rename v2x_freexp FreeExpression; rename v2cltrnslw TransparentLaws;
rename v2dlcommon EliteWantCommonGood; rename v2dlengage PublicWantCommonGood;
rename v2dlencmps PrivateorPublicGoods; rename normCorruption Corruption;
rename v2x_execorr ExecutiveCorrupt; rename v2exthftps PublicSectorTheft; replace PublicSectorTheft=-PublicSectorTheft;
rename v2mecenefm MediaCensored; rename v2jupack PackCourts;
replace PackCourts=-PackCourts;
rename v2clrelig ReligiousFreedom;
rename v2xnp_client Clientelism;
rename normPublicGoods PublicGoods;
rename Democracy6 Dem6;
rename dd_Przeworski Przeworski;
replace v2clstown=-v2clstown;
rename v2clstown StateOwnsEconomy;
/***** Non- V-Dem Variables*****/
rename lifeExp LifeExpect;
rename  measles Vaccination ;
rename safewater CleanWater;
rename hygiene Hygiene ;
rename food FoodConsumption;
rename electric ElectricAccess;
rename   secondaryschool SecondarySchool;
rename  transparencyindex Transparencyindex;
rename  pressRestrict PressFreedom  ;
rename cpiscore CorruptionTI;


/************ Lists of Dependent Variables to be considered in groups ******/
#delimit;
global PublicGood PublicGoods GoodsRatio PrivateorPublicGoods  TransparentLaws EliteWantCommonGood PublicWantCommonGood
RespectConstitution RuleofLaw FreeElections FreeMovement SlaveLabor CivilLiberties PolRights Torture ReligiousFreedomv2clrelig  
v2juhcind v2jucorrdc v2xcl_prpty e_peinfmor v2casurv v2xca_academ v2pehealth v2x_freexp;

/* very general set of public goods */
global varlist1 PublicGood  GoodsRatio PrivateorPublicGoods  TransparentLaws  EliteWantCommonGood PublicWantCommonGood FreeExpression; 
/* Set of Private goods */ 
global varlist2 StateOwnsEconomy Corruption ExecutiveCorrupt PublicSectorTheft Clientelism JudgeCorrupt;
/* Additional public goods */
 
global varlist3 RespectConstitution RuleofLaw FreeElections FreeMovement CivilLiberties PolRights ReligiousFreedom  
JudicialIndep PropertyRights;

global varlist4 MediaCensored PackCourts Torture SlaveLabor  ;
global varlist5  InfantMortality CampusFree AcademicFreedom HealthCare; 
/* list of additional variables that dont come from V-Dem */

global varlist6  LifeExpect Vaccination CleanWater Hygiene HealthExpend FoodConsumption ElectricAccess EducExpend  SecondarySchool Transparency PressFreedom CorruptionTI ;  
/* which tables to run? */ global tabfrom =1; global tabto= 5;
/**************************************************/ 
/*
global varlist1  GoodsRatio;global varlist2  GoodsRatio;global varlist3  GoodsRatio;global varlist4  GoodsRatio;global varlist5  GoodsRatio;
*/
/****Useful labels and Programs  ********/ 

global scorelab2p "Favors W (\$ p<.01$)";global scorelab1p "Favors W (\$ 0.1\ge p>.01$)";
global scorelabneutral "Indeterminate";global scorelab2l "Opposes W (\$ p<.01$)"; global scorelab1l "Opposes W (\$ 0.1\ge p>.01$)";
/******. Programs to make and uppdate score cards *******/
capture program drop scorecardmake; 
program define scorecardmake;
args h ; forvalues hh =1/`h' {; 
 scalar w2p`hh' =0; scalar w1p`hh' =0;  scalar wneutral`hh'=0; scalar w1l`hh'=0;scalar w2l`hh'=0;}; 
end;
capture program drop scorecardupdate; 
program define scorecardupdate; 
/* update score card */ if twaldp${m}<.01 & twald${m} >0 {; scalar w2p${m}=w2p${m} +1; };
if twaldp${m}>=.01 & twaldp${m}<.1 & twald${m} >0 {; scalar w1p${m}=w1p${m} +1; };
if twaldp${m}<.01 & twald${m} <0 {; scalar w2l${m}=w2l${m} +1; };
if twaldp${m}>=.01 & twaldp${m}<.1 & twald${m} <0 {; scalar w1l${m}=w1l${m} +1; };
if twaldp${m}>=.1  {; scalar wneutral${m}=wneutral${m} +1; };
end;
#delimit; 
capture program drop AICscorecardmake; 
program define AICscorecardmake;
args h ; global aicList "";
forvalues hh =1/`h' {; 
 scalar aicWin`hh' =0;
 global aicList "$aicList  " " aicWin`hh' ";
 }; 
end;



#delimit;
capture program drop AICscoreupdate; 
program define AICscoreupdate;
args h; 
scalar a_min = 100000000;
forvalues  i =1/ `h' {;
if Waic`i' < a_min {;     scalar a_min = Waic`i'; 
     local i_min = `i' ; }; 
	 di Waic`i'; }; di "`i_min' ";
di a_min;scalar count=0;
forvalues  i =1/ `h' {;

if Waic`i' - a_min >10 {; scalar count= count+1; }; };
if count >= `h' -1 {; di "winner";   scalar aicWin`i_min' =aicWin`i_min' +1;};

end;

regen yr=year-1980,replace; 

#delimit; 
global NATT " e_migdppcln logpop c.yr c.yr#c.yr c.yr#c.yr#c.yr  i.ccode i.ccode#c.yr i.ccode#c.yr#c.yr  i.ccode#c.yr#c.yr#c.yr " ;



capture log close;
log using "${store}/NewWmeasure.smcl", replace;

/*****************************************************************************/ 
/*****************************************************************************/ 
/******* Comparision of W4, W-old, polity dem6 and Bois on a common set of observations ****/ 
/*****************************************************************************/ 
 /*****************************************************************************/ 

#delimit; 
global model W4 support  W_old normpolity Dem6 e_boix_regime ; 
set more off;
global titlelist ""Key Public Goods"  "Key Private Goods" "Fundamental Freedoms" "Abuse" "Health and Education" "Additional Measures" ";


capture  file close myfile;  

scorecardmake 6; 
AICscorecardmake 6; 
 /* loop thru 5 tables */ 
forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
file open myfile using "${store}/tableCommonObs`listnum'.tex", write replace;
file write myfile " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons: $wd \label{tab:tableCommonObs`listnum'}} " _n  ;
file write myfile " \begin{tabular}{|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfile " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} &  \multicolumn{1}{|c|}{W}  & \multicolumn{1}{|c|}{Support} &  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Dem6} & \multicolumn{1}{|c|}{Boix} \\  \hline   \hline  " _n ;


/********* Now run analysis and collect appropriate stats **********/

global listtorun varlist`listnum' ;
foreach i of global $listtorun {;

global m=1; 
di "`i"';
reghdfe `i' $model e_migdppcln logpop , a(ccode year) resid vce(clu ccode)  ;
  capture drop sampletorun; 
gen sampletorun=e(sample); scalar numobs = e(N); 
local var0 : display %5.0f scalar(numobs);
/* now do regression for each institutional measure */ 
foreach j of global model {;
reghdfe `i' `j' e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode) ;
 
mat bb= e(b); mat eV=e(V);
capture drop Wsm${m} ;capture capture drop Wsm${m}_2; predict Wsm${m},res;gen Wsm${m}_2=Wsm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S); /* Small sample correction to AIC: total parameters =k= e(df_m)+e(df_a)+1 (RE+Xs+cons)
AICc= -2 ll +2k +2k(k+1)/(n-k-1) */
  scalar Waic${m} =aic[1,5]; /*scalar Waic${m} =-2*e(ll)+ 2 * (e(df_m)+e(df_a)+1)  + 2*(e(df_m)+e(df_a)+1) *(e(df_m)+e(df_a)+2) /(e(N)-(e(df_m)+e(df_a)+1)-1)  ;*/
local aic$m :display %10.2f scalar(Waic${m}- Waic1);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});
if $m==1 {; local twald1 =0; local twaldp1 . ; };
if $m>1 {; capture drop diff; gen diff= Wsm${m}_2-Wsm1_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
scorecardupdate; 
 
local twald${m} : display %8.3f scalar(twald${m});
 local twaldp${m} : display %8.3f scalar(twaldp${m}); };
global m=$m +1;
dis $m;
};
AICscoreupdate 6;
global m=$m -1;
/*file write myfile `"`i'"';*/
file write myfile "`i'"/*=char(9)'`: variable label `i''"*/ ;
file write myfile "& $\hat{\beta}$ " ;
forvalues ii=1/$m {; 
file write myfile `" & `bb`ii''  "'; }; 
file write myfile     "\\ obs = `var0'   & \$ t=\hat{\beta}/\hat{se}$ "   ;
forvalues ii=1/$m {; 
file write myfile `" & `tt`ii''  "'; }; 
file write myfile     "\\  & $\Delta$ AIC  "   ;
forvalues ii=1/$m {; 
file write myfile `" & `aic`ii''   "'; };
file write myfile     "\\  & Vuong "   ;
forvalues ii=1/$m {; 
file write myfile `" & `twald`ii''   "'; };
 file write myfile "\\ \hline "; };
file write myfile     " \end{tabular} \end{center} \end{table}  "   ;

file close myfile; };

#delimit;
forvalues ii=1/6 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
 local dneutral`ii' : display %8.0f scalar(wneutral`ii');};

 
capture file close myfilescore;
file open  myfilescore using "${store}/scoreCommonObs`listnum'.tex", write replace;
file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard: Common Sample for All Tests (1800-2008) \label{tab:scoreCommonObs`listnum'}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score  & \multicolumn{1}{|c|}{Support} &    \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Dem6} & \multicolumn{1}{|c|}{Boix} 
  \\  \hline   \hline  " _n;
file write myfilescore " $scorelab2p   &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5' & `dw2p6'\\ " _n;
file write myfilescore " $scorelab1p &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5' & `dw1p6' \\ " _n;
file write myfilescore " $scorelabneutral  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5'& `dneutral6' \\ " _n;
file write myfilescore " $scorelab1l  &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5'  & `dw1l6'\\ " _n;
file write myfilescore " $scorelab2l   &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5'  & `dw2l6' \\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
 file close myfilescore;


 #delimit;
 capture file close myfilescore;
file open  myfilescore using "${store}/AICscores.tex", write replace;
file write myfilescore " AIC Scores : common pool "_n ;
file write myfilescore " W Support W (old) Polity}  Dem6 Boix "_n ;
file write myfilescore  `" `: display aicWin1' "' `" `: display aicWin2' "' `" `: display aicWin3' "' `" `: display aicWin4' "' `" `: display aicWin5' "' `" `: display aicWin6' "' _n;
file close myfilescore;

log close;

 
/*****************************************************************************/ 
/*****************************************************************************/ 
/******* Comparision of W4,support  W-old, polity dem6, Bois Przeworski and GWF and T&L on a common set of observations ****/ 
/*****************************************************************************/ 
 /*****************************************************************************/ 

#delimit; 
global model W4 W_old normpolity Dem6 e_boix_regime ; 
global model "W4 support W_old normpolity Dem6 e_boix_regime  Przeworski  "gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo" "v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party " "; 

set more off;
global titlelist ""Key Public Goods"  "Key Private Goods" "Fundamental Freedoms" "Abuse" "Health and Education" ";


capture log close; log using "${store}/tempFull",replace; 
capture  file close myfile;  

scorecardmake 9; 
AICscorecardmake 9; 

 /* loop thru 5 tables */ 
forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
file open myfile using "${store}/tableFullCommonObs`listnum'.tex", write replace;
file write myfile " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons: $wd \label{tab:tableCommonObs`listnum'}} " _n  ;
file write myfile " \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfile " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} &  \multicolumn{1}{|c|}{W} &  \multicolumn{1}{|c|}{Support} &  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Dem6} & \multicolumn{1}{|c|}{Boix} & \multicolumn{1}{|c|}{Przeworski}
 & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}\\  \hline   \hline  " _n ;

/********* Now run analysis and collect appropriate stats **********/

global listtorun varlist`listnum' ;
foreach i of global $listtorun {;

global m=1; 
di "`i"';
reghdfe `i' W4 support W_old normpolity Dem6 e_boix_regime  Przeworski gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo $TL e_migdppcln logpop , a(ccode year) resid vce(clu ccode)  ; capture drop sampletorun; 
gen sampletorun=e(sample); scalar numobs = e(N); 
local var0 : display %5.0f scalar(numobs);
/* now do regression for each institutional measure */ 
foreach j of global model {;
reghdfe `i' `j' e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode) ;
mat bb= e(b); mat eV=e(V);
capture drop Wsm${m} ;capture capture drop Wsm${m}_2; predict Wsm${m},res;gen Wsm${m}_2=Wsm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S); /* Small sample correction to AIC: total parameters =k= e(df_m)+e(df_a)+1 (RE+Xs+cons)
AICc= -2 ll +2k +2k(k+1)/(n-k-1) */
  scalar Waic${m} =aic[1,5]; scalar Waic${m} =-2*e(ll)+ 2 * (e(df_m)+e(df_a)+1)  + 2*(e(df_m)+e(df_a)+1) *(e(df_m)+e(df_a)+2) /(e(N)-(e(df_m)+e(df_a)+1)-1)  ;
local aic$m :display %10.2f scalar(Waic${m}- Waic1);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});
if $m==1 {; local twald1 =0; local twaldp1 . ; };
if $m>1 {; capture drop diff; gen diff= Wsm${m}_2-Wsm1_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
scorecardupdate; 
local twald${m} : display %8.3f scalar(twald${m});
 local twaldp${m} : display %8.3f scalar(twaldp${m}); };
global m=$m +1;
dis $m;
};
AICscoreupdate 9; 
global m=$m -1;
/*file write myfile `"`i'"';*/
file write myfile "`i'"/*=char(9)'`: variable label `i''"*/ ;
file write myfile "& $\hat{\beta}$ " ;
forvalues ii=1/$m {; 
file write myfile `" & `bb`ii''  "'; }; 
file write myfile     "\\ obs = `var0'   & \$ t=\hat{\beta}/\hat{se}$ "   ;
forvalues ii=1/$m {; 
file write myfile `" & `tt`ii''  "'; }; 
file write myfile     "\\  & $\Delta$ AIC  "   ;
forvalues ii=1/$m {; 
file write myfile `" & `aic`ii''   "'; };
file write myfile     "\\  & Vuong "   ;
forvalues ii=1/$m {; 
file write myfile `" & `twald`ii''   "'; };
 file write myfile "\\ \hline "; };
file write myfile     " \end{tabular} \end{center} \end{table}  "   ;

file close myfile; };


#delimit;
forvalues ii=1/9 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
 local dneutral`ii' : display %8.0f scalar(wneutral`ii');};

 
 

capture file close myfilescore;

file open myfilescore using "${store}/scoreFullCommonObs`listnum'.tex", write replace;
file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard: Common Sample for All Tests (1800-2008) \label{tab:scoreCommonObs`listnum'}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score &     \multicolumn{1}{|c|}{Support} &  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Dem6} & \multicolumn{1}{|c|}{Boix} 
  & \multicolumn{1}{|c|}{Przeworski}
 & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL} \\  \hline   \hline  " _n;
file write myfilescore " $scorelab2p   &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5' & `dw2p6'& `dw2p7' & `dw2p8' & `dw2p9'\\ " _n;
file write myfilescore " $scorelab1p &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5'& `dw1p6' & `dw1p7'& `dw1p8' & `dw1p9'\\ " _n;
file write myfilescore " $scorelabneutral  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5'& `dneutral6' & `dneutral7' & `dneutral8' & `dneutral9'\\ " _n;
file write myfilescore " $scorelab1l  &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5' & `dw1l6'& `dw1l7' & `dw1l8'& `dw1l9'\\ " _n;
file write myfilescore " $scorelab2l   &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5' & `dw2l6'& `dw2l7' & `dw2l8'  & `dw2l9'\\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
 file close myfilescore;

 
 #delimit;
 capture file close myfilescore;
file open  myfilescore using "${store}/AICscores.tex", write append;
file write myfilescore " AIC Scores : common pool: extra var "_n ;
file write myfilescore " W Support W (old) Polity}  Dem6 Boix  Prezeworksi. GWF TL"_n ;
file write myfilescore  `" `: display aicWin1' "' `" `: display aicWin2' "' `" `: display aicWin3' "' `" `: display aicWin4' "' `" `: display aicWin5' "' `" `: display aicWin6' "' `" `: display aicWin7' "'  `" `: display aicWin8' "' `" `: display aicWin9' "' _n;
file close myfilescore;
 
log close;


/*****************************************************************************/ 
/**************Autocracies *************************************/ 
/******* Comparision of W4, W-old, polity dem6, Bois Przeworski and GWF on a common set of observations ****/ 
/*****************************************************************************/ 
 /*****************************************************************************/ 

#delimit; 
global model W4 W_old normpolity Dem6 e_boix_regime ; 
global model "W4 support W_old normpolity  e_boix_regime  Przeworski  "gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo" "v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party " "; 

set more off;
global titlelist ""Key Public Goods"  "Key Private Goods" "Fundamental Freedoms" "Abuse" "Health and Education" "Additional Policies" ";


capture log close; log using "${store}/tempFullAuto",replace; 
capture  file close myfile;  

scorecardmake 8; 
AICscorecardmake 8; 
 /* loop thru 5 tables */ 
forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
file open myfile using "${store}/tableFullCommonObsAuto`listnum'.tex", write replace;
file write myfile " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons: Autocracies: $wd \label{tab:tableCommonObsAuto`listnum'}} " _n  ;

file write myfile " \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfile " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} & 
 \multicolumn{1}{|c|}{W} & 
 \multicolumn{1}{|c|}{Support} &  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} &  \multicolumn{1}{|c|}{Boix} 
 & \multicolumn{1}{|c|}{Przeworski}
 & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}\\  \hline   \hline  " _n ;

/********* Now run analysis and collect appropriate stats **********/

global listtorun varlist`listnum' ;
foreach i of global $listtorun {;

global m=1; 
di "`i"';
reghdfe `i' W4 W_old normpolity  DemAnoAuto e_boix_regime  Przeworski gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party e_migdppcln logpop if DemAnoAuto<1, a(ccode year) resid vce(clu ccode)  ; capture drop sampletorun; 
gen sampletorun=e(sample); scalar numobs = e(N); 
local var0 : display %5.0f scalar(numobs);
/* now do regression for each institutional measure */ 
foreach j of global model {;
reghdfe `i' `j' e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode) ;
mat bb= e(b); mat eV=e(V);
capture drop Wsm${m} ;capture capture drop Wsm${m}_2; predict Wsm${m},res;gen Wsm${m}_2=Wsm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S); /* Small sample correction to AIC: total parameters =k= e(df_m)+e(df_a)+1 (RE+Xs+cons)
AICc= -2 ll +2k +2k(k+1)/(n-k-1) */
  scalar Waic${m} =aic[1,5]; scalar Waic${m} =-2*e(ll)+ 2 * (e(df_m)+e(df_a)+1)  + 2*(e(df_m)+e(df_a)+1) *(e(df_m)+e(df_a)+2) /(e(N)-(e(df_m)+e(df_a)+1)-1)  ;
local aic$m :display %10.2f scalar(Waic${m}- Waic1);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});
if $m==1 {; local twald1 =0; local twaldp1 . ; };
if $m>1 {; capture drop diff; gen diff= Wsm${m}_2-Wsm1_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
scorecardupdate; 
local twald${m} : display %8.3f scalar(twald${m});
 local twaldp${m} : display %8.3f scalar(twaldp${m}); };
global m=$m +1;
dis $m;
};
AICscoreupdate 8;
global m=$m -1;
/*file write myfile `"`i'"';*/
file write myfile "`i'"/*=char(9)'`: variable label `i''"*/ ;
file write myfile "& $\hat{\beta}$ " ;
forvalues ii=1/$m {; 
file write myfile `" & `bb`ii''  "'; }; 
file write myfile     "\\ obs = `var0'   & \$ t=\hat{\beta}/\hat{se}$ "   ;
forvalues ii=1/$m {; 
file write myfile `" & `tt`ii''  "'; }; 
file write myfile     "\\  & $\Delta$ AIC  "   ;
forvalues ii=1/$m {; 
file write myfile `" & `aic`ii''   "'; };
file write myfile     "\\  & Vuong "   ;
forvalues ii=1/$m {; 
file write myfile `" & `twald`ii''   "'; };
 file write myfile "\\ \hline "; };
file write myfile     " \end{tabular} \end{center} \end{table}  "   ;

file close myfile; };

#delimit;
#delimit;
forvalues ii=1/8 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
 local dneutral`ii' : display %8.0f scalar(wneutral`ii');};

capture file close myfilescore;
file open myfilescore using "${store}/scoreFullCommonObsAuto`listnum'.tex", write replace;
file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard Autocracies: Common Sample for All Tests (1800-2008) \label{tab:scoreCommonObsAuto`listnum'}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score &  \multicolumn{1}{|c|}{Support}&   \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2}  & \multicolumn{1}{|c|}{Boix} 
  & \multicolumn{1}{|c|}{Przeworski}  
 & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}\\  \hline   \hline  " _n;
file write myfilescore " $scorelab2p   &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5' & `dw2p6'& `dw2p7' & `dw2p8' \\ " _n;
file write myfilescore " $scorelab1p &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5'& `dw1p6' & `dw1p7' & `dw1p8'\\ " _n;
file write myfilescore " $scorelabneutral  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5'& `dneutral6' & `dneutral7'  & `dneutral8' \\ " _n;
file write myfilescore " $scorelab1l  &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5' & `dw1l6'& `dw1l7' & `dw1l8'\\ " _n;
file write myfilescore " $scorelab2l   &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5' & `dw2l6'& `dw2l7'  & `dw2l8'\\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
 file close myfilescore;

 #delimit;
 capture file close myfilescore;
file open  myfilescore using "${store}/AICscores.tex", write append;
file write myfilescore " AIC Scores : common pool: Autocracies "_n ;
file write myfilescore " W Support W (old) Polity}   Boix  Prezeworksi. GWF TL"_n ;
file write myfilescore  `" `: display aicWin1' "' `" `: display aicWin2' "' `" `: display aicWin3' "' `" `: display aicWin4' "' `" `: display aicWin5' "' `" `: display aicWin6' "' `" `: display aicWin7' "'  `" `: display aicWin8' "'  _n;
file close myfilescore;
   
log close;



/*****************************************************************************/ 
/**************Democracies *************************************/ 
/******* Comparision of W4, W-old, polity dem6, Bois Przeworski and GWF on a common set of observations ****/ 
/*****************************************************************************/ 
 /*****************************************************************************/ 

#delimit; 
global model W4 W_old normpolity Dem6 e_boix_regime ; 
global model "W4 support W_old normpolity  e_boix_regime  Przeworski  "gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo" "v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party " "; 

set more off;
global titlelist ""Key Public Goods"  "Key Private Goods" "Fundamental Freedoms" "Abuse" "Health and Education" ";


capture log close; log using "${store}/tempFullDemo",replace; 
capture  file close myfile;  

scorecardmake 8; 
AICscorecardmake 8; 
 /* loop thru 5 tables */ 
forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
file open myfile using "${store}/tableFullCommonObsDemo`listnum'.tex", write replace;
file write myfile " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons: Democracies: $wd \label{tab:tableCommonObs`listnum'}} " _n  ;
file write myfile " \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfile " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} &  \multicolumn{1}{|c|}{W} & \multicolumn{1}{|c|}{Support} & \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Boix} & \multicolumn{1}{|c|}{Przeworski}
 & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}\\  \hline   \hline  " _n ;

/********* Now run analysis and collect appropriate stats **********/

global listtorun varlist`listnum' ;
foreach i of global $listtorun {;

global m=1; 
di "`i"';
reghdfe `i' W4 support W_old normpolity   e_boix_regime  Przeworski gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party e_migdppcln logpop if DemAnoAuto==1, a(ccode year) resid vce(clu ccode)  ; capture drop sampletorun; 
gen sampletorun=e(sample); scalar numobs = e(N); 
local var0 : display %5.0f scalar(numobs);
/* now do regression for each institutional measure */ 
foreach j of global model {;
reghdfe `i' `j' e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode) ;
mat bb= e(b); mat eV=e(V);
capture drop Wsm${m} ;capture capture drop Wsm${m}_2; predict Wsm${m},res;gen Wsm${m}_2=Wsm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S); /* Small sample correction to AIC: total parameters =k= e(df_m)+e(df_a)+1 (RE+Xs+cons)
AICc= -2 ll +2k +2k(k+1)/(n-k-1) */
  scalar Waic${m} =aic[1,5]; scalar Waic${m} =-2*e(ll)+ 2 * (e(df_m)+e(df_a)+1)  + 2*(e(df_m)+e(df_a)+1) *(e(df_m)+e(df_a)+2) /(e(N)-(e(df_m)+e(df_a)+1)-1)  ;
local aic$m :display %10.2f scalar(Waic${m}- Waic1);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});
if $m==1 {; local twald1 =0; local twaldp1 . ; };
if $m>1 {; capture drop diff; gen diff= Wsm${m}_2-Wsm1_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
scorecardupdate; 
local twald${m} : display %8.3f scalar(twald${m});
 local twaldp${m} : display %8.3f scalar(twaldp${m}); };
global m=$m +1;
dis $m;
};
AICscoreupdate 8;
global m=$m -1;
/*file write myfile `"`i'"';*/
file write myfile "`i'"/*=char(9)'`: variable label `i''"*/ ;
file write myfile "& $\hat{\beta}$ " ;
forvalues ii=1/$m {; 
file write myfile `" & `bb`ii''  "'; }; 
file write myfile     "\\ obs = `var0'   & \$ t=\hat{\beta}/\hat{se}$ "   ;
forvalues ii=1/$m {; 
file write myfile `" & `tt`ii''  "'; }; 
file write myfile     "\\  & $\Delta$ AIC  "   ;
forvalues ii=1/$m {; 
file write myfile `" & `aic`ii''   "'; };
file write myfile     "\\  & Vuong "   ;
forvalues ii=1/$m {; 
file write myfile `" & `twald`ii''   "'; };
 file write myfile "\\ \hline "; };
file write myfile     " \end{tabular} \end{center} \end{table}  "   ;

file close myfile; };

#delimit;
#delimit;forvalues ii=1/8 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
 local dneutral`ii' : display %8.0f scalar(wneutral`ii');};

capture file close myfilescore;
file open myfilescore using "${store}/scoreFullCommonObsDemo`listnum'.tex", write replace;
file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard Democracies: Common Sample for All Tests (1800-2008) \label{tab:scoreCommonObsDemo`listnum'}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score &  \multicolumn{1}{|c|}{Support}&   \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Boix} 
  & \multicolumn{1}{|c|}{Przeworski}
 & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}\\  \hline   \hline  " _n;
file write myfilescore " $scorelab2p   &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5' & `dw2p6' & `dw2p7'& `dw2p8' \\ " _n;
file write myfilescore " $scorelab1p &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5'& `dw1p6' & `dw1p7'& `dw1p8' \\ " _n;
file write myfilescore " $scorelabneutral  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5'& `dneutral6' & `dneutral7' & `dneutral8' \\ " _n;
file write myfilescore " $scorelab1l  &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5' & `dw1l6' & `dw1l7' & `dw1l8'\\ " _n;
file write myfilescore " $scorelab2l   &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5' & `dw2l6' & `dw2l7' & `dw2l8'\\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
 file close myfilescore;

 
 
 
 
#delimit;
 capture file close myfilescore;
file open  myfilescore using "${store}/AICscores.tex", write append;
file write myfilescore " AIC Scores : common pool: Democracies "_n ;
file write myfilescore " W Support W (old) Polity}   Boix  Prezeworksi. GWF TL"_n ;
file write myfilescore  `" `: display aicWin1' "' `" `: display aicWin2' "' `" `: display aicWin3' "' `" `: display aicWin4' "' `" `: display aicWin5' "' `" `: display aicWin6' "' `" `: display aicWin7' "'  `" `: display aicWin8' "'  _n;
file close myfilescore;
  
 
 
log close;





 

/**************************************************************************/
/**********************Pairwise Comparisons *******************************/
/**************************************************************************/

#delimit; 
scorecardmake 8;

global model " support W_old normpolity Dem6 e_boix_regime  Przeworski  "gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo" "v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party " ";  
set more off;
capture log close; log using "${store}/tempPW",replace; 
capture  file close myfilepw;  

 /* loop thru 5 tables */ 
 forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
capture  file close myfilepw;
file open myfilepw using "${store}/tablePWObs`listnum'.tex", write replace;
file write myfilepw " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons (Pairwise): $wd \label{tab:tablePWObs`listnum'}} " _n  ;
file write myfilepw " \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}   \hline " _n ; 
file write myfilepw " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} & \multicolumn{1}{|c|}{Support}&  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Dem6} & \multicolumn{1}{|c|}{Boix} 
 & \multicolumn{1}{|c|}{Przeworski} & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL} \\  \hline   \hline  " _n ;

/********* Now run analysis and collect appropriate stats **********/


global listtorun varlist`listnum' ;
foreach i of global $listtorun {;

global m=1; 

/* now do regression for each institutional measure */ 
foreach j of global model {;
di "`j'";
local jj =substr("`j'",1,8);
di "`jj'"; 
reghdfe `i' W4 e_migdppcln logpop if W4~=. & `jj'~=. , a(ccode year) resid vce(clu ccode) ;
capture drop Wsm ;capture capture drop Wsm_2; predict Wsm,res;gen Wsm_2=Wsm^2;
estat ic;mat aic=r(S);  scalar Waic =aic[1,5]; 
scalar numobs = e(N); 
local var${m} : display %5.0f scalar(numobs);
reghdfe `i' `j' e_migdppcln logpop if W4~=. & `jj'~=. , a(ccode year) resid vce(clu ccode) ;
mat bb= e(b); mat eV=e(V);
capture drop sm${m} ;capture capture drop sm${m}_2; predict sm${m},res;gen sm${m}_2=sm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S);  scalar Waic${m} =aic[1,5];
local aic$m :display %10.2f scalar(Waic${m}- Waic);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});

capture drop diff; gen diff= sm${m}_2-Wsm_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
/* update score card */ scorecardupdate; 
local twald${m} : display %8.3f scalar(twald${m});
 local twaldp${m} : display %8.3f scalar(twaldp${m}); 

global m=$m +1;
dis $m;
};

global m=$m -1;

file write myfilepw "`i' & Obs. "/*=char(9)'`: variable label `i''"*/ ;
forvalues ii=1/$m {; 
file write myfilepw `" & `var`ii''  "'; }; 
/*file write myfilepw     "\\  &  $\Delta$ AIC "   ;
forvalues ii=1/$m {; 
file write myfilepw `" & `aic`ii''  "'; }; */

file write myfilepw     "\\  &  Vuong "   ;
forvalues ii=1/$m {; 
file write myfilepw `" & `twald`ii''  "'; }; 
 file write myfilepw "\\ \hline "; };
file write myfilepw     " \end{tabular} \end{center} \end{table}  "   ;

file close myfilepw; };

#delimit;
forvalues ii=1/8 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
 local dneutral`ii' : display %8.0f scalar(wneutral`ii');};

capture file close myfilescore;
file open myfilescore using "${store}/scorePW.tex", write replace;

file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard: Pairwise Samples  \label{tab:scorePW}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score &   \multicolumn{1}{|c|}{Support}&  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Dem6} & \multicolumn{1}{|c|}{Boix} & \multicolumn{1}{|c|}{Przeworski} 
  & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL} \\  \hline   \hline  " _n;
file write myfilescore " $scorelab2p &`dw2p1'  &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5'& `dw2p6' & `dw2p7'& `dw2p8' \\ " _n;
file write myfilescore " $scorelab1p  &`dw1p1' &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5' & `dw1p6'  & `dw1p7' & `dw1p8'\\ " _n;
file write myfilescore " $scorelabneutral &`dneutral1'  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5' & `dneutral6' & `dneutral7' & `dneutral8'\\ " _n;
file write myfilescore " $scorelab1l &`dw1l1' &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5' & `dw1l6' & `dw1l7' & `dw1l8'\\ " _n;
file write myfilescore " $scorelab2l &`dw2l1' &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5' & `dw2l6' & `dw2l7' & `dw2l8'\\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
file close myfilescore;

/******************************************************************************/ 
/******************** Pairwise Comparisons: Focus only on Non-Democracies *****/ 
/******************************************************************************/ 
#delimit; 
scorecardmake 8;

global model " support W_old normpolity DemAnoAuto e_boix_regime  Przeworski  "gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo" "v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party " "; 
set more off;
capture log close; log using "${store}/AUTOPW",replace; 
capture  file close myfilepw;  

 /* loop thru 5 tables */ 
 forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
capture  file close myfilepw;
file open myfilepw using "${store}/tablePWAuto`listnum'.tex", write replace;
file write myfilepw " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons for Autocracies (Pairwise): $wd \label{tab:tablePWAuto`listnum'}} " _n  ;
file write myfilepw " \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}   \hline " _n ; 
file write myfilepw " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} 
&    \multicolumn{1}{|c|}{Support} &    \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2}&\multicolumn{1}{|c|}{Anoc.} & \multicolumn{1}{|c|}{Boix} 
 & \multicolumn{1}{|c|}{Przeworski} & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL} \\  \hline   \hline  " _n ;

/********* Now run analysis and collect appropriate stats **********/
global listtorun varlist`listnum' ;
foreach i of global $listtorun {;
global m=1; 
/* now do regression for each institutional measure */ 
#delimit;
foreach j of global model {;
di "`j'";
local jj =substr("`j'",1,8);
di "`jj'"; 
reghdfe `i' W4 e_migdppcln logpop if W4~=. & `jj'~=. & DemAnoAuto<1, a(ccode year) resid vce(clu ccode) ;
capture drop Wsm ;capture capture drop Wsm_2; predict Wsm,res;gen Wsm_2=Wsm^2;
estat ic;mat aic=r(S);  scalar Waic =aic[1,5]; 
scalar numobs = e(N); 
local var${m} : display %5.0f scalar(numobs);
reghdfe `i' `j' e_migdppcln logpop if W4~=. & `jj'~=. &DemAnoAuto<1, a(ccode year) resid vce(clu ccode) ;
mat bb= e(b); mat eV=e(V);
capture drop sm${m} ;capture capture drop sm${m}_2; predict sm${m},res;gen sm${m}_2=sm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S);  scalar Waic${m} =aic[1,5];
local aic$m :display %10.2f scalar(Waic${m}- Waic);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});
capture drop diff; gen diff= sm${m}_2-Wsm_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
/* update score card */ scorecardupdate; 
local twald${m} : display %8.3f scalar(twald${m});
local twaldp${m} : display %8.3f scalar(twaldp${m}); 
global m=$m +1;
dis $m;
};

global m=$m -1;
file write myfilepw "`i' & Obs. "/*=char(9)'`: variable label `i''"*/ ;
forvalues ii=1/$m {; 
file write myfilepw `" & `var`ii''  "'; }; 
/*file write myfilepw     "\\  &  $\Delta$ AIC "   ;
forvalues ii=1/$m {; 
file write myfilepw `" & `aic`ii''  "'; }; */
file write myfilepw     "\\  &  Vuong "   ;
forvalues ii=1/$m {; 
file write myfilepw `" & `twald`ii''  "'; }; 
 file write myfilepw "\\ \hline "; };
file write myfilepw     " \end{tabular} \end{center} \end{table}  "   ;
file close myfilepw; };

forvalues ii=1/8 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
 local dneutral`ii' : display %8.0f scalar(wneutral`ii');};
capture file close myfilescore;

file open myfilescore using "${store}/scorePWAuto.tex", write replace;
file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard: Autocracies (Polity2 \$<6$, Pairwise Samples)  \label{tab:scorePWAuto}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score &    \multicolumn{1}{|c|}{Support}&    \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2}& \multicolumn{1}{|c|}{Anoc.}  & \multicolumn{1}{|c|}{Boix}
 & \multicolumn{1}{|c|}{Przeworski} & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}
  \\  \hline   \hline  " _n;
  file write myfilescore " $scorelab2p &`dw2p1'  &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5'& `dw2p6' & `dw2p7'& `dw2p8' \\ " _n;
file write myfilescore " $scorelab1p  &`dw1p1' &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5' & `dw1p6'  & `dw1p7' & `dw1p8'\\ " _n;
file write myfilescore " $scorelabneutral &`dneutral1'  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5' & `dneutral6' & `dneutral7' & `dneutral8'\\ " _n;
file write myfilescore " $scorelab1l &`dw1l1' &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5' & `dw1l6' & `dw1l7' & `dw1l8'\\ " _n;
file write myfilescore " $scorelab2l &`dw2l1' &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5' & `dw2l6' & `dw2l7' & `dw2l8'\\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
file close myfilescore;
 

/******************************************************************************/ 
/******************** Pairwise Comparisons: Focus only Democracies ************/ 
/******************************************************************************/ 
#delimit;
scorecardmake 7;
global model "support W_old normpolity  e_boix_regime  Przeworski  "gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo" "v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party " "; 
set more off;
capture log close; log using "${store}/DEMOPW",replace; 
capture  file close myfilepw;  

 /* loop thru 5 tables */ 
 forvalues  listnum =$tabfrom / $tabto {; 
 global wd: word `listnum' of $titlelist;
 di "$wd";
capture  file close myfilepw;
file open myfilepw using "${store}/tablePWdemo`listnum'.tex", write replace;
file write myfilepw " \begin{table}[!h]  \begin{center}
\caption{Model Comparisons for Democracies (Pairwise): $wd \label{tab:tablePWdemo`listnum'}} " _n  ;
file write myfilepw " \begin{tabular}{|c|c|c|c|c|c|c|c|c|}   \hline " _n ; 
file write myfilepw " \multicolumn{1}{|c|}{Variable} & \multicolumn{1}{|c|}{Stats} 
&    \multicolumn{1}{|c|}{Support} &    \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2} & \multicolumn{1}{|c|}{Boix} 
 & \multicolumn{1}{|c|}{Przeworski} & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL} \\  \hline   \hline  " _n ;

/********* Now run analysis and collect appropriate stats **********/
global listtorun varlist`listnum' ;
foreach i of global $listtorun {;
global m=1; 
/* now do regression for each institutional measure */ 
#delimit;
foreach j of global model {;
di "`j'";
local jj =substr("`j'",1,8);
di "`jj'"; 
reghdfe `i' W4 e_migdppcln logpop if W4~=. & `jj'~=. & DemAnoAuto==1, a(ccode year) resid vce(clu ccode) ;
capture drop Wsm ;capture capture drop Wsm_2; predict Wsm,res;gen Wsm_2=Wsm^2;
estat ic;mat aic=r(S);  scalar Waic =aic[1,5]; 
scalar numobs = e(N); 
local var${m} : display %5.0f scalar(numobs);
reghdfe `i' `j' e_migdppcln logpop if W4~=. & `jj'~=. &DemAnoAuto==1, a(ccode year) resid vce(clu ccode) ;
mat bb= e(b); mat eV=e(V);
capture drop sm${m} ;capture capture drop sm${m}_2; predict sm${m},res;gen sm${m}_2=sm${m}^2;
mat bb= e(b); mat eV=e(V); scalar eV${m}=eV[1,1]; 
scalar Wb${m}=bb[1,1]; scalar rsq${m}= e(r2) ; local rsq${m} :display %6.3f scalar(rsq${m});
estat ic;mat aic=r(S);  scalar Waic${m} =aic[1,5];
local aic$m :display %10.2f scalar(Waic${m}- Waic);
local bb$m : display %8.3f scalar(Wb${m});
local tt$m : display %8.3f scalar(Wb${m})/sqrt(eV${m});

capture drop diff; gen diff= sm${m}_2-Wsm_2;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar twald${m}= bbb/sqrt(vvv); 
scalar twaldp${m}  = tprob(e(df_r),-abs(twald${m}));
/* update score card */ scorecardupdate; 
local twald${m} : display %8.3f scalar(twald${m});
local twaldp${m} : display %8.3f scalar(twaldp${m}); 
global m=$m +1;
dis $m;
};
global m=$m -1;

file write myfilepw "`i' & Obs. "/*=char(9)'`: variable label `i''"*/ ;
forvalues ii=1/$m {; 
file write myfilepw `" & `var`ii''  "'; }; 
/*file write myfilepw     "\\  &  $\Delta$ AIC "   ;
forvalues ii=1/$m {; 
file write myfilepw `" & `aic`ii''  "'; }; */
file write myfilepw     "\\  &  Vuong "   ;
forvalues ii=1/$m {; 
file write myfilepw `" & `twald`ii''  "'; }; 
file write myfilepw "\\ \hline "; };
file write myfilepw     " \end{tabular} \end{center} \end{table}  "   ;
file close myfilepw; };

#delimit;
forvalues ii=1/7 {;local dw2p`ii' : display %8.0f scalar(w2p`ii');
local dw1p`ii' : display %8.0f scalar(w1p`ii');
local dw1l`ii' : display %8.0f scalar(w1l`ii');
local dw2l`ii' : display %8.0f scalar(w2l`ii');
local dneutral`ii' : display %8.0f scalar(wneutral`ii');};

capture file close myfilescore;
file open myfilescore using "${store}/scorePWdemo.tex", write replace;
file write myfilescore " \begin{table}[!h]  \begin{center}
\caption{Scorecard: Democracies (Polity2 $\ge 6$)  \label{tab:scorePWdemo}} " _n  ;
file write myfilescore " \begin{tabular}{|c|c|c|c|c|c|c|c|}  \hline " _n ; 
file write myfilescore " Score &  \multicolumn{1}{|c|}{Support} &  \multicolumn{1}{|c|}{W (old)} & 
 \multicolumn{1}{|c|}{Polity2}  & \multicolumn{1}{|c|}{Boix} & \multicolumn{1}{|c|}{Przeworski} & \multicolumn{1}{|c|}{GWF} & \multicolumn{1}{|c|}{TL}
  \\  \hline   \hline  " _n;
  file write myfilescore " $scorelab2p &`dw2p1'  &`dw2p2' &`dw2p3'&`dw2p4' & `dw2p5'& `dw2p6' & `dw2p7'\\ " _n;
file write myfilescore " $scorelab1p  &`dw1p1' &`dw1p2' &`dw1p3'&`dw1p4' & `dw1p5' & `dw1p6'  & `dw1p7'  \\ " _n;
file write myfilescore " $scorelabneutral &`dneutral1'  &`dneutral2' &`dneutral3'&`dneutral4' & `dneutral5' & `dneutral6' & `dneutral7'  \\ " _n;
file write myfilescore " $scorelab1l &`dw1l1' &`dw1l2' &`dw1l3'&`dw1l4' & `dw1l5' & `dw1l6' & `dw1l7'  \\ " _n;
file write myfilescore " $scorelab2l &`dw2l1' &`dw2l2' &`dw2l3'&`dw2l4' & `dw2l5' & `dw2l6' & `dw2l7'  \\ " _n;
file write myfilescore "   \hline \end{tabular} \end{center} \end{table}  " ;
 file close myfilescore;
  
capture file close myfilescore;

/******************************************************************************/ 
/******************** Simple Table for the first DepVar. **********************/ 
/******************************************************************************/ 
#delimit; 
global NATT " e_migdppcln logpop c.yr c.yr#c.yr c.yr#c.yr#c.yr  i.ccode i.ccode#c.yr i.ccode#c.yr#c.yr  i.ccode#c.yr#c.yr#c.yr " ;
#delimit; 
global NATT " e_migdppcln logpop i.year   i.ccode" ;
capture gen inst=.; 
global depvar "PublicGoods"; 
label var inst "Institution Measure"; label var e_migdppcln "Ln(GDPpc)"; label var logpop "Ln(population)";
global model  "support W_old normpolity Dem6 e_boix_regime  " ;
/* Find the common sample */
reghdfe $depvar W4 $model e_migdppcln logpop , a(ccode year) resid vce(clu ccode)  ;
  capture drop sampletorun; 
gen sampletorun=e(sample); 
/* cycle through the insitutional measure to collect the Vong statistic */
replace inst=W4; capture drop resW4; capture drop res2W4; 
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ; 
predict resW4,res;gen res2W4=resW4^2;
 
replace inst=support; capture drop resSupport; capture drop res2Support; 
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ;

predict resSupport,res;gen res2Support=resSupport^2;
capture drop diff; gen diff= res2Support-res2W4;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar vSupport= bbb/sqrt(vvv); 
scalar vpSupport  = tprob(e(df_r),-abs(vSupport));

replace inst=W_old; capture drop resWold; capture drop res2Wold; 
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ;
predict resWold,res;gen res2Wold=resWold^2;
capture drop diff; gen diff= res2Wold-res2W4;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar vWold= bbb/sqrt(vvv); 
scalar vpWold  = tprob(e(df_r),-abs(vWold));



replace inst=normpolity;capture drop respol; capture drop res2pol;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ; 
predict respol,res;gen res2pol=respol^2;
capture drop diff; gen diff= res2pol-res2W4;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar vpol= bbb/sqrt(vvv); 
scalar vppol  = tprob(e(df_r),-abs(vpol));
replace inst=Dem6;capture drop resDem6; capture drop res2Dem6;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ; 
predict resDem6,res;gen res2Dem6=resDem6^2;
capture drop diff; gen diff= res2Dem6-res2W4;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar vDem6= bbb/sqrt(vvv); 
scalar vpDem6  = tprob(e(df_r),-abs(vDem6));
replace inst=e_boix_regime;capture drop resboix; capture drop res2boix;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ; 
predict resboix,res;gen res2boix=resboix^2;
capture drop diff; gen diff= res2boix- res2W4;
reg diff, cluster(ccode); mat bb=e(b); scalar bbb = bb[1,1]; 
mat vv=e(V) ; scalar vvv=vv[1,1];  scalar vboix= bbb/sqrt(vvv); 
scalar vpboix  = tprob(e(df_r),-abs(vboix));

/* Cycle through the institutional models and store estimates (add Vuong stats) */
#delimit; 
replace inst=W4; 
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ; 
eststo mW4;  estadd scalar fes=e(df_a_initial) ; estat ic;mat aic=r(S); scalar aid1=aic[1,5] ; estadd scalar Waic =0;
estadd scalar vv = 0; estadd scalar pvv = 0;

#delimit;
replace inst=support;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ;

eststo mSupport; estat ic;mat aic=r(S); estadd scalar Waic =aic[1,5]-aid1; 
estadd scalar vv = vSupport ;estadd scalar pvv =vpSupport ; 
estadd scalar fes=e(df_a_initial), replace;

replace inst=W_old;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ;
eststo mWold; estat ic;mat aic=r(S); estadd scalar Waic =aic[1,5]-aid1; 
estadd scalar vv = vWold ;estadd scalar pvv =vpWold ; estadd scalar fes=e(df_a_initial);
replace inst=normpolity;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ;  
eststo mpolity;estat ic;mat aic=r(S); estadd scalar Waic =aic[1,5]-aid1; 
estadd scalar vv =vpol ;estadd scalar pvv =vppol ; estadd scalar fes = e(df_a_initial);
replace inst=Dem6;
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ; 
eststo mdem6; estat ic; mat aic=r(S); estadd scalar Waic =aic[1,5]-aid1; 
estadd scalar vv =vDem6 ;estadd scalar pvv =vpDem6 ; estadd scalar fes=e(df_a_initial);
replace inst=e_boix_regime; 
reghdfe $depvar inst  e_migdppcln logpop if sampletorun==1 , a(ccode year) resid vce(clu ccode)  ;  
eststo mboix;estat ic;mat aic=r(S); estadd scalar Waic =aic[1,5]-aid1; 
estadd scalar vv =vboix ;estadd scalar pvv = vpboix  ; estadd scalar fes=e(df_a_initial);

/********** Create a Table comparing the measures for the first dependent Variable *****/

 #delimit;
global lab "inst "Institution Measure"    e_migdppcln "Ln(GDPpc)" logpop "Ln(population)" _cons "Constant" ";
global model  "support W_old normpolity Dem6 e_boix_regime  " ;
global n1 "${store}/BasicReg";	
esttab mW4 mSupport mWold mpolity mdem6 mboix  using "${n1}.tex",replace  ar2 r2 t 
   title("Public Goods Provisions and Institutional Indicators: Dependent Variable is Public Goods \label{tab:BasicReg}" )
   stats( N fes ll Waic vv pvv,
	labels("Observations" "FEs" "Loglike." "\$\Delta\$ AIC" "Vuong" "Pr(Vuong)")fmt(0 0 %12.3f %8.3f %8.3f %8.3f))  drop(  )
 varlabel($lab)  star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
 mlabels(   "W" "Support" "W (old)" "Polity2" "Dem6" "Boix" , nonumbers)   ; 

 
 
 #delimit;
/****** Summary Statstics ***********/ 
global var W4 support W_old normpolity Dem6 e_boix_regime  Przeworski  gwf_party gwf_military gwf_monarchy  gwf_personal gwf_demo v2x_ex_direlect v2x_ex_confidence v2x_ex_hereditary v2x_ex_military v2x_ex_party; 
capture  file close myfilesum;
file open myfilesum using "${store}/TableSummaryStats.tex", write replace;
file write myfilesum " \begin{table}[!h]  \begin{center} \caption{Summary Statistics: \label{tab:TableSummaryStats}}   \begin{tabular}{l*{5}{cc}} \hline\hline " _n;
file write myfilesum " Variable &       Observations & Mean & Median &Std. Dev.   & Years\\
\hline \hline  " _n ;
foreach i of global var {; qui sum `i' , detail;
local ob : display %8.0f scalar(r(N));
local mea : display %8.2f scalar(r(mean));
local med : display %8.2f scalar(r(p50));
local sd : display %8.2f scalar(r(sd));
qui sum year if `i'~=. ; 
local ymin : display %8.0f scalar(r(min));
local ymax : display %8.0f scalar(r(max));
file write myfilesum " `i' & `ob' & `mea' & `med' &`sd' & `ymin' - `ymax' \\ " _n;
};
 file write myfilesum " \end{tabular}  \end{center} \end{table} " ;
 file close myfilesum;
 
/********* Graph **********/
 histogram W4, bin(100) frequency xtitle(Coalition Size: W) name(Whistogram, replace);
   
graph export "${store}/Whistogram.pdf", replace;
/* finished: go get a beer! */ 
 
