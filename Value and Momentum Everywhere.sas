libname VM 'C:\Users\30970\Desktop\New'; run;
libname Test 'C:\Users\30970\Desktop\New\documents-export-2016-07-05\ModData'; run;

/* libname AMSep29 'D:\Data\Projects\Stocks - ReturnPredictability\SASData'; run; */




%let wrds=wrds.wharton.upenn.edu 4016; 
options comamid=TCP remote=WRDS;        
signon username=_prompt_;               
                                         
libname rwork slibref = work server = wrds; run;

rsubmit;
options nocenter nodate nonumber ls=max ps=max msglevel=i; 
/* libname mf '/wrds/crsp/sasdata/q_mutualfunds'; run; *refers to crsp;
libname crspa '/wrds/crsp/sasdata/a_stock'; run; *refers to crsp;
libname ff '/wrds/ff/sasdata'; run;
libname s12 '/wrds/tfn/sasdata/s12'; run; *refers to tfn;
libname mfl '/wrds/mfl/sasdata'; run;
libname a_ccm '/wrds/crsp/sasdata/a_ccm'; run; *refers to crsp;
libname naa '/wrds/comp/sasdata/naa'; run; *refers to compa;
libname nk '/sastemp7/NK'; run; */

*Specify CRSP begin and end dates;
%let crspbegdate = '01Jan1971'd;
%let crspenddate = '31Dec2013'd; 
endrsubmit;









/*******************************************/
/*******************************************/
/*******************************************/
/************ CRSP STOCK SAMPLE ************/
/*******************************************/
/*******************************************/
/*******************************************/



/******************************************/
/* Step 1. Obtain BM ratio for all stocks */
/******************************************/
rsubmit;
/* Step 1.1. Obtain Book-Equity from Compustat */
data comp_extract;
   format gvkey datadate date_fyend calyear fyear fyr indfmt consol datafmt popsrc 
          TXDITC BE0 BE1 BE_FF EPSPX EBITDA SALE DVC CSHO ADJEX_C;  /*http://www.crsp.com/products/documentation/annual-data-industrial */
   set comp.funda
   (where=(fyr>0 and at>0 and consol='C' and
           indfmt='INDL' and datafmt='STD' and popsrc='D'));

   **BE as in Daniel and Titman (JF, 2006); 
   *Shareholder equity(she);
   if missing(SEQ)=0 then she=SEQ; 
   *SEQ=Stockholders Equity (Total);
   else if missing(CEQ)=0 and missing(PSTK)=0 then she=CEQ+PSTK; 
        *CEQ=Common/Ordinary Equity (Total), PSTK=Preferred/Preference Stock (Capital) (Total);
        else if missing(AT)=0 and missing(LT)=0 then she=AT-LT;
		     *AT=Assets (Total), LT=Liabilities (Total), MIB=Minority Interest (Balance Sheet); 
             else she=.;

   if missing(PSTKRV)=0 then BE0=she-PSTKRV; *PSTKRV=Preferred Stock (Redemption Value);
   else if missing(PSTKL)=0 then BE0=she-PSTKL; *PSTKL=Preferred Stock (Liquidating Value);
        else if missing(PSTK)=0 then BE0=she-PSTK; *PSTK=Preferred/Preference Stock (Capital) (Total);
             else BE0=.;
  

   **BE as in Kayhan and Titman (2003);
   *Book_Equity = Total Assets - [Total Liabilities + Preferred Stock] + Peferred Taxes + Conv. Debt;  
   if AT>0 and LT>0 then BE1=AT-(LT+PSTKL)+TXDITC+DCVT;
   else BE1=.;


   **BE as in Fama and French (JF, 2008); 
   if missing(AT)=0 and missing(LT)=0 and missing(TXDITC)=0 then BE_FF_Temp = (AT-LT) + TXDITC; *AT=Assets (Total), LT=Liabilities (Total);
   else if missing(AT)=0 and missing(LT)=0 and missing(TXDITC)=1 then BE_FF_Temp = (AT-LT);  *TXDITC=Deferred Taxes and Investment Tax Credit;
        else BE_FF_Temp = .;
   if missing(PSTKL)=0 then BE_FF = BE_FF_Temp - PSTKL; *PSTKL=Preferred Stock (Liquidating Value);
   else if missing(PSTKRV)=0 then BE_FF = BE_FF_Temp - PSTKRV; *PSTKRV=Preferred Stock (Redemption Value);
        else if missing(PSTK)=0 then BE_FF = BE_FF_Temp - PSTK; *PSTK=Preferred/Preference Stock (Capital) (Total);
		     else BE_FF = .;
   drop BE_FF_Temp;

   *Converts fiscal year into calendar year data;
   if (1<=fyr<=5) then date_fyend=intnx('month',mdy(fyr,1,fyear+1),0,'end');
   else if (6<=fyr<=12) then date_fyend=intnx('month',mdy(fyr,1,fyear),0,'end');
   calyear=year(date_fyend);
   format date_fyend date9.;

   *Accounting data since calendar year 't-1';
   if year(&crspbegdate)-1<=year(date_fyend)<=year(&crspenddate)+1;
   keep gvkey datadate date_fyend calyear fyear fyr indfmt consol datafmt popsrc 
        TXDITC BE0 BE1 BE_FF EPSPX EBITDA SALE DVC CSHO ADJEX_C;
   *Note that datadate is same as date_fyend;
run;


/* Step 1.2 Calculate BE as in Daniel and Titman (JF, 2006) */
proc sql; /*http://www.listendata.com/2014/04/proc-sql-select-statement.html */
  create table comp_extract_be as 
  select distinct a.gvkey, a.datadate format=date9., a.fyr, a.fyear, a.date_fyend, a.calyear, 
         case 
            when missing(TXDITC)=0 and missing(PRBA)=0 then BE0+TXDITC-PRBA 
            else BE0
         end as BE, BE1, BE_FF
  from comp_extract as a left join comp.aco_pnfnda (keep=gvkey indfmt consol datafmt popsrc datadate prba) as b
  on a.gvkey=b.gvkey and a.indfmt=b.indfmt and a.consol=b.consol and a.datafmt=b.datafmt and 
     a.popsrc=b.popsrc and a.datadate=b.datadate;
quit;
endrsubmit;

/* Step 1.3 Manually download CRSP-COMPUSTAT LINKS from web query and upload it*/
data CCMLINKS;
  set VM.DATA;
  if missing(lpermno)=1 then delete;
  keep gvkey lpermno linkdt;
  /* datadate=input(linkdt,YYMMDD8.);
format linkdt date10.; */
run;
proc sort data=CCMLINKS nodupkey; by gvkey; run;
rsubmit;
proc upload data=CCMLINKS out=CCMLINKS; run;
proc sql;
  create table BE as
  select distinct a.*, b.lpermno
  from comp_extract_be as a, CCMLINKS as b
  where a.gvkey=b.gvkey;
quit;

*Date on which BE is available to the investors;
data BE;
  format gvkey lpermno datadate investdate BE BE_FF;
  set BE;
  investdate=intnx("month",datadate,6,"E");
  format investdate date9.;
  if BE_FF<0 then delete; *As in FF2008;
  keep gvkey lpermno datadate investdate BE BE_FF;
run;
proc download data=BE out=BE_AMP; run;
endrsubmit;
data Work.BE_AMP; 
  set BE_AMP; 
run;
data VM.BE;
	set Work.BE_AMP;  /*search for BE in VM lib */
run;


/*****************
Sending SAS data to R
*****************/

C:\Program Files\SASHome\SASFoundation\9.4\sas.exe -RLANG


proc options option=rlang;
run;


-RLANG
-config "C:\Program Files\SASHome\SASFoundation\9.4\nls\en\sasv9.cfg"




options set=R_HOME='C:\Program Files\R\R-3.3.1';


proc iml;
  call ExportDataSetToR("Sashelp.Class", "BE" );
  submit / R;
    names(BE)
  endsubmit;
run;

-SET sasext0 "C:\Program Files\SASHome\SASFoundation\9.4"
-SET sasroot "C:\Program Files\SASHome\SASFoundation\9.4"
-SET sasext1 "C:\Program Files\SASHome\SASFoundation\9.4\nls"

/*****************
*****************/

/************************************/
/* Step 2. Extract US common stocks */
/************************************/


/* Step 2.1 Run web query and download the data in Test */
/* Step 2.2 Keep only common stocks and obtan Size */
data MonthlyStockData;
  set VM.MonthlyStockRET_FF2008;
   if SHRCD in (10,11); *SELECT ONLY COMMON STOCKS;
   Date=intnx("month",date,0,"E");
   format Date date9.;
   if missing(PRC)=0 and missing(SHROUT)=0 and SHROUT>0 then ME=(abs(PRC)*SHROUT*1000)/1000000; *SHROUT SHOULD BE GREATER THAN 0 TO MAKE SENSE;   
   label ME='Market Cap ($M)';
   if missing(ME)=1 then delete;
   keep permno Date PRC RET ME;
run;
proc sort data=MonthlyStockData nodupkey; by permno date; run;
data MonthlyStockData;
  set MonthlyStockData;
  by permno date;
  LagME=lag(ME);
  if first.permno=1 then LagME=.;
run;


***MOM;
data MonthlyStockData;
  format permno past1yrdate Date;
  set MonthlyStockData;
  past1yrdate=intnx("month",Date,-12,"E"); *t-12 month end date at the start of date;
  format past1yrdate date9.;
run;
*Valid past 12-month return data;
proc sql;
  create table MonthlyStockData as
  select distinct a.*
  from MonthlyStockData as a, MonthlyStockData as b
  where a.permno=b.permno and a.past1yrDate<b.Date<=a.Date and missing(b.RET)=0
  group by a.permno, a.Date
  having count(distinct b.Date)=12; *Require good return for all 12 months;
quit;
proc sql;
  create table MonthlyStockData1 as
  select distinct a.*, exp(sum(log(1+b.RET))) - 1 as MOM
  from MonthlyStockData as a, MonthlyStockData as b
  where a.permno=b.permno and a.past1yrDate<b.Date<=intnx("month",a.Date,-1,"E") and missing(b.RET)=0
  group by a.permno, a.Date
  having count(distinct b.Date)=11; *Require good return for all 11 months;
quit;

***Value: Valid BE available to the investor;
proc sql;
  create table MonthlyStockData2 as
  select distinct a.*, b.datadate, b.investDate, b.BE_FF as BE, intck('month',b.investDate,a.Date) as diffmonth
  from MonthlyStockData1 as a, Work.BE_AMP as b
  where a.permno=b.lpermno and b.investDate<=a.Date
  group by a.permno, a.Date
  having a.Date-b.investDate=min(a.Date-b.investDate);
quit;
data MonthlyStockData2;
  format permno past1yrDate Date PRC RET BE ME LagME Value MOM;
  set MonthlyStockData2;
  if diffmonth<=11;
  drop investDate diffmonth past1yrdate;
  Value = BE/ME;
run;



***Screen: At the end of every month, select stocks with cum market-cap of 90%;
proc sort data=MonthlyStockData2; by Date descending ME; run; 
data MonthlyStockData2;
  set MonthlyStockData2;
  by Date;
  Retain SumME;
  SumME = sum(SumME, ME);
  if first.Date=1 then SumME=ME;
run;
proc sql;
  create table MonthlyStockData2 as
  select distinct *, max(SumME) as MaxSumME
  from MonthlyStockData2
  group by Date
  order by Date, ME desc;
quit;
data MonthlyStockData2;
  set MonthlyStockData2;
  CumME = SumME/MaxSumME;
  format CumME 6.3;
run;
data MonthlyStockData2;
  set MonthlyStockData2;
  if CumME<=0.90;
  drop SumME MaxSumME;
  if "31Jan1972"d<=date<="30Jun2012"d;
run;
*381,673 OBS; 
data VM.MonthlyStockData2;
  set MonthlyStockData2;
run;


***Rearrange the variables;
proc sql;
  create table Value as
  select distinct permno, date, PRC, RET, BE, ME, LagME, 'Value' as AnomalyVar, Value as Anomaly
  from MonthlyStockData2;

  create table MOM as
  select distinct permno, date, PRC, RET, BE, ME, LagME, 'MOM' as AnomalyVar, MOM as Anomaly
  from MonthlyStockData2;
quit;
data VM.Value;
	set Value;
run;
data VM.Mom;
	set Mom;
run;


*SUE from VM lib;  /*Merge database */
proc sql;
  create table SUE as
  select distinct a.permno, a.date, b.rdq, a.PRC, a.BE, a.ME, a.LagME, 'SUE' as AnomalyVar, b.SUE1 as Anomaly
  from MOM as a left join Test.Comp_SUE as b
  on a.permno=b.lpermno and b.rdq<=a.date
  group by a.permno, a.date
  having a.date-b.rdq=min(a.date-b.rdq);
quit;
*Avoid stale data. Assign missing values for old SUE older than 4 months;
data SUE;
  set SUE;
  if ((date - rdq) > 120) then delete;
  if missing(Anomaly)=1 then delete;
  drop rdq;
run;
*308,064 OBS;

***Combine anomalies in one dataset;
data MonthlyStockData3;
  set Value MOM SUE;
run;
proc sql;
  drop table Value, MOM, SUE;
quit;




/*******************************/
/*** Step 4. Form Portfolios ***/
/*******************************/

***Tercile groups;
proc sort data=MonthlyStockData3; by date AnomalyVar; run;
data VM.MonthlyStockData3; set MonthlyStockData3; run;
proc rank data=MonthlyStockData3 groups=3 out=MonthlyStockData3;
  by date AnomalyVar;
  var Anomaly;
  ranks Rank_Anomaly;
run;
data MonthlyStockData3;
  set MonthlyStockData3;
  Rank_Anomaly=Rank_Anomaly+1;
run;

***Cross-sectional percentile ranks;
proc rank data=MonthlyStockData3 fraction out=MonthlyStockData3;
  by date AnomalyVar;
  var Anomaly;
  ranks PctlRank_Anomaly;
run;
proc sort data=MonthlyStockData3; by date AnomalyVar Anomaly; run;

data VM.MonthlyStockData3_new; set MonthlyStockData3; run;
***Signal weights;
proc sql;
  create table MonthlyStockData3 as
  select distinct *, PctlRank_Anomaly-mean(PctlRank_Anomaly) as wt
  from MonthlyStockData3
  group by date, AnomalyVar
  order by date, AnomalyVar, Anomaly;
quit;
data MonthlyStockData3;
  set MonthlyStockData3;
  if missing(wt)=0 and wt<0 then long=0;
  if missing(wt)=0 and wt>0 then long=1;
run;
proc sql;
  create table MonthlyStockData3 as
  select distinct *, wt/sum(wt) as signalwt
  from MonthlyStockData3
  group by date, AnomalyVar, long
  order by date, AnomalyVar, long, Anomaly;
quit;
data MonthlyStockData3;
  set MonthlyStockData3;
  if long=0 then signalwt=-1*signalwt;
  drop wt;
run;
data VM.MonthlyStockData3_new2; set MonthlyStockData3; run;



***Holding period - next one month;
data MonthlyStockData3;
  set MonthlyStockData3;
  Date1=intnx("month",Date,1,"E");
  format Date1 date9.;
run;


***Holding period stock return;
proc sql;
  create table MonthlyStockData4 as
  select distinct a.*, b.Ret as HoldRet label='Holding Period Ret'
  from MonthlyStockData3 as a, MonthlyStockData as b
  where a.permno=b.permno and a.Date1=b.Date and missing(b.Ret)=0
  order by a.Date, a.AnomalyVar, a.long, a.Anomaly;
quit;



***Obtain Portfolio Returns;
proc sql;
  create table MonthlyStockData4_Port as
  select distinct Date1, AnomalyVar, Rank_Anomaly, sum(ME*HoldRet)/sum(ME) as VWHoldRet
  from MonthlyStockData4
  group by Date1, AnomalyVar, Rank_Anomaly
  order by Date1, AnomalyVar, Rank_Anomaly;
quit;
data VM.MonthlyStockData4_Port;set MonthlyStockData4_Port; run;
proc sql;
  create table HedgePort as
  select distinct a.Date1, a.AnomalyVar, 4 as Rank_Anomaly, a.VWHoldRet-b.VWHoldRet as VWHoldRet
  from MonthlyStockData4_Port(where=(Rank_Anomaly=3)) as a, MonthlyStockData4_Port(where=(Rank_Anomaly=1)) as b
  where a.Date1=b.Date1 and a.AnomalyVar=b.AnomalyVar;        
quit;
proc sql;
  create table FactorPort as
  select distinct Date1, AnomalyVar, 5 as Rank_Anomaly, sum(signalwt*HoldRet) as VWHoldRet
  from MonthlyStockData4
  group by Date1, AnomalyVar
  order by Date1, AnomalyVar;
quit;

*Consolidate portret in one dataset;
data MonthlyStockData4_Port;
  set MonthlyStockData4_Port HedgePort FactorPort;
run;
proc sort data=MonthlyStockData4_Port; by Date1 AnomalyVar Rank_Anomaly; run;
data VM.MonthlyStockData4_Port_new;set MonthlyStockData4_Port;run;

***Mom+Value returns;
proc sql;
  create table MomValueCombo_Hedge as
  select distinct Date1, 'ValueMom' as AnomalyVar, Rank_Anomaly, mean(VWHoldRet) as VWHoldRet
  from MonthlyStockData4_Port
  where AnomalyVar in ('Value','MOM') and Rank_Anomaly=4
  group by Date1;

  create table MomValueCombo_Factor as
  select distinct Date1, 'ValueMom' as AnomalyVar, Rank_Anomaly, mean(VWHoldRet) as VWHoldRet
  from MonthlyStockData4_Port
  where AnomalyVar in ('Value','MOM') and Rank_Anomaly=5
  group by Date1;
quit;
data MonthlyStockData4_Port;
  set MomValueCombo_Hedge MomValueCombo_Factor MonthlyStockData4_Port;
run;
data VM.MonthlyStockData4_Port_new2; set MonthlyStockData4_Port; run;


**Correlation between Value and Mom;
proc sql;
  create table ValueMom as
  select distinct a.Date1, a.Rank_Anomaly, a.VWHoldRet as Value_VWHoldRet, b.VWHoldRet as MOM_VWHoldRet
  from MonthlyStockData4_Port(where=(AnomalyVar='Value')) as a, MonthlyStockData4_Port(where=(AnomalyVar='MOM')) as b
  where a.Date1=b.Date1 and a.Rank_Anomaly=b.Rank_Anomaly and a.Rank_Anomaly in (4,5);
quit;
proc sort data=ValueMom; by Rank_Anomaly; run;
data VM.ValueMom; set ValueMom; run;
proc corr data=ValueMom noprint outp=CorrValMom;
  by Rank_Anomaly;
  var Value_VWHoldRet MOM_VWHoldRet;
run;
data Test.CorrValMom; set CorrValMom; run;

**Download RF, VW market;  *Downloaded from fama-french database
**Include in MonthlyStockData4;
proc sql;
  create table MonthlyStockData5_Port1 as
  select distinct a.*, b.RF, a.VWHoldRet-b.RF as ExVWHoldRet, b.MKTRF+b.RF as MKT, b.MKTRF
  from MonthlyStockData4_Port as a, Test.FFMonthlyFactors as b
  where a.Date1=intnx("month",b.dateff,0,"E") and a.Rank_Anomaly in (1,2,3);

  create table MonthlyStockData5_Port2 as
  select distinct a.*, b.RF, a.VWHoldRet as ExVWHoldRet, b.MKTRF+b.RF as MKT, b.MKTRF
  from MonthlyStockData4_Port as a, Test.FFMonthlyFactors as b
  where a.Date1=intnx("month",b.dateff,0,"E") and a.Rank_Anomaly in (4,5);
quit;
data MonthlyStockData5_Port;
  set MonthlyStockData5_Port1 MonthlyStockData5_Port2;
run;
data VM.MonthlyStockData5_Port; set MonthlyStockData5_Port; run;
proc sql;
  drop table MonthlyStockData5_Port1, MonthlyStockData5_Port2;
quit;





/*******************************/
/*** Step 5. Test Portfolios ***/
/*******************************/


*MOM and Value: Date1 start = 29Feb1972, Date1 end = 31Jul2012;
*SUE: Date1 start = 31Oct1974, Date1 end = 31Jul2012;

/*** Full Sample Results ***/
***Mean, STD, T-stat;
proc sort data=MonthlyStockData5_Port; by AnomalyVar Rank_Anomaly; run;
proc means data=MonthlyStockData5_Port noprint;
  by AnomalyVar Rank_Anomaly;
  var VWHoldRet ExVWHoldRet;
  output out=MeanRet mean=VWHoldRet ExVWHoldRet;
quit;
proc means data=MonthlyStockData5_Port noprint;
  by AnomalyVar Rank_Anomaly;
  var VWHoldRet ExVWHoldRet;
  output out=TstatRet t=VWHoldRet ExVWHoldRet;
quit;
proc means data=MonthlyStockData5_Port noprint;
  by AnomalyVar Rank_Anomaly;
  var VWHoldRet ExVWHoldRet;
  output out=STDRet std=VWHoldRet ExVWHoldRet;
quit;
data MeanRet;
  set MeanRet;
  drop _Type_ _Freq_;
  Var=1;
  Stat='Mean';
  VWHoldRet=VWHoldRet*12*100;
  ExVWHoldRet=ExVWHoldRet*12*100;
run;
data TstatRet;
  set TstatRet;
  drop _Type_ _Freq_;
  Var=2;
  Stat='T-Stat';
run;
data STDRet;
  set STDRet;
  drop _Type_ _Freq_;
  Var=3;
  Stat='STD';
  VWHoldRet=VWHoldRet*sqrt(12)*100;
  ExVWHoldRet=ExVWHoldRet*sqrt(12)*100;
run;


***Sharpe Ratio;
proc sql;
  create table SharpeRatio as
  select distinct 4 as Var, AnomalyVar, Rank_Anomaly, 'Sharpe' as Stat, (mean(VWHoldRet)/std(VWHoldRet))*sqrt(12) as VWHoldRet 
  from MonthlyStockData5_Port
  group by AnomalyVar, Rank_Anomaly;
quit;


***Alpha;
proc sort data=MonthlyStockData5_Port; by AnomalyVar Rank_Anomaly; run;
proc reg data=MonthlyStockData5_Port noprint tableout outest=Alpha;
  by AnomalyVar Rank_Anomaly;
  model ExVWHoldRet = MKTRF;
quit;
data Alpha;
  set Alpha;
  where _Type_ in ('PARMS','T');
  keep AnomalyVar Rank_Anomaly _Type_ Intercept;
  if _Type_='PARMS' then Intercept=Intercept*12*100;
  rename Intercept=VWHoldRet;
  rename _Type_ =Stat;
run;
data Alpha;
  set Alpha;
  if Stat='PARMS' then Var=5;
  if Stat='T' then Var=6;
run;


***Collect all results;
data TestStats;
  length Stat $ 10;
  set MeanRet TstatRet STDRet SharpeRatio Alpha;
run;
data VM.TestStats; set TestStats; run;
data TestStats;
  format Var AnomalyVar Rank_Anomaly Stat;
  set TestStats;
  drop ExVWHoldRet;
  if Stat='PARMS' then Stat='Alpha';
  if Stat='T' then Stat='T-Alpha';
run;
proc sort data=TestStats; by AnomalyVar Var Stat; run;
data VM.TestStats2; set TestStats; run;
proc transpose data=TestStats out=TestStats;
  by AnomalyVar Var Stat;
  var VWHoldRet;
  id Rank_Anomaly;
run;
data TestStats;
  set TestStats;
  drop _Name_ _Label_;
  rename _1 = P1;
  rename _2 = P2;
  rename _3 = P3;
  rename _4 = P3_P1;
  rename _5 = Factor;
  format _1 6.2 _2 6.2 _3 6.2 _4 6.2 _5 6.2;
run;
data Test.AMP_SUE_TestStats_FS_SRNORF;
  set TestStats;
run;

proc export data=Test.Results outfile="C:\Users\30970\Desktop\New\documents-export-2016-07-05\ModData\Results.csv" 
dbms=csv replace;
run;


