@ see BILOGIT.DOC for documentation @
nr=200;goto xxx;open f1=dummy varindxi;xxx:useivs=1;  @ do not change @
dataset="brit";  @ name Gauss dataset here @
est=1;           @ ESTIMATOR:0=(only rho);       1=(rho & lambdas);
                             2=(g0+g1*taag=rho); 3=(t0+t1*year=rho);@
incoth=0;        @ INCLUDE "OTHER PARTY" IN THE ANALYSIS:  0=no, 1=yes @
f1=openfile(dataset,useivs);closeall f1; @ do not change @

votes=iconsv| ilabv| ilibv;  @ # votes for each party @
otherv=iothv;       @ # votes for parties to be included in other category @

seats=iconss| ilabs| ilibs;  @ # seats for each party @
others=ioths;  @ # seats for parties to be included in other category @

ndist=itseats;        @ # of districts @
vyear=iyear;          @ year in four digits @


@-------------------------------------------------------------------------@
@-----------You probably don't need to change anything below here---------@
@-------------------------------------------------------------------------@
tmp=getname(dataset);
if     est==1;parnames="rho"|tmp[trimr(votes,1,0),.];
              if incoth==1;parnames=parnames|"otherv";endif;
elseif est==2;parnames="g0"|"g1"|tmp[trimr(votes,1,0),.];
              if incoth==1;parnames=parnames|"otherv";endif;
elseif est==3;parnames="t0"|"t1"|tmp[trimr(votes,1,0),.];
              if incoth==1;parnames=parnames|"otherv";endif;
elseif est==4;parnames="a0"|"a1"|tmp[trimr(votes,1,0),.];
elseif est==0;parnames="rho";else;"est not implemented ";stop;endif;

proc addindx(dta,a,b); @ ADD VAR'S INDXED by B TO VAR INDXED by A FOR DTA @
    local t,i,res,n;n=rows(b);    if rows(a)/=1;stop;endif;
    res=zeros(rows(dta),1);
    i=1;do while i<=n;
        t=b[i,1];res=res+dta[.,t];
    i=i+1;endo;
    dta[.,a]=res;
    retp(dta);endp;

proc 0=defvar(dta);clearg year,ts,n,v,s,oth,tv,mag,taag,dists,vdum;
    n=rows(dta);
    year=dta[.,vyear]-100*trunc(dta[.,vyear]/100); @ YEAR = 2 digits @
    v=dta[.,votes];   @ v & s should be n x J @
    if incoth==1;v=v~sumc( dta[.,otherv]' );endif;
    tv=sumc(v');
    v=v./tv;
    s=dta[.,seats];
    if incoth==1;s=s~sumc( dta[.,others]' );endif;
    ts=sumc(s');
    dists=dta[.,ndist];
    mag=ts./dists;
    taag=(ln(tv)./ln(ts))^(1./mag);
endp;
proc startval;local b;
    if     est==1;                          b=1.5|zeros(rows(parnames)-1,1);
    elseif (est==2) or (est==3) or (est==4);b=1|0|zeros(rows(parnames)-2,1);
    elseif est==0;                          b=1.5;
    else;"est not implemented";stop;endif;retp(b);endp;
proc LI(b);local t,res,r,l,rlv,tv0,tv1;
    if     est==1; r=b[1];           l=(0|trimr(b,1,0))';
    elseif est==2; r=b[1]+b[2]*taag; l=(0|trimr(b,2,0))';
    elseif est==3; r=b[1]+b[2]*year; l=(0|trimr(b,2,0))';
    elseif est==4; r=b[1]+b[2]*vdum; l=(0|trimr(b,2,0))';
    elseif est==0; r=b[1];           l=0;
    else; "est not implemented ";stop;endif;
    tv0=(v.>0);tv1=(v.==0);
    rlv=l+(r.*ln(v+tv1));
    t=ln(sumc( (tv0.*exp(rlv))' ));
    res=sumc( (tv0.*s.*(rlv-t))' );
    retp( res );endp;
title=" *** Multinomial Bilogit *** ";
let algrithm=BFGS;let initalg=BHHH;let finalalg=NR;varnames="";outdata="";
hessname=&HESSIAN;outputf=dataset$+".out";output file=^outputf off;
 strategy=1;       dstat=1; nfactor=5.5;  gradtol=1e-5;      dh=0; saveit=0;
   maxobs=0;    pcntsmpl=0;   prntime=1;     noprint=0;      m0=0;
 everyobs=0;    smplstvl=0;    prntit=0;    corrcomp=0;    iter=1;   typf=1;
     help=1;    stepflag=2; maxits=1e+5;    maxbkst=10; btol=1e-5;   typb=1;
gradname=&GRAD1;header=title;
b=maxlik(&startval,dataset,parnames,header,title,iter,m0); format 5,3;
output file=^outputf on;
  "Dependent variables (seats): ";;$ tmp[seats,1]';
  "Mean Votes: ";;meanc(v)';
  "Mean Seats: ";;meanc(s)';
  "effective parties:  ";;1./sumc(meanc(v)^2);
  "Lambda set to zero on votes variable: ";;$ tmp[votes[1,1],1];
  if incoth==1;"Parties included in OTHERV category: ";;$ tmp[otherv,1]';endif;
  "Total Seats (avg,min,max): ";;meanc(ts);;minc(ts);;maxc(ts);
  "Taagerpera's index (avg,min,max): ";;meanc(taag);;minc(taag);;maxc(taag);
output off;
