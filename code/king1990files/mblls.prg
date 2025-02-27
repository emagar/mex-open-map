goto xxx;open f1=dummy varindxi;xxx:#lineson;enable;trace 0;useivs=1;
nr=200;dataset="\\data\\mbl\\aus";
stoch=0;         @ 0=multinomial distribution; 1=Logit-Normal distribution @
est=4;           @ 0=(only rho); 1=(rho & lambdas); 2=(g0+g1*taag=rho);
                   3=(t0+t1*time=rho); 4=(a0+a1*vdum=rho)@
incoth=0;        @ include other category 0=no, 1=yes @
loadstrt=0;      @ 0=use zeros; 1=load from file b; @
covar=0;         @ 0=sigs; 1=sigs+cov; 2=only 1 sig @

if     stoch==0;title=" *** Multinomial Bilogit *** ";
elseif stoch==1;title=" *** Logit-Normal Multinomial Bilogit *** ";
else;  "stoch set at incorrect value";
endif;
f1=openfile(dataset,useivs);closeall f1;tmp=getname(dataset);clear vdum;

if dataset$=="\\data\\mbl\\brit";              @BRITAIN @
    votes=iconsv| ilabv;
          otherv=iothv| ilibv;
    seats=iconss| ilabs;
          others=ioths| ilibs;
    ndist=itseats;
    vyear=iyear;

elseif dataset$=="\\data\\mbl\\japan";              @JAPAN: (AND SEE DEFVAR)@
    votes=ilibdv| isocv| icomv;
          otherv=iothv| iclnv;
    seats=ilibds| isocs| icoms;
          others=ioths| iclns;
    ndist=idist;
    vyear=iyear;

elseif dataset$=="\\data\\mbl\\dut";            @DUT:@
   votes=ihcdav| ilabv| ilibpv| icommv;
        otherv=idem66v| ipfuv| iradv| irefpuv| ipsocv| idem70v| ifarmv| iothv;
   seats=ihcdas| ilabs| ilibps| icomms;
        others=idem66s| ipfus| irads| irefpus| ipsocs| idem70s| ifarms| ioths;
   ndist=idist;
   vyear=iyear;

elseif dataset$=="\\data\\mbl\\aus";            @AUS:@
   votes=isocv| ipeopv| icommv;
        otherv=iothv| ileagv;
   seats=isocs| ipeops| icomms;
        others=ioths| ileags;
   ndist=idist;
   vyear=iyear;
   dum=icontext;

elseif dataset$=="\\data\\mbl\\can";         @CANADA:@
   votes=iconsv| ilibv;
      otherv=ilabv| icomv| inppv| ireconv| ibpcv| irhinv| iothv| iscv| indpv;
   seats=iconss| ilibs;
      others=ilabs| icoms| inpps| irecons| ibpcs| irhins| ioths| iscs| indps;
   ndist=idist;
   vyear=iyear;

elseif dataset$=="\\data\\mbl\\us";             @US:@
    votes=idemv| irepv; otherv=iothv;
    seats=idems| ireps; others=ioths;
    ndist=idist;
    vyear=iyear;

elseif dataset$=="\\data\\mbl\\fin";            @FIN: (SEE DEFVAR TOO)@
     votes=isocdv| iagaruv| inatlcv;
        otherv=iothv| ilibpv| isppv| ifpduv| ifruralv| ichrtv| ismfv;
     seats=isocds| iagarus| inatlcs;
        others=ioths| ilibps| ispps| ifpdus| ifrurals| ichrts| ismfs;
     ndist=idist;
     vyear=iyear;

elseif dataset$=="\\data\\mbl\\swi";            @SWI:@
    votes=isocdv| iraddv| icathcv| ifarmv;
          otherv=idemv| iprotpv| icommv| iindpv| iothv| ilibcv;
    seats=isocds| iradds| icathcs| ifarms;
          others=idems| iprotps| icomms| iindps| ioths| ilibcs;
    ndist=idist;
    vyear=iyear;

elseif dataset$=="\\data\\mbl\\ita";            @ITA: (SEE DEFVAR TOO)@
    votes=ichristv| icommv| isocv| irepv;
          otherv=imchv| iothv| imsiv| istyrolv| irepv| isocdv| ilibv;
    seats=ichrists| icomms| isocs| ireps;
          others=imchs| ioths| imsis| istyrols| ireps| isocds| ilibs;
    ndist=idist;
    vyear=iyear;

else;"dataset wrong, dataset= ";;$dataset;stop;endif;

if     est==1;parnames="rho"|tmp[trimr(votes,1,0),.];
              if incoth==1;parnames=parnames|"otherv";endif;
elseif est==2;parnames="g0"|"g1"|tmp[trimr(votes,1,0),.];
              if incoth==1;parnames=parnames|"otherv";endif;
elseif est==3;parnames="t0"|"t1"|tmp[trimr(votes,1,0),.];
              if incoth==1;parnames=parnames|"otherv";endif;
elseif est==4;parnames="a0"|"a1"|tmp[trimr(votes,1,0),.];
elseif est==0;parnames="rho";else;"est not implemented ";stop;
endif;

if     incoth==0;  ptys=rows(votes);
elseif incoth==1;  ptys=rows(votes)+1;
endif;

if stoch==1;
    if covar==0;
        parnames=parnames|(tmp[trimr(seats,1,0),.]$+"sig");
        if incoth==1;  parnames=parnames|"othsig"; endif;
    elseif covar==1;
        parnames=parnames|(tmp[trimr(seats,1,0),.]$+"sig");
        if incoth==1;  parnames=parnames|"othsig"; endif;
        parnames=parnames|"cov";
    elseif covar==2;
        parnames=parnames|"sig";
    else; "covar error";stop;
    endif;
endif;

proc addindx(dta,a,b); @ ADD VAR'S INDXED by B TO VAR INDXED by A FOR DTA @
    local t,i,res,n;n=rows(b);    if rows(a)/=1;stop;endif;
    res=zeros(rows(dta),1);
    i=1;do while i<=n;
        t=b[i,1];res=res+dta[.,t];
    i=i+1;endo;
    dta[.,a]=res;
    retp(dta);endp;

proc logit(v);retp(ln(v./(1-v)));endp;

proc 0=defvar(dta);clearg year,ts,n,v,s,oth,tv,mag,taag,dists,zs;
    n=rows(dta);
    year=dta[.,vyear]-100*trunc(dta[.,vyear]/100); @ YEAR = 2 digits @

    if dataset$=="\\data\\mbl\\fin";                    @FIN:@
        dta=addindx(dta,ifpduv,ifpduv|isocwv);
        dta=addindx(dta,ifpdus,ifpdus|isocws);

    elseif dataset$=="\\data\\mbl\\ita";                @ITA:@
        dta[.,icommv]=dta[.,icommv]+0.5*dta[.,icsocv];
        dta[.,icomms]=dta[.,icomms]+floor(0.5*dta[.,icsocs]);
        dta[.,isocv]=dta[.,isocv]+0.5*dta[.,icsocv]+0.5*dta[.,iusocv];
        dta[.,isocs]=dta[.,isocs]+ceil(0.5*dta[.,icsocs])+
                     floor(0.5*dta[.,iusocs]);
        dta[.,isocdv]=dta[.,isocdv]+0.5*dta[.,iusocv];
        dta[.,isocds]=dta[.,isocds]+ceil(0.5*dta[.,iusocs]);

    elseif dataset$=="\\data\\mbl\\japan";              @JAPAN:@
        dta=delif(dta,(year.<58));
        year=delif(year,(year.<58));
        dta=addindx(dta,ilibdv, ilibdv| iprogv| idemv| ihatv|
            ilibv| iylv| ipcpv| inlibv);
        dta=addindx(dta,isocv,isocv| ilwsv| irwsv| ilfv| isrpv| idemsv);

    elseif dataset$=="\\data\\mbl\\aus";
        vdum=dta[.,dum];

    else;" no recoding of dataset ";;$dataset;endif;

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
    if stoch==1;
        s=s./ts;
        s=subsitut(s,s.==0,0.001);
@        v=subsitut(v,v.==0,0.001);     @
    endif;
    n=rows(s);
    endp;
proc startval;local b,bb;
    if     est==1;                          b=1|zeros(ptys-1,1);
    elseif (est==2) or (est==3) or (est==4);b=1|0|zeros(ptys-1,1);
    elseif est==0;                          b=1.5;
    else;"est not implemented";stop;
    endif;
    if stoch==1;
        if     covar==0;        b=b|(0.004*ones(ptys-1,1));
        elseif covar==1;        b=b|(-2*ones(ptys-1,1));
            if ptys>2; b=b|1; endif;
        elseif covar==2;        b=b|-3;
        endif;
    endif;
    if loadstrt==1;loadm bb=b;
        b[1:rows(bb)]=bb;
    endif;
    retp(b);endp;
proc LI(b);clearg t,res,r,l,rlv,tv0,tv1,yu,tt,sigi,trm,sig,penalty;
    if     stoch==0; trm=0;
    elseif stoch==1;
        if     covar==0; trm=ptys-1;
        elseif covar==1; trm=ptys-1;   if ptys>2; trm=trm+1; endif;
        elseif covar==2; trm=1;
        endif;
    endif;
    if     est==1; r=b[1];           l=(0|trimr(b,1,trm))';
    elseif est==2; r=b[1]+b[2]*taag; l=(0|trimr(b,2,trm))';
    elseif est==3; r=b[1]+b[2]*year; l=(0|trimr(b,2,trm))';
    elseif est==4; r=b[1]+b[2]*vdum; l=(0|trimr(b,2,trm))';
    elseif est==0; r=b[1];           l=0;
    else; "est not implemented ";stop;
    endif;
    tv0=(v.>0);tv1=(v.==0);
    if stoch==0;
        rlv=l+(r.*ln(v+tv1));
        t=ln(sumc( (tv0.*exp(rlv))' ));
        res=sumc( (tv0.*s.*(rlv-t))' );
    elseif stoch==1;
        if ptys==2;
            sig=b[rows(b)]^2;
        elseif ptys>2;
            if     covar==0;    sig=eye(ptys-1);
                   tt=rev(b);tt=tt[1:ptys-1];tt=0.0002+exp(rev(tt));
                   sig=diagrv(sig,tt);
            elseif covar==1;    sig=ones(ptys-1,ptys-1)*b[rows(b)];
                   tt=rev(b);tt=tt[2:ptys];tt=0.0002+exp(rev(tt));
                   sig=diagrv(sig,tt);
            elseif covar==2;    sig=0.0002+exp(b[rows(b)]);
            endif;
        endif;
        rlv=tv0.*exp(l+(r.*ln(v+tv1)));
        yu=logit(s[.,2:ptys])-logit(rlv[.,2:ptys]./sumc(rlv'));
        sigi=inv(sig);
        tt=ones(n,1);
        i=1;do while i<=n;
            tt[i]=yu[i,.]*sigi*(yu[i,.]');
        i=i+1;endo;
        res=-0.5*(ln(detl)+tt);
    else;"stoch not implemented";
    endif;
    retp( res-penalty);endp;
@-------------------------------------------------------------------------@
@-----------You probably don't need to change anything below here---------@
@-------------------------------------------------------------------------@
let algrithm=Bfgs;let initalg=bfgs;let finalalg=BHHH;varnames="";outdata="";
hessname=&HESSIAN;outputf=dataset$+".out";output file=^outputf off;
 strategy=1;       dstat=1; nfactor=5.5;  gradtol=1e-5;      dh=0; saveit=0;
   maxobs=0;    pcntsmpl=0;   prntime=1;     noprint=0;      m0=0;
 everyobs=0;    smplstvl=0;    prntit=1;    corrcomp=0;    iter=1;   typf=1;
     help=1;    stepflag=2; maxits=1e+5;    maxbkst=10; btol=1e-5;   typb=1;
gradname=&GRAD1;header=title;
b=maxlik(&startval,dataset,parnames,header,title,iter,m0);
output file=^outputf on;
  "Dependent variables (seats): ";;$ tmp[seats,1]';
  "Mean Votes: ";;meanc(v)';
  "Mean Seats: ";;meanc(s)';
  "effective parties:  ";;1./sumc(meanc(v)^2);
  "Lambda set to zero on votes variable: ";;$ tmp[votes[1,1],1];
  if incoth==1;"Parties included in OTHER category: ";;$ tmp[otherv,1]';endif;
  "Total Seats (avg,min,max): ";;meanc(ts);;minc(ts);;maxc(ts);
  "Taagerpera's index (avg,min,max): ";;meanc(taag);;minc(taag);;maxc(taag);
output off;
save b;
