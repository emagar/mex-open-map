use maxlik;library optmum,maxlik;__title="MBL Model";

ds="hc4a";  /* hr3 hr4 hr5,  hc1a hc2a hc3a hc4a */
est=1;        @ 0=rho; 1=rho and lambdas @
        /*  VARS:  year totvot seats
        ldpv ldps ldpd    dspv dsps dspd   suffix: v=votes
        nlcv nlcs nlcd    jcpv jcps jcpd           s=seats
        cgpv cgps cgpd    indv inds indd           d=seats if d'hondt
        jspv jsps jspd  HR ONLY:  sdlv sdls sdld */
  /* opt: 1=all parties; 3=oth is indv+sdlv; 5=ldp,jsp,oth */
  /*      2=with d'h     4=d'h               6=d'h        */
t=getname(ds);hc=0;
if in("sdls",t,0);  hc=1;    sdls=0;sdlv=0;sdld=0;     endif;

opt=1;do while opt<=8;

    if opt==1;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jsps = jsps+dsps;
            keep ldpv jspv nlcv cgpv jcpv indv @ sdlv @
                 ldps jsps nlcs cgps jcps inds @ sdls @;
        endata;
    elseif opt==2;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jspd = jspd+dspd;
            keep ldpv jspv nlcv cgpv jcpv indv @ sdlv @
                 ldpd jspd nlcd cgpd jcpd indd @ sdld @;
        endata;
    elseif opt==3;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jsps = jsps+dsps;
            make othV=indv+sdlv;
            make othS=inds+sdls;
            keep ldpv jspv nlcv cgpv jcpv othV
                 ldps jsps nlcs cgps jcps othS;
        endata;
    elseif opt==4;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jspd = jspd+dspd;
            make othV=indv+sdlv;
            make othD=indd+sdld;
            keep ldpv jspv nlcv cgpv jcpv othV
                 ldpd jspd nlcd cgpd jcpd othD  ;
        endata;
    elseif opt==5;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jsps = jsps+dsps;
            make othV=nlcv+cgpv+indv+sdlv;
            make othS=nlcs+cgps+inds+sdls;
            keep ldpv jspv jcpv othV
                 ldps jsps jcps othS;
        endata;
    elseif opt==6;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jspd = jspd+dspd;
            make othV=nlcv+cgpv+indv+sdlv;
            make othD=nlcd+cgpd+indd+sdld;
            keep ldpv jspv jcpv othV
                 ldpd jspd jcpd othD;
        endata;
    elseif opt==7;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jsps = jsps+dsps;
            make othV=nlcv+cgpv+jcpv+indv+sdlv;
            make othS=nlcs+cgps+jcps+inds+sdls;
            keep ldpv jspv othV
                 ldps jsps othS;
        endata;
    elseif opt==8;
        dataloop ^ds tmp;
            jspv = jspv+dspv;
            jspd = jspd+dspd;
            make othV=nlcv+cgpv+jcpv+indv+sdlv;
            make othD=nlcd+cgpd+jcpd+indd+sdld;
            keep ldpv jspv othV
                 ldpd jspd othD;
        endata;
    endif;

    if     est==1;  t=getname("tmp");
           _mlparnm="rho"|trimr(t,rows(t)/2+1,0);
           stval=1.5|zeros(rows(_mlparnm)-1,1);
    elseif est==0;  let _mlparnm=rho;
                    stval=1.5;
    endif;

    _mlcovp=3;
    t=ds$+".OUT";
    output file=^t on;
        ?;"opt=";;opt;?;
        {b,logl,g,vc,ret}=maxprt(maxlik("tmp",getname("tmp"),&loglik,stval));
    output off;

opt=opt+1;endo;


proc loglik(b,dta);
    clearg t,res,r,l,rlv,tv0,tv1,v,s;
    v=dta[.,1:cols(dta)/2];      @ v & s should be n x J @
    v=v./sumc(v');
    s=dta[.,(cols(dta)/2)+1:cols(dta)];
    if     est==1; r=b[1];           l=(0|trimr(b,1,0))';
    elseif est==0; r=b[1];           l=0;
    else; "est not implemented ";stop;endif;
    tv0=(v.>0);tv1=(v.==0);
    rlv=l+(r.*ln(v+tv1));
    t=ln(sumc( (tv0.*exp(rlv))' ));
    res=sumc( (tv0.*s.*(rlv-t))' );
    retp( res );endp;

