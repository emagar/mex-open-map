@  DENSITY ESTIMATION  @
#lineson;disable; @ z=input value, m=mean, h=smoothing value @
proc kernaln(z,m,h);local res;z=(z-m)./h;      @ NORMAL KERNAL @
    res=(1./sqrt(2*pi))*exp(-(1/2).*(z^2));retp(res);endp;
proc kernale(z,m,h);local a,res,t;z=(z-m)./h;  @ EPANECHNIKOV KERNAL @
    t=(abs(z).<sqrt(5)); a=code(t,sqrt(5)|1);
    res=t.*((3/4)*(1-(1/5).*(z^2))./a);retp(res);endp;
proc kernalb(z,m,h);local a,res,t;z=(z-m)./h;  @ BIWEIGHT KERNAL @
    t=(abs(z).<1);
    res=t.*((15/16)*((1-(z^2))^2));retp(res);endp;
proc kernalt(z,m,h);local a,res,t;z=(z-m)./h;  @ TRIANGULAR KERNAL @
    t=(abs(z).<1);
    res=t.*(1-abs(z));retp(res);endp;
proc kernalr(z,m,h);local a,res,t;z=(z-m)./h;  @ RECTANGULAR KERNAL @
    t=(abs(z).<1);
    res=t.*0.5;retp(res);endp;
proc loadasc(file,obns,vars);local d;loadm d[]=^file;if rows(d)/=vars*obns;
    "input data file error: wrong number of cols or rows";stop;endif;
    d=reshape(d,obns,vars);retp(d);endp;

@ --------------------------------------------------------------------- @
dataset="indiana.asc";
obns=100;
vars=8;
title="Indiana, 1974";
               ycol=5;
@ --------------------------------------------------------------------- @
h=0.02;         @ SMOOTHING PARAMETER h>0 @
strt=0;         @ ORIGIN @
endd=1;         @ END    @
pts=200;        @ POINTS TO BE PLOTTED @
kernal=2;       @ KERNAL CHOICE:  1=normal, 2=Epanechnikov, 3=Biweight,
                                  4=triangular, 5=rectangular @
xlabel="Proportion Dem. Votes, v";ylabel="f(v)";
d=miss(loadasc(dataset,obns,vars),-9); @ LOADS AN ASCII FILE INTO D @
y=packr(d[.,ycol]);@ DATA; DEFINE AN NX1 VECTOR Y @


px=seqa(strt,(endd-strt)/pts,pts+1); py=px;
format 5,0;locate 10,1;"Calculating ";;pts;;" Points: ";
i=1;do while i<=pts;
    if kernal==1;
    py[i,1]=sumc( kernaln(py[i,1],y,h) )./(rows(y).*h);
    elseif kernal==2;
    py[i,1]=sumc( kernale(py[i,1],y,h) )./(rows(y).*h);
    elseif kernal==3;
    py[i,1]=sumc( kernalb(py[i,1],y,h) )./(rows(y).*h);
    elseif kernal==4;
    py[i,1]=sumc( kernalt(py[i,1],y,h) )./(rows(y).*h);
    elseif kernal==5;
    py[i,1]=sumc( kernalr(py[i,1],y,h) )./(rows(y).*h);
    else; "K parameter incorrect";stop;endif;
    locate 10,35;i;;i=i+1;
endo;
    @ ** 2D plot ** @
plottype="XY";fonts="simplex";colors=15*ones(8,1);cross=0;tekfile="notek";
let world[2,2]=0 0 1 7;axesflag=1;cross=0;xt=5|0;yt=5|0;gridinfo=0|0;
let symbols[5,1]=1 0.05 6 15 1;tickflag="0";scrnflag=1;datestmp="";
let gridinfo=0 0;xformat="1f";yformat="1f";let charhts=0.35 0 0;rotate=0;
let plotsize[2,2]=0 0 0 0;let plotshft[2,2]=0 0 0 0;let boxflag=1 0;
call graph2d;
format 16,8;
