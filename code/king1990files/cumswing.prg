#lineson;enable;
proc loadasc(file,obns,vars);local d;loadm d[]=^file;if rows(d)/=vars*obns;
    "input data file error: wrong number of cols or rows";stop;endif;
    d=reshape(d,obns,vars);retp(d);endp;

@ ----------------------------------------------------------- @
xlabel="Democratic votes";  xcol=1;
ylabel="Democratic seats";  ycol=5;
dataset="us.asc";
obns=21;
vars=10;
@ ----------------------------------------------------------- @

d=miss(loadasc(dataset,obns,vars),-9);
px=sortc(packr(d[.,xcol]),1);py=sortc(packr(d[.,ycol]),1);
title="Indiana Cum. Partisan Swing";
    @ ** 2D plot ** @
plottype="XY";fonts="simplex";colors=15*ones(8,1);cross=0;tekfile="notek";
let world[2,2]=. 0 . 1;axesflag=1;cross=0;xt=5|0;yt=5|0;gridinfo=0|0;
let symbols[5,1]=1 0.05 6 15 -1;tickflag="0";scrnflag=1;datestmp="";
let gridinfo=0 0;xformat="1f";yformat="1f";let charhts=0.35 0 0;rotate=0;
let plotsize[2,2]=0 0 0 0;let plotshft[2,2]=0 0 0 0;let boxflag=1 0;
call graph2d;

