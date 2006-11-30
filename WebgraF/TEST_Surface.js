// Input file

title = "Surface verification"

framec="Goldenrod"

v[0] = ['PS_00000000','V_00000000','v_00000000','f_00000000']
t[0] = ['Timeserie stat','Fc length ver','Dayvar','Freq dist.']

v[1] = ['00000000']
t[1] = v[1]

v[2] =  [1] ;
t[2] = v[2] ;

v[3] = [0,1,2,3,4,5,6,7]
t[3] = ['Wind speed','Wind dir','PMSL','T2m','Rh2m','q2m','Cloud cover','Precip']

v[4] =  [0] ;
t[4] = v[4] ;

v[5] =  [0] ;
t[5] = v[5] ;

mname = ["Type","Station","dum","Parameter","Level","Exp"]
help = "<b> ALADIN and AROME experiments </b>       <br> ALD  : 11km, 40 levels       <br> ARO : 2.5km, 40 levels"; hide_help = false ;

pdir ="Surface/"
ext='png'
do_send = true ;

