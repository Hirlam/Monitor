// Input file

title = "Bias maps of surface variables"

framec="Teal"


v[0] = ['m','M']
t[0] = ['By time of day','By fc hour']
v[1] = ['b','r']
t[1] = ['Bias','Rmse']
   
v[2] = ['00000000_00000000']
t[2] = v[2] ;

v[3] =[1] ;
t[3] = v[3] ;

v[4] = ['calc',0,6]
t[4] = ['Wind speed','Wind dir','PMSL','T2m','Rh2m','q2m','Cloud cover']

v[5] =[0,1] ;
t[5] = v[5] ;
v[6] =[0,1,2]
t[6] = ['G05','UM4','al00']
mname = ["Type","Error","Station","dum","Parameter","Step","Exp"]
pdir ="http://www.smhi.se/sgn0106/if/hirald/WebgraF/MODINT/Surface/"
ext='png'
do_debug = false
help = "Different models:       <br> G05: HIRLAM, 5.5km, 60 levels, H      <br> UM4: UM, 3.6km, ?? levels, NH      <br> al00: ALADIN, 11km, 60 levels, H"; hide_help = false ;

