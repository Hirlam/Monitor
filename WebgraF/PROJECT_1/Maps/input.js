title ="Forecast maps"
pdir = "http://www.smhi.se/sgn0106/if/hirald/WebgraF/OPER/Maps/"

framec='coral'
backgc='lightgrey'

ext='png'

mname = ["Type","Area","Exp","Date","Length"]

v[0] = ['map']
t[0] = ['Map']
v[1] = ['large']
t[1] = ['Large']
v[2] = ['C22','E11','al00_31t0']
t[2] = ['C22','E11','al00_31t0']
v[3] = ['gen_date','YYYYMMDDHH',today(-1),today(-5),24]
t[3] = ['gen_date','YYYY/MM/DD',today(-1,'YYYY/MM/DD'),today(-5,'YYYY/MM/DD'),24]
v[4] = ['month_num',1,30,1,2]
t[4] = ['month_num',1,30,1,1]

start_at = "0_0_2_1_0" ;

pre_lan = 0

do_subset = true ;

hide_help = false;

help =" C22 : HIRLAM 22km <br> E11 : HIRLAM 11 km <br> al00_31t0 : ALADIN 11km CY31t0 " ;


