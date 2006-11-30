
// Input file definition for WebgraF

// Title
title ="Forecast maps"

pdir = "http://www.smhi.se/sgn0106/if/hirald/WebgraF/OPER/Maps/"

framec='coral'
backgc='lightgrey'


ext='png'

// Menu headers
mname = ["Type","Area","Exp","Date","Length"]

v[0] = ['map']
t[0] = ['Map']
v[1] = ['large']
t[1] = ['Large']
v[2] = ['C22','E11','al00']
t[2] = ['C22','E11','al00']
v[3] = ['gen_date','YYYYMMDDHH',today(),today(-4),24]
t[3] = ['gen_date','YYYY/MM/DD',today(0,'YYYY/MM/DD'),today(-4,'YYYY/MM/DD'),24]
v[4] = ['month_num',1,48,1,2]
t[4] = ['month_num',1,48,1,1]




help =" C22 : HIRLAM 22km <br> E11 : HIRLAM 11 km <br> al00 : ALADIN 11 km " ;


start_at = "0_0_0_1_23"
pre_lan = 0
do_send = true
do_subset = true ;
