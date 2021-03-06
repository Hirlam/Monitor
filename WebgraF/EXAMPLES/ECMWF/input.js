
// Input file definition for WebgraF

// Title
title ="ECMWF forecasts"

pdir = "http://www.ecmwf.int/products/forecasts/d/getchart/catalog/products/forecasts/medium/deterministic/msl_uv850_z500!"

framec='lightsalmon'
backgc='#FFFFF2'

// Menu headers
mname = ["Chart","Fc length","Area","Dum","Date","Dum5"]

v[0] = ['Wind%20850%20and%20mslp','Geopotential%20500%20hPa']
t[0] = ['Wind 850 & Mslp','Geopot 500 hPa']
v[1] = [72,96,120,144,168]
t[1] = [72,96,120,144,168]
v[2] = ['Europe','North%20hemisphere']
t[2] = ['Europe','North hemisphere']
v[3] = ['pop!od!oper!public_plots']
t[3] = ['pop!od!oper!public_plots']
v[4] = ['gen_date','YYYYMMDDHH',today(-1),today(-4),12]
t[4] = ['gen_date','YYYYMMDDHH',today(-1),today(-4),12]
v[5] = ['!chart']
t[5] = ['!chart']

sep='!'
ext='gif'
loc = ['l','t','l','l','t','l']

info ='http://www.ecmwf.int/products/forecasts/d/charts/medium/deterministic/'

do_debug = true
