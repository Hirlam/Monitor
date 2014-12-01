#!/bin/sh
#

set_bases(){
  bsurf=""
  [ "$ANASURF" != "none" ] && bsurf="ecma_sfc"
  if [ "$ANAATMO" == "3DVAR" -o "$ANAATMO" == "4DVAR" ]; then
    bases="ccma ecma"
  fi
  bases="$bases $bsurf"
}

set_obtypes(){
  obtypes="synop dribu ship aircraft temp amsua amsub mhs iasi radar scatt"
  #obtypes="iasi"
}

set_levels(){
  case $1 in
    "temp"|"aircraft"|"pilot")
      levels="100000 92500 80000 60000 45000 35000 27500 22500 17500 12500 8500 6500 4000 2500 1500"
      ytics='(">925" 100000 ,"925-800" 92500 ,"800-600" 80000 ,"600-450" 60000,"450-350" 45000,"350-275" 35000 ,"275-225" 27500,"225-175" 22500,"175-125" 17500,"125-85" 12500,"85-65" 8500 ,"65-40" 6500 ,"40-25" 4000 ,"25-15" 2500 ,"15-0" 1500 )'
#      ytics='(">925" 100000 ,"925-800" 92500 ,"800-600" 80000 ,"600-450" 60000,"450-350" 45000,"350-275" 35000 ,"275-225" 27500,"225-175" 22500,"175-125" 17500,"125-85" 12500,"85-65" 8500 ,"     " 6500 ,"40-25" 4000 ,"     " 2500 ,"15-0" 1500 )'
    ;;
    *)
      echo "Levels not defined for observation type $1"
      exit 1
    ;;
  esac
}

set_satelites(){
  case $1 in
    "amsua")
      satelites="noaa15 noaa16 noaa17 noaa18 noaa19 metop2"
    ;;
    "amsub")
      satelites="noaa15 noaa16 noaa17 noaa18"
    ;;
    "mhs")
      satelites="noaa19 metop2"
    ;;
    "iasi")
      satelites="metop2"
    ;;
    *)
      echo "Satelited not defined for instrument $1"
      exit 1
    ;;
  esac

}

set_level_type(){
  case $1 in
    "temp"|"aircraft"|"pilot")
      level_type="vert"
      set_levels $1
    ;;
    "iasi"|"amsua"|"amsub"|"mhs")
      level_type="sat"
      set_satelites $1
    ;;
    "synop"|"dribu"|"ship")
      level_type="surf"
    ;;
    *)
      echo "Level type not defined for observation type $1"
      exit 1
    ;;
  esac
}

set_vars(){
  case $1 in
    "temp")
      vars="t u v q"
    ;;
    "aircraft")
      vars="t u v"
    ;;
    "pilot")
      vars="u v"
    ;;
    "synop"|"dribu"|"ship")
      vars="z"
    ;;
    "iasi"|"amsua"|"amsub"|"mhs")
      vars="bt"
    ;;
    *)
      echo "Variables not defined for observation type $1"
      exit 1
    ;;
  esac
  #echo $vars
}

set_obtype(){
  set_vars $1 || exit 1
  set_level_type $1 || exit 1
}

set_unit(){
  case $1 in
    "t")
      unit="K"
    ;;
    "z")
      unit="m"
    ;;
    "u"|"v")
      unit="m/s"
    ;;
    "q")
      unit="g/m^3"
    ;;
    "bt")
      unit="K"
    ;;
    *)
      unit="no unit set"
    ;;
  esac
  echo $unit
}

## TEST
#obtypes="synop temp aircraft pilot ship iasi amsua amsub mhs"
#for obtype in $obtypes; do
#  set_obtype $obtype
#  echo $obtype
#  echo $vars
#  echo $levels
#  for var in $vars; do
#    set_unit $var
#  done
#done
