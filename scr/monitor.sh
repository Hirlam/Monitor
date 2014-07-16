#!/bin/bash

if [ "$#" -ne "4" -a "$#" -ne "6" -a "$#" -ne "8" ]; then
  echo "Usage $0 dtgstart dtgend base obtype [ instrument instrument_selection [ instrument_satelite satelite_channels ]]"
  exit 1
else
  dtgstart=$1
  dtgend=$2
  base=$3
  obtype=$4
  instrument=$5
  instrument_selection=$6
  instrument_satelite=$7
  satelite_channels=$8

  if [ "$WRK" == "" -o "$FCINT" == "" -o "$EXTRARCH" == "" ]; then
    echo "The following environment variables are needed:"
    echo "     WRK: $WRK"
    echo "EXTRARCH: $EXTRARCH"
    echo "   FCINT: $FCINT"
    exit 1
  fi

  # Default obsusage for this obtype
  obsusage="l${obtype}=.T.,"

  # Manipulate obtype
  partinfo_name_instrument=""
  partinfo_instrument_selection=""
  partinfo_end1=""
  if [ "$instrument" != "" -a "$instrument_selection" != "" ]; then
    partinfo_name_instrument="&$instrument"
    partinfo_instrument_selection="$instrument_selection,"
    partinfo_end1="/"
  fi
  partinfo_name_satelite=""
  partinfo_channel_selection=""
  partinfo_end2=""
  if [ "$instrument_satelite" != "" -a "$satelite_channels" != "" ]; then
    partinfo_name_satelite="&$instrument_satelite"
    partinfo_channel_selection="channels=$satelite_channels,"
    partinfo_end2="/"
  fi

  # Namelist for obsmon
  cat > fort.4 << EOF
&obsmon
  verbose=3,
  $obsusage
  odbbase="${base}",
  lusage=.TRUE.,
  lstat=.TRUE.,
/

$partinfo_name_instrument
$partinfo_instrument_selection
$partinfo_end1

$partinfo_name_satelite
$partinfo_channel_selection
$partinfo_end2

EOF

  # Generate statistics for each dtg
  # Run obsmon program to generate statistics
  $BINDIR/obsmon $dtgstart $dtgend $FCINT  

fi 
