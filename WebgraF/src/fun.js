//----------------------------------------------------
function expand (){

 var pat = /\d+-\d+/;
 for (j = 0; j < (this.v.length); j++) {
   while (this.v[j].match(pat) != null) {
     i3 = this.v[j].match (pat);
     ii = i3[0].split ("-") 
     str = ii[0];
     is = parseInt(ii[0]) + 1 
     for (k = is; k <= ii[1]; k++) {
       str += "," + k;
     }
     this.v[j] = this.v[j].replace(pat,str);
   };
 };
}
//----------------------------------------------------
function resize(size_ind) {
   if ( size_ind == 0 ) { size_fig = 1.0 }else{
      if ( size_fig < Math.abs(size_ind) && size_ind < 0 ) {
       size_fig = (1+size_ind) * size_fig
      }else{
       size_fig += size_ind
      }
   }

   if ( multi ){
      multi = false
      getFig(mpos,-1)
   } else {
      getFig(0,pos[0])
   }
}
//----------------------------------------------------
function debug_window() {

   passing = input_list[choice_ind].substr(0,input_list[choice_ind].lastIndexOf("\.js"))
   newURL=baseURL.substr(0,baseURL.lastIndexOf("index\.html"))+'/'+passing+'/debug.html'
   passing = 'debug_'+passing
   window.open(newURL,passing,'width=700,height=400,scrollbars=yes,menubar=yes')

}
//----------------------------------------------------
function createLink() {

   var hej = new Array()
   remlist = ""

   if ( rem_menu.t.length > 0 ) {
      for ( i=0 ; i <  rem_menu.t.length  ; i++) { 

      for ( ii=0 ; ii <  mlist.length  ; ii++) { 
         if ( mlist[ii].flip ) { rem_menu.pos[i][ii] = mlist[ii].t.length - 1 - rem_menu.pos[i][ii]}
      }

      hej[i] = rem_menu.pos[i].join('_')

      }
   } else { hej[0] = pos.join('_'); }

   if (hej.length >= 1 ) {remlist = hej.join(':')}

   passing = input_list[choice_ind].substr(0,input_list[choice_ind].lastIndexOf("\.js"))
   newURL=baseURL+"?choice_ind="+passing+";inremlist="+remlist ;
   window.location = newURL ;

}
// --------------------------------------------------------
function evalInput(inremlist) {
    inremlist = inremlist.split(":")
    for (i=0; i<  inremlist.length ; i++ ) {
         var p = inremlist[i].split('_')
         for (ii=0; ii<  p.length ; ii++ ) { p[i] = Number(p[i]) }
         figName(p);
    }
}
// --------------------------------------------------------
function evalURL() {
str = new String(window.location) ;
baseURL = str

si = str.lastIndexOf("?")

if ( si > 0 ) {
   eq_ind = str.indexOf("=")
   se_ind = str.indexOf(";")
   qm_ind = str.lastIndexOf("?")
   if ( se_ind == -1 ) { se_ind = str.length } 

   passing         = str.substring(eq_ind+1,se_ind)
   passing_remlist = str.substring(se_ind+11,str.length)

   if(isNaN(passing)) { passing = convert_passing() } ;

   passing='choice_ind='+passing ;
   if (passing_remlist.length > 0 ) { passing +=";inremlist='"+passing_remlist+"'" ; } ;
   eval(passing)

   baseURL = str.substr(0,qm_ind)
}

}
// --------------------------------------------------------
function convert_passing() {

    passing = passing.replace(/\%D6/g,"Ö") ;
    passing = passing.replace(/\%F6/g,"ö") ;
    passing = passing.replace(/\%C4/g,"Ä") ;
    passing = passing.replace(/\%E4/g,"ä") ;
    passing = passing.replace(/\%E5/g,"Å") ;
    passing = passing.replace(/\%C5/g,"å") ;

    for (i=0; i<  input_list.length ; i++ ) {
       passing_test = input_list[i].substr(0,input_list[i].lastIndexOf("\.js"))
       if ( passing_test == passing ) { passing = i ; return passing }
    } 

    return 0
 
}
// --------------------------------------------------------
function findProject(pind) {
   str = new String(window.location) ;
   // si = str.indexOf("WebgraF") + 8
   si = str.lastIndexOf("/") 
   str = str.substring(0,si)
   si = str.lastIndexOf("/") 
   if ( pind == 1 ) {
      si = str.lastIndexOf("/") + 1
      ei = str.length
      return str.substring(si,ei)
   }
   str2 = str.substring(0,si)
   si = str2.lastIndexOf("/") + 1
   ei = str.length
   return str.substring(si,ei)
}
// --------------------------------------------------------
function showUser() {
 this.txt = "<br><br>"
 this.txt += "navigator.language:"+ navigator.language  + "<br>"
 this.txt += "navigator.appName:"+ navigator.appName    + "<br>"
 this.txt += "navigator.platform: "+ navigator.platform + "<br>"
 this.txt += "navigator.appCodeName :"+ navigator.appCodeName + "<br>"
 this.txt += "window.screen.availWidth:" + window.screen.availWidth + "<br>"
 this.txt += "window.screen.availHeight :" + window.screen.availHeight + "<br>"
 return this.txt
}
// --------------------------------------------------------
function check_view_limit(lim) {
    if ( lim > view_limit ) { alert("Menu too long to view all plots ("+lim+"/"+view_limit+")") ; return true }
    return false
}
// --------------------------------------------------------
function flip_help() {

 if ( hide_help ) { hide_help = false }
             else { hide_help = true  }

 tmp = String(parent.frames['Pic'].location)

 if( tmp.indexOf("dp"   ,0) > 0 ) { update_all('Pic','dp'   ) }
 if( tmp.indexOf("info" ,0) > 0 ) { show_info()               }
 if( tmp.indexOf("slide",0) > 0 ) { update_all('Pic','slide') }

}
// --------------------------------------------------------
function set_backgc() { return "<style type='text/css'>body {  background-color : "+backgc+";}</style>" }
// --------------------------------------------------------
function goHome(i)
{
   gone_home = true
   change_input(i)
}
// --------------------------------------------------------
function change_input(i) {

   if ( input_list[i] != undefined ) { if ( input_list[i].indexOf("---",0) != -1 ) return }

   for (j = 0 ; j < mlist.length ; j++ ) { delete mlist[j] }
   mlist.splice(1,(mlist.length - 1))

   multi      = false
   choice_ind = i
   top_menu.p = i
   
   str = new String(window.location) ;
   si = str.lastIndexOf("?")
   passing = input_list[choice_ind].substr(0,input_list[choice_ind].lastIndexOf("\.js"))
   if ( si > 0 ) { window.location = str.substr(0,str.lastIndexOf("?")) + "?choice_ind=" + passing }
   else {          window.location = window.location                    + "?choice_ind=" + passing }

}
// --------------------------------------------------------
function clean(p)
{
   n = p.length - 1
   for (j = 0; j <= n; j++) { delete p[j] }
   p.length = 0

   return p
}
// --------------------------------------------------------
function adjust_rem(start,end,update) {

 rem_menu.t.splice(start,end)
 rem_menu.v.splice(start,end)
 rem_menu.pos.splice(start,end)
 rem_menu.p = Math.max(0,rem_menu.p - 1)
 
 rem_menu.active = ( rem_menu.t.length > 0 )

if ( ! on_start ) { 
    if ( rem_menu.active ) { getFig(-1,rem_menu.p)   }
                      else { update_all('Top','top','List','list','Top_left','top_left') }
}
 
}
// --------------------------------------------------------
function cri(inc)
{
    inc += Number(this.p)
    if ( inc  ==  this.v.length ) { return 0     }
    if ( inc  == -1 ) { return ( this.v.length - 1) }
    return inc
}
// --------------------------------------------------------
function menu_style(j) {

 if ( this.p == j || multi && mpos == this.ind ) { myclass = m_curr } else { myclass = m_item }
 return myclass

}
// --------------------------------------------------------
function figName(p)
{

   is_graphics = true
   this.act = ""
   this.tmp = new Array()
   this.src = pdir
   this.txt =  "" 
   this.pos = new Array()
 
   if (my_con[con_ind] == 'All' ) {
     for (j = 0; j < (spec_name.length - 1); j++) {
       if ( msep[spec_name[j]] == undefined ) { msep[spec_name[j]] = sep }
       this.src += mlist[spec_name[j]].v[p[spec_name[j]]] + msep[spec_name[j]]
     }
     this.src += mlist[spec_name[j]].v[p[spec_name[j]]]
     this.src += "." + ext
   } else {
     this.src = my_con[con_ind]
    // Expand [] arguments
    while ( this.src.search("\\[") != -1 ) {
        i1= this.src.search("\\[")
        i2= this.src.search("\\]")
        i3= this.src.substr(i1+1,i2-i1-1)
        if ( this.src.search("\\,") != -1 ) {
           hej = i3.split(",")
           i3 = hej[0]
        }
        this.src = this.src.replace(this.src.substr(i1,i2-i1+1),mlist[i3].v[p[i3]])
    } ;

    if ( this.src.match(/.xml/) || this.src.match(/.html/) ) {
       is_graphics = false
    }
   }

   jj = 0
   for (j = 0; j < mlist.length ; j++) {

      if (mlist[j].active ) {
         this.tmp[jj] = mlist[j].t[p[j]]
         if ( j == mpos ) { jjpos = jj }
         jj++
      }

   }
   this.txt = this.tmp.join(";")


   for (j = 0; j < mlist.length ; j++) { this.pos[j] = p[j] }

   if ( is_graphics ) {
     this.act = dirPic(this.src,("pre_getFig(["+this.pos+"])"),this.txt)
     dp_src = "nada"
   } else {
     this.act = ""
     dp_src = this.src
   } 

   found_rem = false
   if ( ! sli_menu.active && do_remember ) {

      for (j = 0; j < rem_menu.t.length ; j++) {
         if ( rem_menu.t[j] == this.txt ) { found_rem = true ; break }
      }
  
      if ( found_rem ) { rem_menu.p = j }
      else {
         j = rem_menu.t.length
         rem_menu.t[j]   = this.txt
         rem_menu.v[j]   = this.txt
         rem_menu.active = ( j >= 0 )
         rem_menu.p      = j 

         rem_menu.pos[j] = new Array()
         for (jj = 0; jj < mlist.length ; jj++) { rem_menu.pos[j][jj] = this.pos[jj] }
      }

   } else { rem_menu.active = false }

   if ( multi && mpos >= 0 ) {
      this.tmp.splice(jjpos,1,(lang[pre_lan].late+" "+mlist[mpos].name)) 
      this.txt = this.tmp.join(";")
    }


}
// --------------------------------------------------------
function pre_getFig(p) {

 pos = p
 multi = false
 getFig(0,pos[0])

}
// --------------------------------------------------------
function getFig(ind,selectedIndex) {

 if ( ind == -1 ) {

    sli_menu.active = false
    if ( selectedIndex == -1 ) {

       if ( check_view_limit(rem_menu.t.length)) { return }
       fig = new Array()
       p   = new Array()

       for ( i=0 ; i < rem_menu.v.length ; i++ ) {
          
          for ( ii=0 ; ii < pos.length ; ii++ ) { p[ii] = rem_menu.pos[i][ii] }
          tmp    = new figName(p)
          fig[i] = tmp.act

       }
   
       multi   = true
       mpos    = ind
       fig.act = fig.join("")

    } else {

       multi = false
       for ( i=0 ; i < pos.length ; i++ ) { pos[i] = rem_menu.pos[selectedIndex][i] }
       getFig(0,pos[0])
       return

   }

 } else {

    if ( mpos == -1 ) { multi = false }

    if ( selectedIndex == -1 ) {

       sli_menu.active = false

       if (multi && mpos == ind) {

          fig   = new figName(pos)
          multi = false

       } else {

          if ( check_view_limit(mlist[ind].t.length)) { return }

          fig = new Array()
          p   = new Array()

          multi   = true
          mpos    = ind

          for ( i=0 ; i < pos.length ; i++ ) { p[i] = pos[i] }

          for ( i=0 ; i < mlist[ind].v.length ; i++ ) {
   
             p[ind] = i 
             tmp    = new figName(p)
             fig[i] = tmp.act
          }
   
          fig.act = fig.join("")
          fig.txt = tmp.txt
       
       }

    } else {

       if ( sli_menu.active ) {
          pos[ind] = selectedIndex
          slideShow(sli_menu.p)
          return
       }

       if ( multi && mpos != ind  ) {

          pos[ind] = selectedIndex
          multi = false
          getFig(mpos,-1)

       } else {
          if ( multi && mpos == ind ) { multi = false }
          pos[ind] = selectedIndex
          fig = new figName(pos)
       }
    }

 }
 
 if ( ! on_start ) { update_all('List','list','Top_left','top_left','Top','top','Pic','dp') }

}
// --------------------------------------------------------
function today()
{
// Create dynamic time array
 dh_0 = 0
 if ( arguments[0] != undefined ) { dh_0 = - arguments[0]*24 }

      sep_yy ='' ; sep_hh ='' ;
      sep_ym ='' ; sep_md ='' ;  sep_dh ='' ;
      so = 0 ; si=0 ;
      
      include_yy = true ;
      include_mm = true ;
      include_dd = true ;
      include_hh = true ;

      dh = dh_0

 if ( arguments[1] != undefined ) {

      include_yy = false ;
      include_mm = false ;
      include_dd = false ;
      include_hh = false ;

      form = arguments[1] 
      if ( form.indexOf("YYYY",si) > -1 ) { 
         si = form.indexOf("YYYY",si)
         if ( so != si ) { sep_yy = form.substring(so,si) }
         si += 4 ; so = si  ;
         include_yy = true ;
         dh = dh_0 * 365
      } ;

      if ( form.indexOf("MM"  ,si) > -1 ) {
         si = form.indexOf("MM",si)
         if ( so != si ) { sep_ym = form.substring(so,si) }
         si += 2 ; so = si  ;
         include_mm = true ;
         dh = dh_0 * 31 
      } ;

      if ( form.indexOf("DD"  ,si) > -1 ) {
         si = form.indexOf("DD",si)
         if ( so != si ) { sep_md = form.substring(so,si) }
         si += 2 ; so = si  ;
         include_dd = true ;
         dh = dh_0
      };

      if ( form.indexOf("HH"  ,si) > -1 ) {
         si =form.indexOf("HH",si)
         if ( so != si ) { sep_dh = form.substring(so,si) }
         si += 2 ; so = si  ;
         include_hh = true ;
         dh = dh_0 / 24
      } ;

       so = form.length ;
       if ( so != si ) { sep_hh = form.substring(so,si) }
 } ;

 add = magn(10,0)
 add = add.reverse()

 cday = new Date()
 nday = new Date(Date.UTC(cday.getFullYear(),cday.getMonth(),   cday.getDate(),
                 cday.getHours(),0,0) - (dh+cday.getTimezoneOffset()/60)*60*60*1000)

         if ( nday.getTimezoneOffset() - cday.getTimezoneOffset() != 0 ) {
         nday = new Date(Date.UTC(cday.getFullYear(),cday.getMonth(),   cday.getDate(),
                                  cday.getHours(),0,0) - (dh+nday.getTimezoneOffset()/60)*60*60*1000)
         }

 sy = nday.getFullYear()
 sm = add[magn(Number(nday.getMonth()+1))] + Number(nday.getMonth()+1)
 sd = add[magn(nday.getDate())] + nday.getDate()
 sh = '00'

 date = sep_yy
 if ( include_yy) { date += sy }
 date += sep_ym
 if ( include_mm) { date += sm }
 date += sep_md
 if ( include_dd) { date += sd }
 date += sep_dh
 if ( include_hh) { date += sh }
 date += sep_hh
 //date = sep_yy + sy + sep_ym + sm + sep_md + sd + sep_dh + sh + sep_hh ;
 return date

}
// --------------------------------------------------------
function checkV()
{

var is
var ie
var my
var mm
var ii
var jj
var txt
var p = arguments[0]

// Number axis
   if ( p[0] == 'calc' ) {


      if ( p[1] < p[2] ) { mstart = p[1] ; mstop = p[2] ; reverse = false }
                    else { mstart = p[2] ; mstop = p[1] ; reverse = true  }

      if ( p[3] == undefined ) { mm_inc = 1 } else { mm_inc = Math.abs(p[3]) }

      clean(p)

      mm = mstart
      jj = 0
      do {
         p[jj] =  mm
         mm += mm_inc
	 jj += 1
      } while ( mm <= mstop )

      if ( reverse ) { p = p.reverse() }

   }

// Year month axis or text axis

   if ( p[0] == 'time_axis' || p[0] == 'time_text' ) {

      flag = p[0]

      is = Number(p[1])
      ie = Number(p[2])

      if ( p[1] < p[2] ) { jj = 1} else { jj = -1} 

      clean(p)

      mm = is % 100
      my = ( is - mm ) / 100
      ii = 0

      do {
         if ( flag == 'time_text' ) { p[(ii)] = months[pre_lan][(mm-1)] + " " + my }
                               else { p[(ii)] = is                    }

         if ( is == ie ) { break}
         ii++ ; mm += jj
         if (mm > 12 ) { my = my + jj ; mm = 1 }
         if (mm <  1 ) { my = my + jj ; mm = 12}
         is = my * 100 + mm
         
      } while ( 1 > 0 )
   }

// Year month archive

   if ( p[0] == 'time_arch' ) {

      flag = p[0]

      is = Number(p[1])
      ie = Number(p[2])
      my_sep = '/' ;
      if ( p.length == 4 ) { my_sep = p[3] } ;

      if ( p[1] < p[2] ) { jj = 1} else { jj = -1}

      clean(p)
      mm = is % 100
      my = ( is - mm ) / 100
      ii = 0

      do {

         if ( mm < 10 ) { add = "0" } else { add ="" }
         p[(ii)] = my + my_sep + add + mm
         if ( my_sep == '/' ) { p[(ii)] += my_sep }

         if ( is == ie ) { break}
         ii++ ; mm += jj
         if (mm > 12 ) { my = my + jj ; mm = 1 }
         if (mm <  1 ) { my = my + jj ; mm = 12}
         is = my * 100 + mm

      } while ( 1 > 0 )
   }

// Month list

   if ( p[0] == 'month_num' ) {

      if ( p[1] == undefined ) { mstart = 1    ; mstop = 12   } else { 

         if ( p[1] < p[2] ) { mstart = p[1] ; mstop = p[2] ; reverse = false }
                       else { mstart = p[2] ; mstop = p[1] ; reverse = true  }

      }

      if ( p[3] == undefined ) { mm_inc = 1 } else { mm_inc = Math.abs(p[3]) }

      if ( p[4] == undefined ) {
         add = magn(Math.max(mstart,mstop),0)
      } else {
         mm = 1
         jj = 1
         do { mm *= 10 ; jj += 1 } while ( jj <= p[4] )
         add = magn(mm,0)
      }
      add = add.reverse()

      clean(p)

      mm = mstart
      jj = 0
      do {
         p[jj] =  add[magn(mm)] + String(mm) 
         mm += mm_inc
         jj += 1
      } while ( mm <= mstop )
      if ( reverse ) { p = p.reverse() }

   }

// Month text list

   if ( p[0] == 'month_txt' ) {

      mstart = 1 ; mstop = 12 ; reverse = false

      if ( p[1] != undefined ) {
         if ( p[1] < p[2] ) { mstart = p[1] ; mstop = p[2] ; reverse = false }
                       else { mstart = p[2] ; mstop = p[1] ; reverse = true  }
      }

      clean(p)
      jj = 0
      for ( mm=mstart ; mm <= mstop ; mm++ ) {
         p[(jj)] =  months[pre_lan][(mm-1)]
         jj += 1
      }

      if ( reverse ) { p = p.reverse() }
   }

// Season text list
   if ( p[0] == 'season_txt' ) {

      mstart = 1 ; mstop = 4 ; reverse = false

      if ( p[1] != undefined ) {
         if ( p[1] < p[2] ) { mstart = p[1] ; mstop = p[2] ; reverse = false }
                       else { mstart = p[2] ; mstop = p[1] ; reverse = true  }
      }

      clean(p)
      jj = 0
      for ( mm=mstart ; mm <= mstop ; mm++ ) {
         
         p[(jj)] =  seasons[pre_lan][(mm-1)]
         jj += 1
      }

      if ( reverse ) { p = p.reverse() }
   }

// General date stepping
   if ( p[0] == 'gen_date' || p[0] == 'txt_date' ) {

      sy = 0 ; sm = 0, sd = 0 , sh = 0
      ey = 0 ; em = 0, ed = 0 , eh = 0

      p[2] = p[2].toString()
      p[3] = p[3].toString()

      do_month  = false ; 
      do_day    = false ; 
      do_hour   = false ; 
      do_text   = ( p[0] == 'txt_date' )

      dh = -1 


      si = 0  ; so = 0 ;
      sep_yy ='' ; sep_hh ='' ;
      sep_ym ='' ; sep_md ='' ;  sep_dh ='' ;
      
      if ( p[1].indexOf("YYYY",si) > -1 ) { 
  
         si = p[1].indexOf("YYYY",si)
         sy = p[2].substr(si,4) 
         ey = p[3].substr(si,4) 

         if ( so != si ) { sep_yy = p[1].substring(so,si) }
         si += 4 ; so = si  ;

      } ;


      if ( p[1].indexOf("MM"  ,si) > -1 ) {
         si = p[1].indexOf("MM",si)
         sm = p[2].substr(si,2) - 1
         em = p[3].substr(si,2) - 1

         if ( so != si ) { sep_ym = p[1].substring(so,si) }
         si += 2 ; so = si  ;
         do_month = true ; 

      } else {
          sm = 06 
          em = 06 
	  if ( dh == -1 ) {
             if ( p[4] == undefined ) { dh = 6*30*24 } else { dh = p[4]*30*24 } ;
	  } ;
      } ;


      if ( p[1].indexOf("DD"  ,si) > -1 ) {
         si = p[1].indexOf("DD",si)
         sd = p[2].substr(si,2)
         ed = p[3].substr(si,2)

         if ( so != si ) { sep_md = p[1].substring(so,si) }
         si += 2 ; so = si  ;
       
         do_day = true ; 
       
      } else {
         sd = 15 
         ed = 15 
	 if ( dh == -1 ) {
             if ( p[4] == undefined ) { dh = 30*24 } else { dh = p[4]*30*24 } ;
	 } ;
      };


      if ( p[1].indexOf("HH"  ,si) > -1 ) {
         si = p[1].indexOf("HH",si)
         sh = p[2].substr(si,2)
         eh = p[3].substr(si,2)

         if ( so != si ) { sep_dh = p[1].substring(so,si) }
         si += 2 ; so = si  ;

         do_hour = true ; 
	 if ( dh == -1 ) {
            if ( p[4] == undefined ) { dh = 6 } else { dh = p[4] } ;
	 } ;

      } else {
         sh = 12 
         eh = 12 
	 if ( dh == -1 ) {
            if ( p[4] == undefined ) { dh = 24 } else { dh = p[4]*24 } ;
	 } ;
      } ;

      so = p[1].length ;
      if ( so != si ) { sep_hh = p[1].substring(so,si) }

      cday = new Date(sy,sm,sd,sh,00,00)
      eday = new Date(ey,em,ed,eh,00,00)

      dday = cday - eday ; 

      if ( dday > 0 ) {
         reverse = true
         cday = new Date(ey,em,ed,eh,00,00)
         eday = new Date(sy,sm,sd,sh,00,00)
      } else {
         reverse = false
      }

      add = magn(10,0)
      add = add.reverse()

      clean(p)

      jj = 0
      do {

         if ( ! do_day && ! do_hour && do_text ) {
            p[jj]  = sep_yy + months[pre_lan][cday.getMonth()]
            p[jj] += sep_ym + cday.getFullYear() 
         } else {
            p[jj]  = sep_yy + cday.getFullYear() 
            p[jj] += sep_ym + add[magn(Number(cday.getMonth()+1))] + Number(cday.getMonth()+1)
         } ;

         if ( do_day ) {
            p[jj] += sep_md + add[magn(cday.getDate())] + cday.getDate() 
         } ;
  
         if ( do_hour ) {
            p[jj] += sep_dh + add[magn(cday.getHours())] + cday.getHours() 
         } ;

         p[jj] += sep_hh ;

         nday = new Date(Date.UTC(cday.getFullYear(),cday.getMonth(),   cday.getDate(),
                                  cday.getHours(),0,0) + (dh+cday.getTimezoneOffset()/60)*60*60*1000)

         if ( nday.getTimezoneOffset() - cday.getTimezoneOffset() != 0 ) {
         nday = new Date(Date.UTC(cday.getFullYear(),cday.getMonth(),   cday.getDate(),
                                  cday.getHours(),0,0) + (dh+nday.getTimezoneOffset()/60)*60*60*1000)
         }
         cday = nday

         if ( ! do_hour ) { cday.setHours(00) }
         if ( ! do_day  ) { cday.setDate(15)  }
 
         dday = cday - eday ; 

         jj += 1 ;

      } while ( dday <= 0 )

      if ( reverse ) { p = p.reverse() }

   }

// Return the new array

   tmp = new Array()
   for ( jj = 0 ; jj < p.length ; jj++ ) { tmp[jj] = p[jj] }
   return tmp
}
// --------------------------------------------------------
function magn(a,f) {

     if ( f == 0 ) {
        add = new Array()
        add[0] = ''
     }

     aa = a

     disorder = 0
     while ( aa >= 10 ) {

        aa = aa /10
        disorder++
        if ( f == 0 ) {  add[disorder] = add[(disorder-1)] + '0'}

     }

     if ( f == 0 ) { return add      }
     else          { return disorder }

}
// --------------------------------------------------------
function update_all() {

   i = 0
   do {

      if ( arguments[i] == 'Pic' && dp_src != "nada" ) {
        parent.frames['Pic'].location = dp_src
      } else {
        pp = arguments[i+1]
        if( pp.indexOf("html",0) < 0  && pp.indexOf("xml",0) < 0 ) { pp += ".html" }
        parent.frames[arguments[i]].location = pp
      }
      i++ ; i++

  } while ( i < arguments.length )

}
// --------------------------------------------------------
function nada() { return ""}
// --------------------------------------------------------
function sel_top(w,action) {
   this.txt  = "<td bgcolor='#ffffff' rowspan='1' colspan='"+w+"' width='"+twidth+"'>"
   this.txt += "<select name='inputs' onchange='parent."+action+"' class="+m_curr+">"
   return this.txt
}
// --------------------------------------------------------
function dirFig(fg,action,ft,tdstyle)
{
   if ( tdstyle == undefined ) { tdstyle = '' }
   tfg = " No plot available "
   tfg = fg
   return "<td "+tdstyle+"><a href='javascript:parent." +action+ "' a><img alt='" +tfg+ "' title='" +ft+ "' src='" +fg+ "' border='0'></a></td>"
}
// --------------------------------------------------------
function dirPic(fg,action,ft,tdstyle)
{
   if ( tdstyle == undefined ) { tdstyle = '' }
   fgs = ""

   re  = /svg$/ ;

   if ( ! fg.match(re)) {
      myimage = new Image()
      myimage.src = fg
      if ( do_resize ) {
         if (myimage.height != 0 ) {hgt = myimage.height * size_fig }
         if (myimage.width  != 0 ) {wdt = myimage.width  * size_fig }
         if ( wdt != 0 || hgt != 0 ){ fgs =" width="+wdt+" height="+hgt+" " }
      }
   }
   
   tfg = " No plot available "
   tfg = fg

   if ( fg.match(re)) {
     // .svg file must be embedded
     return "<td "+tdstyle+"><a href='javascript:parent." +action+ "' a><embed src='"+fg+"'"+ fgs+" /embed></a></td>"
   } else {
     return "<td "+tdstyle+"><a href='javascript:parent." +action+ "' a><img alt='" +tfg+ "' title='" +ft+ "' src='" +fg+ "'"+ fgs+" border='0'></a></td>"
   }

}
// --------------------------------------------------------
function dirTxt(tdstyle,text,action,style,mytitle)
{

   
   if ( mytitle == undefined ) { mytitle = '' } ;
   if ( action == 'nada()' ) { return "<td "+tdstyle+"><div class='"+style+"' >"+text+"</div></td>" }
   else { return "<td "+tdstyle+"><a href='javascript:parent." +action+ "' title='"+mytitle+"' class='"+style+"' a>" +text+ "</a></td>" }

}
// --------------------------------------------------------
function table_top() {

   this.txt  = "<td><table bgcolor='"+framec+"' cellspacing='1'><tbody><tr><td>"
   this.txt += "<table cellpadding='1' cellspacing='0' border='0' width='"+twidth+"' bgcolor='"+framec+"'><tbody><tr>"
   return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function table_bot() {
   this.txt = "</tr></tbody></table></td></tr></tbody></table></td>"
   return this.txt
}
//--------------------------------------------------------------------------------------------------------------------
function all_head( ) { 

 this.txt  = ""
 this.cri  = cri

 typ = this.typ
 if ( this.loc == 'l' && this.typ == 0 && this.v.length > menu_type_0_maxlen ) {
  typ = 1 
 } 

 if ( typ == 1 ) { 
    if ( this.ind == -17 ) {
       this.txt += dirFig('down1.gif',("con_update("+this.cri(-1)+")"),lang[pre_lan].up)
    } else {
       this.txt += dirFig('down1.gif',("getFig("+this.ind+","+this.cri(-1)+")"),lang[pre_lan].up)
    }
 }

 if ( this.ind == -1 ) {

    this.txt += dirTxt('',lang[pre_lan].late,'getFig(-1,-1)',m_item,lang[pre_lan].mhed)
    if ( do_save && local_save && ! sli_menu.active ) { 
       this.mytitle = lang[pre_lan].saco
       this.txt += dirTxt('',lang[pre_lan].save,'SaveCookie()' ,m_item,this.mytitle) 
    }
    this.mytitle = lang[pre_lan].cltm
    this.txt += dirTxt('',lang[pre_lan].clea ,("adjust_rem(0,"+rem_menu.t.length+",true)"),m_item,this.mytitle)
    act = "adjust_rem("+this.p+",1,true)"

 } else {

    mytitle = lang[pre_lan].mhed + " " + this.name

    if ( this.ind == -17 ) {
       this.txt += dirTxt('width=100',this.name,"nada()",m_head,mytitle)
    } else {
       this.txt += dirTxt('width=100',this.name,("getFig("+this.ind+",-1)"),m_head,mytitle)
    }
    act = "adjustMenu("+this.ind+","+this.p+",true)"

 }

 if ( ! sli_menu.active ) {
    if ( do_flip   ) {
       this.mytitle = lang[pre_lan].fltm
       this.txt += dirTxt('width=20',lang[pre_lan].flip,("flip("+this.ind+")"),m_grey,this.mytitle) 
    }
    if ( ( do_delete || this.ind == -1 ) && this.ind != -17) { 
       this.mytitle = lang[pre_lan].rtpo
       this.txt += dirTxt('width=20',lang[pre_lan].remo.substr(0,3),act,m_grey,this.mytitle) 
    }
 }

 if ( do_slide && this.ind != -17 ) {
 if ( sli_menu.active && sli_menu.p == this.ind ) { this.txt += dirFig('stop.gif','slideShow(-99)',lang[pre_lan].stop)             }
        else { this.txt += dirFig('right.gif',("slideShow("+this.ind+")"),lang[pre_lan].slid,'width=10') }
 } 

 if ( typ == 1 ) { 
    this.txt += "</tr><tr>"
    if ( this.ind == -17 ) {
       this.txt += dirFig('up1.gif',("con_update("+this.cri(1)+")"),lang[pre_lan].down)
    } else {
    this.txt += dirFig('up1.gif',("getFig("+this.ind+","+this.cri(1)+")"),lang[pre_lan].down)
    }
 }
 if ( typ == 0 ) { this.txt += "</tr>" }

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function sim_head() { 
 this.txt = ""
 this.mytitle = ""

 switch(this.ind) {
 
    case -2:
    this.act = "rmCookie()"
    if ( pre_lan == 0 ) { this.mytitle = "Remove the cookie:"  + cok_menu.t[Math.max(0,cok_menu.p)] }
    if ( pre_lan == 1 ) { this.mytitle = "Ta bort kakan: " + cok_menu.t[Math.max(0,cok_menu.p)] }
    break

    case -4:
    if ( pre_lan == 0 ) { this.mytitle = "To the WebgraF main menu"    }
    if ( pre_lan == 1 ) { this.mytitle = "Till WebgraF huvudmeny" }

    if ( gone_home ) { this.act = "to_main()"         }
                else { this.act = "to_main()"  }
    gone_home = false
    break

    case -6:
    this.act = "flip_help()"

    if ( pre_lan == 0 ) { this.mytitle = "Show/hide help"  }
    if ( pre_lan == 1 ) { this.mytitle = "Visa/Göm hjälp" }
    break

    case -7:
    this.act = "nada()"
    break

    case -8:
    this.act = "nada()"
    break

    case -9:
    this.act = "nada()"
    break

    case -10:
    this.act = "nada()"
    break

    case -11:
    this.act = "show_info()"
    if ( pre_lan == 0 ) { this.mytitle = "Show some extra information" }
    if ( pre_lan == 1 ) { this.mytitle = "Visa extra information"}
    break

    case -12:
    this.act = "createLink()"
    break

    case -13:
    this.act = "debug_window()"
    if ( pre_lan == 0 ) { this.mytitle = "Show page definition" }
    if ( pre_lan == 1 ) { this.mytitle = "Visa definitionen för sidan"}
    break

    case -14:
    this.act = "nada()"
    this.txt += dirTxt('width=100',this.name,this.act,m_head,this.mytitle)
    this.txt += dirFig('down.gif' ,("resize(-0.15)"),lang[pre_lan].smal,'width=10')
    this.txt += dirFig('stop.gif' ,("resize(   0)"),lang[pre_lan].orig,'width=10')
    this.txt += dirFig('up.gif',   ("resize( 0.15)"),lang[pre_lan].larg,'width=10')
    break

    case -15:
    this.act = "nada()"
    break

    case -16:
    this.act = "nada()"
    break

    case -17:
    this.act = "nada()"
    break


 }


 if ( this.ind == -4 ) {
    this.act = "to_main()"
    this.txt += dirTxt('width=5','../',this.act,m_item,this.mytitle)
    this.txt += dirTxt('width=100',this.name,'nada()',m_head,this.mytitle)
 } else {
    if ( this.ind != -14 ) {
       this.txt += dirTxt('width=100',this.name,this.act,m_head,this.mytitle)
    }
 }

 this.txt += "</tr>" 

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function show_info() {
 tmp = info
 if ( info.lastIndexOf("http") < 0 ) {
    tmp =  baseURL.substr(0,baseURL.lastIndexOf("index.html")) 
    tmp += input_text[choice_ind]+'/'+ input_text[choice_ind]+'.html' 
 }
 parent.frames['Pic'].location = tmp
}
//--------------------------------------------------------------------------------------------------------------------
function to_main() { 
   if ( host_page == undefined ) { window.location = "../index.html" }
                            else { window.location = host_page       }
}
//--------------------------------------------------------------------------------------------------------------------
function my_alert(ind) { alert(this.name + this.p +" "+ this.active +" "+ this.v + "<br>" + this.t + "<BR>" ) }
//--------------------------------------------------------------------------------------------------------------------
function you_body( ) { 

 this.attrib = "bgcolor='#ffffff' rowspan='1' colspan='6'"
 this.menu_style = menu_style
 this.txt        = ""

    for (j = 0; j < this.v.length; j++) {
       if ( this.v[j] == 1 ) {
          this.act = "manipulate_menu("+j+")"
          this.txt += "<tr>" + dirTxt(this.attrib,this.t[j],this.act,this.menu_style(j)) + "</tr>"
       }
    }

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function gen_body( ) { 

 this.attrib = "bgcolor='#ffffff' rowspan='1' colspan='6'"

 this.menu_style = menu_style
 this.txt        = ""
 this.mytitle    = ""

 if ( this.ind >= 0 && this.v.length < 2 ) { return this.txt }

 typ = this.typ
 if ( this.loc == 'l' && this.typ == 0 && this.v.length > menu_type_0_maxlen ) {
  typ = 1 
 } 

 if ( typ == 0 ) {

    for (j = 0; j < this.v.length; j++) {
       switch (this.ind) {
       case -9:
          this.act = "slidepoint("+this.ind+","+j+")"
          break ;
       case -8:
          this.act = "slidepoint("+this.ind+","+j+")"
          break ;
       case -4:
          this.act = "change_input("+j+")"
          break ;
       case -2:
          this.act = "loadCookie("+j+")"
          break ;
       default:
          this.act = "getFig("+this.ind+","+j+")"
          break ;
       }

       this.txt += "<tr>" + dirTxt(this.attrib,this.t[j],this.act,this.menu_style(j),this.mytitle) + "</tr>"
    }

 } 

 if ( typ == 1 ) {

    switch (this.ind) {
    case -9:
       this.act = "slidepoint("+this.ind+",selectedIndex)"
       break ;
    case -8:
       this.act = "slidepoint("+this.ind+",selectedIndex)"
       break ;
    case -4:
       this.act = "change_input(selectedIndex)"
       break ;
    case -2:
       this.act = "loadCookie(selectedIndex)"
       break ;
    default:
       this.act = "getFig("+this.ind+",selectedIndex)"
       break ;
    }

    this.txt += sel_top(6,this.act)

    for (jj = 0; jj < this.v.length; jj++) {
       if ( this.p == jj ) { seltext = 'selected' } else { seltext ='' }
       this.txt += "<option " +seltext+" class="+m_item+">"+this.t[jj]+"</option>"
    }

    this.txt += "</select></td></tr>"
 }

 if ( this.ind >= 0 ) { pos[this.ind] = this.p }

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function dow_body( ) { 

 this.txt        = ""
 tmp = fig.src.substr(0,fig.src.indexOf(ext,0))

    for (j = 0; j < this.v.length; j++) {
         this.txt += "<tr><td bgcolor='#ffffff' rowspan='1' colspan='2'>"
         if ( this.t[j] == 'ps' ||  this.t[j] == 'pdf' ) {
            this.txt += "<a href='"+tmp+this.t[j]+"' class="+m_item+" a>"
            this.txt += "<img src='"+this.t[j]+".gif'></a></td></tr>"
         } else   { this.txt += "<a href='"+tmp+this.t[j]+"' class="+m_item+" a>" +this.t[j]+ "</a></td></tr>" }
    }

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function xml_update(loc) { 
 tmp = my_xml[loc]

 // Add URL
 if ( tmp.lastIndexOf("http") < 0 ) {
    tmp =  baseURL.substr(0,baseURL.lastIndexOf("index.html"))
    tmp += input_text[choice_ind]+'/'+ my_xml[loc]
 }
    
 if ( tmp.lastIndexOf(".") < 0 ) { tmp += '.xml' }
 
 // Expand [] arguments
 while ( tmp.search("\\[") != -1 ) {
     i1= tmp.search("\\[")
     i2= tmp.search("\\]")
     i3= tmp.substr(i1+1,i2-i1-1)
    tmp = tmp.replace(tmp.substr(i1,i2-i1+1),v[i3][pos[i3]])
 } ;
 parent.frames['Pic'].location = tmp
}
//--------------------------------------------------------------------------------------------------------------------
function con_update(loc) { 
 con_ind = loc
 con_menu.p = loc

 if (my_con[con_ind] == 'All' ) {
  for ( i=0 ; i < mlist.length ; i++ ) { 
    mlist[i].active = ( t[i].length > 1 )
  }
 } else {
  for ( i=0 ; i < mlist.length ; i++ ) { 
    mlist[i].active = false
  }
 } 

 // Expand [] arguments
 tmp = my_con[loc]

 while ( tmp.search("\\[") != -1 ) {
     i1= tmp.search("\\[")
     i2= tmp.search("\\]")
     i3= tmp.substr(i1+1,i2-i1-1)
     if ( i3.search("\\,") != -1 ) {
       hej = i3.split(",")
       i3 = hej[0]
       for (jk = 0; jk <= mlist[i3].t.length-1; jk++) { 
         delete mlist[i3].t[jk]
         delete mlist[i3].v[jk]
       }
       mlist[i3].t.length = 0
       mlist[i3].v.length = 0

       for ( i=1 ; i < hej.length ; i++ ) { 
         mlist[i3].t[i-1] = t[i3][hej[i]]
         mlist[i3].v[i-1] = v[i3][hej[i]]
       }
       pos[i3] = 0
     } else {
       // Restore the old full array value
       switch(my_con_sort[i3]) {
       
       case 'v':
        for (jk = 0; jk <= v[i3].length-1; jk++) { 
         mlist[i3].v[jk] = v[i3][jk]
        }
        mlist[i3].v.sort()
        for (jk = 0; jk <= t[i3].length-1; jk++) { 
         jl=v[i3].indexOf(mlist[i3].v[jk]); 
         mlist[i3].t[jk] = t[i3][jl]
        }
        break ;

       case 't':
        for (jk = 0; jk <= t[i3].length-1; jk++) { 
         mlist[i3].t[jk] = t[i3][jk]
        }
        mlist[i3].t.sort()
        for (jk = 0; jk <= v[i3].length-1; jk++) { 
         jl=t[i3].indexOf(mlist[i3].t[jk]); 
         mlist[i3].v[jk] = v[i3][jl]
        }
        break ;

       default :
        for (jk = 0; jk <= v[i3].length-1; jk++) { 
         mlist[i3].v[jk] = v[i3][jk]
         mlist[i3].t[jk] = t[i3][jk]
        }
        break ;

       } 
     }
     mlist[i3].active = ( mlist[i3].t.length > 1 )
     tmp = tmp.replace(tmp.substr(i1,i2-i1+1),'#')
 } ;

 getFig(0,pos[0])
 if ( ! on_start ) { update_all('List','list','Top_left','top_left','Top','top','Pic','dp') }

}
//--------------------------------------------------------------------------------------------------------------------
function con_body( ) { 

 this.attrib = "bgcolor='#ffffff' rowspan='1' colspan='6'"
 this.menu_style = menu_style
 this.txt        = ""

 this.act = "con_update(selectedIndex)"
 this.txt += sel_top(6,this.act)
 this.p = con_ind

 for (jj = 0; jj < this.v.length; jj++) {
    if ( this.p == jj ) { seltext = 'selected' } else { seltext ='' }
    this.txt += "<option " +seltext+" class="+m_item+">"+this.t[jj]+"</option>"
 }

 this.txt += "</select></td></tr>"

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function xml_body( ) { 

 this.attrib = "bgcolor='#ffffff' rowspan='1' colspan='6'"
 this.menu_style = menu_style
 this.txt        = ""

 this.act = "xml_update(selectedIndex)"
 this.txt += sel_top(6,this.act)

 for (jj = 0; jj < this.v.length; jj++) {
    if ( this.p == jj ) { seltext = 'selected' } else { seltext ='' }
    this.txt += "<option " +seltext+" class="+m_item+">"+this.t[jj]+"</option>"
 }

 this.txt += "</select></td></tr>"

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function info_update(loc) { 
 tmp = my_info[loc]

 // Add URL
 if ( tmp.lastIndexOf("http") < 0 ) {
    tmp =  baseURL.substr(0,baseURL.lastIndexOf("index.html"))
    tmp += input_text[choice_ind]+'/'+ my_info[loc]
 }
    
 if ( tmp.lastIndexOf(".") < 0 ) { tmp += '.xml' }
 
 // Expand [] arguments
 while ( tmp.search("\\[") != -1 ) {
     i1= tmp.search("\\[")
     i2= tmp.search("\\]")
     i3= tmp.substr(i1+1,i2-i1-1)
    tmp = tmp.replace(tmp.substr(i1,i2-i1+1),v[i3][pos[i3]])
 } ;
 parent.frames['Pic'].location = tmp
}
//--------------------------------------------------------------------------------------------------------------------
function info_body( ) { 

 this.attrib = "bgcolor='#ffffff' rowspan='1' colspan='6'"
 this.menu_style = menu_style
 this.txt        = ""

 this.act = "info_update(selectedIndex)"
 this.txt += sel_top(6,this.act)

 for (jj = 0; jj < this.v.length; jj++) {
    if ( this.p == jj ) { seltext = 'selected' } else { seltext ='' }
    this.txt += "<option " +seltext+" class="+m_item+">"+this.t[jj]+"</option>"
 }

 this.txt += "</select></td></tr>"

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function make_menu(loc) { 

 this.txt = ""
 re  = /l.+/ ;

 if ( this.ind >= 0 ) { this.p   = pos[this.ind] }
 if ( this.active && this.loc == loc )  {  

    this.txt = table_top()
    if ( sli_menu.active && sli_menu.p == this.ind ) 
         { this.txt += this.sli_head()     }
    else { this.txt += this.head(this.ind) }

     this.txt += this.body() + table_bot() 

    if ( this.loc.match(re) || this.loc == 'l' ) { this.txt += "<br>" }

 }

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function menu(ind,name,v,t,loc,typ,head,body,p,active) {

 this.ind = ind			// Identification and position indicator
 this.name = name 		// Name
 this.v = v 			// Values
 this.t = t 			// Text
 this.loc = loc 		// Location in window
 this.typ = typ 		// Type of menu
 this.head = head		// menu head function
 this.body = body		// menu body function
 this.p = p 			// Position
 this.active = active	        // Active flag
 this.flip   = false            // Allow flipping

 // Functions
 this.make_menu = make_menu
 this.my_alert  = my_alert
 this.sli_head  = sli_head
 this.expand = expand

 if ( this.loc == undefined ) {
    if ( this.v.length > 10 ) { this.loc = 't' } else { this.loc = 'l' }
 }

 if ( this.typ == undefined ) {
      if ( this.loc == 't' ) { this.typ = 1 } else { this.typ = 0 }
 }

 if ( this.loc == 't' ) { this.typ = 1 } 

   
}
//--------------------------------------------------------------------------------------------------------------------
function write_top_left(){

 this.txt = ""
 this.txt += top_menu.make_menu('l')

 return this.txt
}
//--------------------------------------------------------------------------------------------------------------------
function write_list(){

 this.txt = ""
 //this.txt += top_menu.make_menu('l')

 this.txt += con_menu.make_menu('lt')

 for ( i=0 ; i < mlist.length ; i++ ) {
     this.txt += mlist[order[i]].make_menu('l') 
 }

 this.txt += con_menu.make_menu('lb')

 if ( is_graphics ) { this.txt += hel_menu.make_menu('l') }
 this.txt += inf_menu.make_menu('l') 

 if ( ! (multi || sli_menu.active ) ) {  this.txt += dow_menu.make_menu('l') }
 if ( ! sli_menu.active ) { this.txt += cok_menu.make_menu('l') }

 this.txt += you_menu.make_menu('l') 
 if ( do_debug  ) { this.txt += deb_menu.make_menu('l') }
 if ( do_resize && is_graphics) { this.txt += res_menu.make_menu('l') }
 if ( rem_menu.active && is_graphics ) { this.txt += sen_menu.make_menu('l') }
 this.txt += xml_menu.make_menu('l')
 this.txt += info_menu.make_menu('l')
 if ( user != 'nada' ) { this.txt +="<b> Created by "+user+"</b> <br>" }

 list = this.txt

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function write_peak(){ this.txt += "<h2> &nbsp;&nbsp;&nbsp; "+title+"</h2>" }
//--------------------------------------------------------------------------------------------------------------------
function write_top(){

  if ( sli_menu.active ) {
     parent.document.title=title+" "+lang[pre_lan].slid+": "+sli_menu.name

  } else {

     if ( multi ) { 

        if ( mpos == -1 ) { parent.document.title=title+" Fig: "+lang[pre_lan].late +" "+lang[pre_lan].favo }
                     else { parent.document.title=title+" Fig: "+fig.txt   }

     } else { if ( ext != "html" ) { parent.document.title=title+" Fig: "+fig.txt }}

  }


 this.txt = "<table cellpadding='0' cellspacing='1' border='0' ><tbody><tr>"

 this.txt += con_menu.make_menu('t')
 for ( i=0 ; i < mlist.length ; i++ ) {
     this.txt += "<td>"
     this.txt += mlist[order[i]].make_menu('t') 
     this.txt += "</td>"
 }

 if ( do_remember && do_show_remember && is_graphics) { this.txt += "<td>" + rem_menu.make_menu('t') + "</td>" }
//if ( ! rem_menu.active ) { this.txt += "<td><h2> &nbsp;&nbsp;&nbsp; "+title+"</h2></td>" }

 this.txt += "</tr></tbody></table>"

 top = this.txt

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function fix_input_text(input_list){

 for ( i = 0 ; i < input_list.length ; i++ ) {
    if ( input_text[i] == undefined ) {
       hej = input_list[i].split("/")
       input_text[i] = hej[hej.length-1].substr(0,(hej[hej.length-1].length-3))
    }
 } 

 return input_text
}
//--------------------------------------------------------------------------------------------------------------------
function init() {

 //
 // The main WebgraF function
 // Here we define the different menus on display
 //

 // First check if we are deling with svg files
 re  = /svg$/ ;
 if ( ext.match(re) ) {
    do_slide  = false
    do_resize = false
 } 

 // Set text on the main drop menu
 input_text = fix_input_text(input_list)

 // If no title is set the path as title
 if ( title == "" ) { title = findProject(0) +"/" + input_text[choice_ind]  }

 
 // Define the menu that remembers what we have done
 rem_menu = new menu(-1,"rem_menu",[""],[""],'t',1,all_head,gen_body,0,false)
 rem_menu.pos = new Array()

 // Menu that handles save favorites ( as cookies )
 cok_menu = new menu(-2,lang[pre_lan].favo,[""],[""],'l',0,sim_head,gen_body,-1,false)

 // The slide show menu, I'm not sure it's really used anymore
 sli_menu = new menu(-3,"sli_menu" ,[""],[""],'l',0,sli_head,nada    ,-1,false)

 // The top left meny, i.e. the different entries under a project
 if (input_list.length <= 1 ) { body = nada } else { body = gen_body }
 top_menu = new menu(-4,findProject(1),input_list,input_text,'l',1,sim_head,body,choice_ind,true)

 // The help menu
 hel_menu = new menu(-6,lang[pre_lan].help,[""],[""],'l',0,sim_head,nada,0,( help != "" ))
 
 // The download menu
 dow_menu = new menu(-7,lang[pre_lan].downl,download,download,'l',0,sim_head,dow_body,0,( download.length > 0 ))

 // I have no idea?
 sus_menu = new menu(-8,lang[pre_lan].stpo,[""],[""],'s',1,sim_head,gen_body,0,true)
 sue_menu = new menu(-9,lang[pre_lan].enpo,[""],[""],'s',1,sim_head,gen_body,0,true)


 // Handling of menu if you allow manipulation
 you_menu = new menu(-10,lang[pre_lan].youm,[1,0,0,0,1],
            [lang[pre_lan].load,lang[pre_lan].save,lang[pre_lan].dele,lang[pre_lan].clea,lang[pre_lan].rest],
            'l',0,sim_head,you_body,-1,false)

 // The menu for the information text
 inf_menu = new menu(-11,lang[pre_lan].mifo,[""],[""],'l',0,sim_head,nada,0,( info != ""))

 // Menu to send an URL pointing to some specific plots
 sen_menu = new menu(-12,lang[pre_lan].send,[""],[""],'l',0,sim_head,nada,0,
                     ( do_show_remember && do_send && ! do_manip && ! do_flip && ! do_delete ))

 // Debug menu
 deb_menu = new menu(-13,'Debug'           ,[""],[""],'l',0,sim_head,nada,0, do_debug )

 // Resize menu
 res_menu = new menu(-14,lang[pre_lan].resi,[""],[""],'l',0,sim_head,nada,0, do_resize )


 // Handle of selected html links
 if ( my_xml_txt.length == 0 ) { my_xml_txt = my_xml }
 xml_menu = new menu(-15,my_xml_title,my_xml,my_xml_txt,'l',0,sim_head,xml_body,0, (my_xml.length > 0 ))

 // Generalized handling of name parsing
 if ( my_con.length == 0 ) { my_con = ['All'] }
 if ( my_con_txt.length == 0 ) { my_con_txt = my_con }
 con_menu = new menu(-17,my_con_title,my_con,my_con_txt,con_menu_loc,1,all_head,con_body,0, (my_con.length > 1 ))
 con_menu.expand()
 
 // This is another info menu, more generalized...
 if ( my_info_txt.length == 0 ) { my_info_txt = my_info }
 info_menu = new menu(-16,my_info_title,my_info,my_info_txt,'l',0,sim_head,info_body,0, (my_info.length > 0 ))


 // Loop through all user defined menus and defined them
 for ( i=0 ; i < mname.length ; i++ ) {

   if ( pos[i] == undefined ) { pos[i] = 0 }
   mname[i] = mname[i].substring(0,1).toUpperCase() +mname[i].substring(1).toLowerCase() 
   mlist[i] = new menu(i,mname[i],checkV(v[i]),checkV(t[i]),loc[i],type[i],all_head,gen_body,0,( t[i].length > 1 ))

   del_menu[i] = new Array()
   
 }

 // Set order if not defined by the user
 if ( order[0] == undefined ) { for (j = 0; j < mlist.length; j++) { order[j] = j } }

 
 // Clean mname for some reason?
 for (j =(mname.length-1); j > 0 ; j-- ) { delete mname[j] }
 mname.length = 1

 // Special names
 if ( spec_name[0] == undefined ) { 
    for ( i=0 ; i < mlist.length ; i++ ) {
       spec_name[i] = i
    }
 }

 // Special separator
 for ( i=0 ; i < mlist.length ; i++ ) {
    if ( msep[i] == undefined ) { msep[i] = sep }
 }

 // Make sure the configuration menu is activated
 if ( (my_con.length > 1 ) ) { con_update(0) }


 // Check if we have any cookies that should be loaded
 cok_menu.t.splice(0,1)
 cok_menu.v.splice(0,1)
 if ( ( do_remember && do_save ) || do_manip ) checkCookie()


 if ( ext != "html" ) { 

    //
    if ( mpos != -99 ){
       getFig(mpos,-1)
    } else {
       getFig(0,0)
    }
    adjust_rem(0,rem_menu.t.length,false)

    // Start at a postion given in the URL
    if ( inremlist != '#') {
       evalInput(inremlist)
       getFig(-1,-1)
    }

    // Start at a postion given in input.js
    if ( start_at != '#' && inremlist == '#' ) {
       inremlist = new String(start_at) 
       evalInput(inremlist)
       getFig(-1,-1)
    }

 }

 // Set title
 parent.document.title=title

 local_save = true

 // Load information about manipulated menus
 if ( you_menu.active && load_manip ) { manipulate_menu(-1) }

 load_manip = true
 on_start   = false
 
}
//-------------------------------------------------------------------------------------------
