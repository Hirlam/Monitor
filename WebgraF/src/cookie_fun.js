function rmCookie(){

   cok_menu.p = Math.max(0,cok_menu.p)
   cookiename =  cok_menu.t[cok_menu.p]+'_'+input_list[choice_ind]
   deleteCookie(cookiename)

   cok_menu.t.splice(cok_menu.p,1)
   cok_menu.v.splice(cok_menu.p,1)

   cok_menu.active = ( cok_menu.t.length > 0 )
   if ( cok_menu.t.length > 3 ) { cok_menu.typ = 1 } else { cok_menu.typ = 0 }

   cookiename = 'list_'+input_list[choice_ind]
   if ( cok_menu.active ) {
      clist = cok_menu.t.join(':') + '##' + del_menu_name
      document.cookie = cookiename+"="+escape(clist)+";expires="+expireDate(100);
      cok_menu.p = Math.min(cok_menu.t.length-1,cok_menu.p)
      cok_menu.p = Math.max(0,cok_menu.p)
   } else {
      if ( del_menu_name != "" ) {
         clist = '##' + del_menu_name
         document.cookie = cookiename+"="+escape(clist)+";expires="+expireDate(100);
      } else {
         deleteCookie(cookiename) 
      }
      cok_menu.p = -1
   }

   update_all('Top','top','List','list','Pic','dp')

}
//-----------------------------------------
function loadCookie(cookiename){

   cok_menu.p = cookiename
   cookiename = cok_menu.t[cok_menu.p]+'_'+input_list[choice_ind]

   adjust_rem(0,rem_menu.t.length,false)
   hamtaCookie(cookiename)
   getFig(-1,0)

}
//----------------------------------------------
function validate(vvvv){

   vvvv = vvvv.replace(/^ /g,'')
   sparaCookie(vvvv+'_'+input_list[choice_ind])

   add_this = true ;
   for ( i=0 ; i < cok_menu.t.length ; i++ ) {
      if ( cok_menu.t[i] == vvvv ) {  add_this = false };
   };

   if ( add_this ) { 
      cookiename   = 'list_'+input_list[choice_ind]
      cok_menu.t[cok_menu.t.length] = vvvv 
      cok_menu.v[cok_menu.v.length] = vvvv 
      tmp = cok_menu.t.join(':') + '##' + del_menu_name
      document.cookie = cookiename+"="+escape(tmp)+";expires="+expireDate(100);
   }

   cok_menu.p = -1
   cok_menu.active = ( cok_menu.t.length > 0 )
   if ( cok_menu.t.length > 3 ) { cok_menu.typ = 1 } else { cok_menu.typ = 0 }

   save_txt = ""

   update_all('Top','top','List','list')

}
//-------------------------------------------------
function SaveCookie(){

   if ( cok_menu.p >=0 ) {
      valtext = cok_menu.t[cok_menu.p]
   } else { 
      valtext = rem_menu.t[rem_menu.p]  
      hej = valtext.indexOf(";",0)
      if  ( hej > -1 ) { valtext = valtext.substr(0,hej)}
   }

   save_txt = "<p>" + "<form name='myForm' onsubmit='parent.validate(myEmail.value)'>"
   save_txt += "<input type='text'   value='"+valtext+"' name='myEmail'>"
   save_txt += "<input type='submit' value='Save'>"
   save_txt += "</p>" 

   update_all('Pic','dp')

}
//--------------------------------------------------
function expireDate(antDagar) { 
   var datum = new Date();
   datum.setTime(datum.getTime()+(1000*86400*antDagar)); 
   return datum.toGMTString(); 
}
//----------------------------------------------------
function sparaCookie(cookiename) {

   var hej = new Array()
   remlist = ""

   for ( i=0 ; i <  rem_menu.t.length  ; i++) { 

      for ( ii=0 ; ii <  mlist.length  ; ii++) { 
         if ( mlist[ii].flip ) { rem_menu.pos[i][ii] = mlist[ii].t.length - 1 - rem_menu.pos[i][ii]}
      }

      hej[i] = rem_menu.pos[i].join('_')

   }

   if (hej.length >= 1 ) {remlist = hej.join(':')}

   document.cookie = cookiename+"="+escape(remlist)+";expires="+expireDate(100);

}
//-------------------------------------------------------
function sokCookie(namn) { 
    var hittad = false;
    var start = 0;
    while (start <= document.cookie.length) {
      var slut = start + namn.length;
      if (document.cookie.substring(start, slut) == namn) {
        hittad = true;
        break;
      }
      start++;
    }

    if (hittad) {
      start= slut + 1;
      slut= document.cookie.indexOf(";",start);
      if (slut<start) slut= document.cookie.length;
      return unescape(document.cookie.substring(start, slut));
    } else { 
      return null
    }
}
//----------------------------------------------
function hamtaCookie(cookiename) {

    remlist = sokCookie(cookiename)
   
    if (remlist == null ) { return false }

      remlist = unescape(remlist)
      remlist = remlist.split(":")

    if (remlist == "" ) { return } 

    for (i=0; i<  remlist.length ; i++ ) {
         var p = remlist[i].split('_')
         for (ii=0; ii<  p.length ; ii++ ) { p[i] = Number(p[i]) }
         figName(p)
    }

    return true

}
//------------------------------------------------------
function deleteCookie(name) { document.cookie = name+"="+";expires="+expireDate(-100); }
//-------------------------------------------------------------
function checkCookie() {

      // Search for cookie
      you_menu.v[0] = 0
      cookiename = 'list_'+input_list[choice_ind]
      remlist = sokCookie(cookiename)
      if (remlist == null ) { return }
      remlist = unescape(remlist)
      if (remlist == ''   ) { return }
      hej = remlist.split("##")

      // Evaluate the manipulation menu
      if ( hej[1] != undefined )  {
         if ( hej[1].length > 0 )  {
            you_menu.active = do_manip
            you_menu.v[0] = 1
            you_menu.v[2] = 1
            you_menu.flip = true
         }
      }
      
      // Evaluate the cookie menu (favorites)
      if ( hej[0] == '' || ! do_save) { return }

      hej = hej[0].split(":")
      for (i=0;i<hej.length;i++){
         cok_menu.t[i]=hej[i] 
         cok_menu.v[i]=hej[i]
      }
      cok_menu.active = true
      if ( cok_menu.t.length > 3 ) { cok_menu.typ = 1 } else { cok_menu.typ = 0 }

}
