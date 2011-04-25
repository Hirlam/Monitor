// --------------------------------------------------------
function manipulate_menu(manip_ii) {

 if  ( manip_ii == -1 ) {

      cookiename = 'list_'+input_list[choice_ind]
      remlist = sokCookie(cookiename)
      if (remlist == null ) { return }
      remlist = unescape(remlist)
      if (remlist == ''   ) { return }
      hej = remlist.split("##")
      hej = hej[1].split(":")

      for ( i=0 ; i < hej.length ; i++ ) {
          
         if ( hej[i] != '' ) {
            tmp = hej[i].split('_')
            for ( j=0 ; j < tmp.length ; j++ ) {
                adjustMenu(i,tmp[j],false)
               
            }
         }
         pos[i] = 0
      }
    
      if ( you_menu.flip ) { you_menu.v[0] = 0 }
    
 }

 if  ( manip_ii == 0 ) {
    load_manip = true
    goHome(choice_ind)
 }
 
 if  ( manip_ii == 1 ) {
    tmp = new Array() ;
    for ( i=0 ; i < del_menu.length ; i++ ) {
       tmp[i] = del_menu[i].join('_')
    }
   
    del_menu_name = tmp.join(':')
    cookiename   = 'list_'+input_list[choice_ind]
    tmp = cok_menu.t.join(':') + '##' + del_menu_name
    document.cookie = cookiename+"="+escape(tmp)+";expires="+expireDate(100);
    you_menu.flip = true
    you_menu.v[0] = 1
    you_menu.v[2] = 1
    you_menu.v[3] = 1
    update_all('List','list')
 }

 
 if  ( manip_ii == 2 || manip_ii == 3 ) {

    you_menu.flip = true
    del_menu_name = ""
    cookiename   = 'list_'+input_list[choice_ind]
    if ( cok_menu.t.length > 0 ) {
       tmp = cok_menu.t.join(':') + '##' 
       document.cookie = cookiename+"="+escape(tmp)+";expires="+expireDate(100);
    } else {
       deleteCookie(cookiename)
    }
    you_menu.v[0] = 0
    you_menu.v[2] = 0
    you_menu.v[3] = 0

    you_menu.active = ( you_menu.v[1] == 1 && do_manip)

    if ( manip_ii == 3 ) { goHome(choice_ind) } else { update_all('List','list') }

 }

 if  ( manip_ii == 4 ) {
    load_manip = false
    goHome(choice_ind)
 }
 
}
// --------------------------------------------------------
function flip(i) {

 if ( i >= 0 ) {

    mlist[i].t = mlist[i].t.reverse()
    mlist[i].v = mlist[i].v.reverse()

    mlist[i].flip = ( ! mlist[i].flip )

    adjust_rem(0,rem_menu.t.length,false)

    pos[i] = mlist[i].t.length - 1 - pos[i]

    local_save = true
    for ( ii = 0 ; ii < mlist.length ; ii++ ) {
       local_save = ( local_save && ! mlist[ii].flip )    
    }
    local_save = ( local_save && do_save )    
 
    if ( multi && mpos == i ) {
       multi = false 
       getFig(i,-1)
       return
    }

    getFig(i,pos[i])

 } else {

    if ( i == -1 ) { 

       rem_menu.t   = rem_menu.t.reverse()
       rem_menu.v   = rem_menu.v.reverse()
       rem_menu.pos = rem_menu.pos.reverse()

       rem_menu.flip = ( ! rem_menu.flip ) 

       if ( multi ) {
          multi = false 
          getFig(i,-1)
       }
    }

 }

}
// --------------------------------------------------------
function adjustMenu(i,j,flag) {

     do_save         = false
     cok_menu.active = false
   
     // Keep track of manipulation

     delj = del_menu[i].length
     del_menu[i][delj] = j
     you_menu.active = do_manip
     if ( you_menu.flip ) { you_menu.v[0] = 1 }
     you_menu.v[1] = 1
     you_menu.v[3] = 1


     // Remove item
     mlist[i].t.splice(j,1)
     mlist[i].v.splice(j,1)
       pos[i] = Math.max(0,j)
       pos[i] = Math.min((mlist[i].t.length-1),j)

     mlist[i].active = ( mlist[i].t.length > 1 )

     adjust_rem(0,rem_menu.t.length,false)

     if ( flag && multi && mpos == i ) {
          multi = false
          getFig(i,-1)
          return
     }

     if ( flag ) { getFig(i,pos[i]) }

}
// --------------------------------------------------------
