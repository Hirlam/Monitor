// --------------------------------------------------------
function slideDir(a) { slide_dir = a}
// --------------------------------------------------------
function slidepoint(ind,p) {

 if ( ind == -8 && p > slide_ep ) { return }
 if ( ind == -9 && p < slide_sp ) { return }

 if ( ind == -8 ) { if (p > slide_sp ) { slide_sp = p ; return } else { slide_sp = p }}
 if ( ind == -9 ) { if (p < slide_ep ) { slide_ep = p ; return } else { slide_ep = p }}

 if ( slide_sp < sus_menu.p || slide_ep > sue_menu.p )  { slideShow(sli_menu.p) }

}
// --------------------------------------------------------
function slideSpeed(a) { speed = Math.max((speed + a),100) }
// --------------------------------------------------------
function slideShow(ind) {

 if ( ind == -99 ) {

    slide_ep = -1
    slide_sp = -1

    sli_menu.p = ind
    sli_menu.active = false

    if (multi ) { multi = false ; getFig(mpos,-1) } else { pre_getFig(pos)}
    return

 }

 clean(sli_menu.t)
 clean(sli_menu.v)

 if ( sli_menu.active ) {
    slide_ep   = -1
    slide_sp   = -1
    sli_menu.p = ind
 }

 if ( ind == -1 ) {

       sli_menu.name = lang[pre_lan].favo
       p = new Array()

       sli_menu.t.splice(0) 
       sli_menu.v.splice(0) 

       for ( i=0 ; i < rem_menu.v.length ; i++ ) {

          for ( ii=0 ; ii < pos.length ; ii++ ) { p[ii] = rem_menu.pos[i][ii] }
          tmp    = new figName(p)
          sli_menu.t[i] = tmp.src
          sli_menu.v[i] = tmp.src

       }
 }

 if ( ind >= 0 ) {


    sli_menu.name = mlist[ind].name
    fig = new Array()
    p   = new Array()
    for ( i=0 ; i < pos.length ; i++ ) { p[i] = pos[i] }

    for ( i=0 ; i < mlist[ind].t.length ; i++ ) {
   
       p[ind] = i 
       tmp    = new figName(p)
       sli_menu.t[i] = tmp.src
       sli_menu.v[i] = tmp.src
    }

 }

 if ( slide_sp < 0 ) {
    slide_sp = 0
    slide_ep = sli_menu.t.length - 1
 }

 sli_menu.active = true
 sli_menu.p      = ind

 update_all('Pic','slide','List','list','Top','top')

}
// --------------------------------------------------------
function sli_head( ) { 

 this.txt = ""
 this.txt += dirFig('left.gif' ,'slideDir(-1)'    ,lang[pre_lan].back)
 this.txt += dirFig('up.gif'   ,'slideSpeed(-250)',lang[pre_lan].fast)
 this.txt += dirFig('stop.gif' ,'slideShow(-99)'  ,lang[pre_lan].stop)
 this.txt += dirFig('down.gif' ,'slideSpeed( 250)',lang[pre_lan].slow)
 this.txt += dirFig('right.gif','slideDir(1)'     ,lang[pre_lan].forw)
 this.txt += "</tr>" 
 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function write_slide(){

 if ( sli_menu.p == -1 ) {
    sus_menu.t = rem_menu.t
    sus_menu.v = rem_menu.t
    sue_menu.t = rem_menu.t
    sue_menu.v = rem_menu.t 
 } else {
    sus_menu.t = mlist[sli_menu.p].t
    sus_menu.v = mlist[sli_menu.p].t
    sue_menu.t = mlist[sli_menu.p].t
    sue_menu.v = mlist[sli_menu.p].t
 }

 sus_menu.p = slide_sp
 sue_menu.p = slide_ep

 this.txt = set_backgc()

 if ( ! hide_help ) { this.txt += "<p>"+help+"</p>" }

 if ( sus_menu.t.length > 3 && do_subset ) {

    this.txt += "<table cellpadding='0' cellspacing='1' border='0' ><tbody><tr>"
    this.txt += "<td>"
    this.txt += sus_menu.make_menu('s')
    this.txt += sue_menu.make_menu('s')
    this.txt += "</td>"
    this.txt += "</tr></tbody></table><br>"

 }


 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
