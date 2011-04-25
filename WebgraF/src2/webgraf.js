function expand_menu(ind) {

 if ( mlist[ind].typ == 0 ) {
    mlist[ind].typ = 1
 } else { mlist[ind].typ = 0 }
 update_all('List','list')

}
// --------------------------------------------------------
function get_src() { parent.frames['Pic'].location = "http://www.smhi.se/sgn0106/if/hirald/WebgraF/WebgraF.tar.gz" }
// --------------------------------------------------------
function get_info(ind) {
   if ( ind > -1 ) {
      tmp = mlist[ind].info_list
      if (  tmp == undefined || tmp == "undefined" ) { show_info() ; return }

      parent.frames['Pic'].location = tmp
   }
}
// --------------------------------------------------------
function getEntry(ind,selectedIndex){
   str = new String(window.location) ;
   str = str.substr(0,str.lastIndexOf("/"))
   tmp = mlist[ind].name
   if ( selectedIndex >= 0 ) {
      str += "/" + tmp + "/index.html?choice_ind="+ t[ind][selectedIndex]
   } else {
      str += "/" + tmp + "/index.html?choice_ind=0"
   }
   window.top.location = str
}
// --------------------------------------------------------
function evalURL() {
str = new String(window.location) ;
si = str.lastIndexOf("?")
if ( si > 0 ) {
   passing = str.substr(str.lastIndexOf("?")+1,str.length)
   eval(passing)
}

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
function cri(inc)
{
    inc += this.p
    if ( inc  ==  this.v.length ) { return 0     }
    if ( inc  == -1 ) { return ( this.v.length - 1) }
    return inc
}
// --------------------------------------------------------
function menu_style(j) {

 if ( this.p == j || multi && mpos == this.ind ) { myclass = m_item } else { myclass = m_item }
 return myclass

}
// --------------------------------------------------------
function update_all() {
   i = 0
   do {

      p = arguments[i+1]
      if( p.indexOf("html",0) < 0 && p.indexOf("http",0) < 0 ) { p += ".html" }
      parent.frames[arguments[i]].location = p
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
function dirFig(fg,action,ft)
{
   tfg = " Missing: "+ fg 
   return "<td><a href='javascript:parent." +action+ "' a><img alt='" +tfg+ "' title='" +ft+ "' src='" +fg+ "' border='0'></a></td>"
}
// --------------------------------------------------------
function dirTxt(tdstyle,text,action,style,ind,mytitle)
{
   if ( mytitle == undefined ) { mytitle = "" }
   if ( action == 'nada()' ) { return "<td "+tdstyle+"><div class='"+style+"' >"+text+"</div></td>" }
   else { 
      tmp =""
      tmp += "<td "+tdstyle+"><a href='javascript:parent." +action+ "'"
      if ( do_mouseinfo ) {
         if (ind >   -1 ) {tmp += " onMouseOver='parent.get_info("+ind+")'" }
         if (ind == -11 ) {tmp += " onMouseOver='parent.show_info()'" }
      } 
      tmp += " class='"+style+"' title='"+mytitle+"' a>" +text+ "</a></td>" 
      return tmp
   }

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

 this.txt += dirTxt('width=100',this.name,("getEntry("+this.ind+",-1)"),m_head,this.ind,lang[pre_lan].showp)
 if ( this.info_list != 'undefined' ) {
    this.txt += dirTxt('width=10',lang[pre_lan].info,("get_info("+this.ind+")"),m_head,this.ind,lang[pre_lan].info)
 } 
 this.txt += "</tr><tr>"

 if (this.typ == 0 ) { this.txt += "</tr>" }

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function sim_head() { 
 this.txt     = ""
 this.mytitle = ""

 switch(this.ind) {
 
    case -2:
    this.act = "rmCookie()"
    break

    case -4:
    this.act = "nada()"
    break

    case -6:
    this.act = "show_info()"
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
    this.act = "to_main()"
    break

    case -12:
    this.act = "get_src()"
    break

 }

 this.txt += dirTxt('width=100',this.name,this.act,m_head,this.ind,this.mytitle)
 this.txt += "</tr>" 

 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function show_info() { 

 if ( info == "undefined" || info == undefined ) {parent.frames['Pic'].location = "info.html" }
 else {parent.frames['Pic'].location = info }

}
//--------------------------------------------------------------------------------------------------------------------
function to_main() { window.top.location = host_page }
//--------------------------------------------------------------------------------------------------------------------
function my_alert(ind) { alert(this.name + this.p +" "+ this.active +" "+ this.v + "<br>" + this.t + "<BR>" ) }
//--------------------------------------------------------------------------------------------------------------------
function gen_body( ) { 

 this.attrib = "bgcolor='#ffffff' rowspan='1' colspan='6'"

 this.menu_style = menu_style
 this.txt        = ""
 this.mytitle    = ""

 if ( this.typ == 0 ) {

    for (j = 0; j < this.v.length; j++) {
       switch (this.ind) {
       case -2:
          this.act = "loadCookie("+j+")"
          break ;
       default:
          this.act = "getEntry("+this.ind+","+j+")"
          break ;
       }

       this.txt += "<tr>" + dirTxt(this.attrib,this.t[j],this.act,this.menu_style(j),this.ind,this.d[j]) + "</tr>"
    }

 } 

 if ( this.typ == 1 ) {

    switch (this.ind) {
    case -2:
       this.act = "loadCookie(selectedIndex)"
       break ;
    default:
       this.act = "getEntry("+this.ind+",selectedIndex)"
       break ;
    }

    this.txt += sel_top(6,this.act,0)

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
function make_menu(loc) { 

 this.txt = ""
 if ( this.ind >= 0 ) { this.p   = pos[this.ind] }
 if ( this.active && this.loc == loc )  {  

 this.txt = table_top()
 this.txt += this.head(this.ind) 
 this.txt += this.body() + table_bot() 

    if ( this.loc == 'l' ) { this.txt += "<br>" }
 }
 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function menu(ind,name,v,t,loc,typ,head,body,p,active,d,info_list) {

 this.ind = ind			// Identification and position indicator
 this.name = name 		// Name
 this.v = v 			// Values
 this.t = t 			// Text
 this.d = d 			// Description
 this.info_list = new String(info_list) // Project information link
 this.loc = loc 		// Location in window
 this.typ = typ 		// Type of menu
 this.head = head		// menu head function
 this.body = body		// menu body function
 this.p = p 			// Position
 this.active = active	// Active flag
 this.flip   = false

 this.make_menu = make_menu
 this.my_alert  = my_alert

 this.loc = 'l'

 if ( this.typ == undefined ) {
      if ( this.v.length > 3 ) { this.typ = 1 } else { this.typ = 0 }
 }

   
}
//--------------------------------------------------------------------------------------------------------------------
function write_list(){

 this.txt = ""
 // this.txt += top_menu.make_menu('l')

 this.txt += top_menu.make_menu('l') 
 this.txt += inf_menu.make_menu('l') 
 this.txt += src_menu.make_menu('l') 

 for ( i=0 ; i < mlist.length ; i++ ) {
     this.txt += mlist[order[i]].make_menu('l') 
 }


 return this.txt

}
//--------------------------------------------------------------------------------------------------------------------
function write_top(){

 parent.document.title=title
 
 this.txt = "<table cellpadding='0' cellspacing='1' border='0' ><tbody><tr>"
 this.txt += "<td><h2> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "+title+"</h2></td>" 
 this.txt += "</tr></tbody></table>"

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

 if ( title == "" ) { title = " WebgraF " }

 top_menu = new menu(-11,lang[pre_lan].back ,[""],[""],'l',0,sim_head,nada,0,(host_page !=undefined))
 inf_menu = new menu( -6,lang[pre_lan].info ,[""],[""],'l',0,sim_head,nada,0, true )
 src_menu = new menu(-12,lang[pre_lan].downl,[""],[""],'l',0,sim_head,nada,0, do_download )

 for ( i=0 ; i < mname.length ; i++ ) {

     pos[i] = 0
   if ( d[i] == undefined ) { d[i] = t[i] } ;
   mlist[i] = new menu(i,mname[i],v[i],t[i],loc[i],type[i],all_head,gen_body,0,( t[i].length > 0 ),d[i],info_list[i])

 }

 if ( order[0] == undefined ) { for (j = 0; j < mlist.length; j++) { order[j] = j } }

 for (j =(mname.length-1); j > 0 ; j-- ) { delete mname[j] }
 mname.length = 1

 parent.document.title=title

}
//-------------------------------------------------------------------------------------------
