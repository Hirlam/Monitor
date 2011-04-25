// User dependent variables
if ( window.screen.availWidth > 1500 ) {
    m_item = 'menuitem'
    m_head = 'menuheading'
    m_curr = 'menucurrent'
    m_grey = 'menugrey'
    var twidth 		= '200'
} else {
    m_item = 's_menuitem'
    m_head = 's_menuheading'
    m_curr = 's_menucurrent'
    m_grey = 's_menugrey'
    var twidth 		= '200'
}

// Random

   project = 'nada'
   entry   = 'nada'
   fig     = 'nada'

// Main menu arrays and variables

var del_menu 	= new Array()
var mlist 		= new Array()
var pos   		= new Array()
var input_list  = new Array() ;
var input_text  = new Array() ;
var multi  		= false
var mpos   		= -99
var save_txt 	= ""
var speed 		= 1600
var slide_dir 	= 1
var choice_ind  = 0
var hide_help   = true
var slide_sp    = -1
var slide_ep    = -1
var local_save  = true
var gone_home   = false
var del_menu_name = ""
var load_manip = true

// User provided 
var v      		= new Array()
var t      		= new Array()
var d      		= new Array()
var info_list           = new Array()
var mname  		= new Array()
var loc    		= new Array()  
var type   		= new Array() 
var download 	= new Array() 
var spec_name	= new Array() 
var order    	= new Array() 
var title  		= ""
var framec 		= '#6699ff'
var backgc 		= '#ffffcc'
backgc="lightgrey"
framec="darkyellow"
var ext    		= 'gif'
var sep    		= '_'
var pdir   		= ''
var help   		= ""
var info   		= "info.html"
var view_limit  = 50
var do_remember = true
var do_save 	= true
var do_flip	= false
var do_delete	= false
var do_subset 	= false
var do_manip  	= false
var do_download = false
var do_expand   = false
var do_mouseinfo = false
var host_page = undefined

// Language

var lang = new Array( )

function language_def(home,favo,late,save,remo,clea,welc,
              slid,up,down,forw,back,fast,slow,stop,help,
              downl,ps,pdf,stpo,enpo,flip,load,youm,dele,
              rest,back,info,showp) {

this.home = home
this.favo = favo
this.late = late
this.save = save
this.remo = remo
this.clea = clea
this.welc = welc
this.slid = slid
this.up   = up
this.down = down
this.forw = forw
this.back = back
this.fast = fast
this.slow = slow
this.stop = stop
this.help = help
this.downl = downl
this.ps   = ps
this.pdf  = pdf
this.stpo  = stpo
this.enpo  = enpo
this.flip  = flip
this.load  = load
this.youm  = youm
this.dele  = dele
this.rest  = rest
this.back  = back
this.info  = info
this.showp = showp

}

var pre_lan = 0

// English
lang[0] = new language_def("WebgraF","Favorites","All","Save","DEL","Clear","Choices",
                           "Slideshow","Up","Down","Forward","Backward","Faster","Slower",
                           "Stop","Help","Download","Postscript","PDF","Start point","End point",
                            "FLIP","Load","Your Menu","Delete","Restore","Back to main page","Info",
                           "Show this project")
// Swedish
lang[1] = new language_def("WebgraF","Favoriter","Alla","Spara","DEL","Rensa","Val",
                           "Bildspel","Upp","Ned","Framåt","Bakåt","Snabbare","Långsammare",
                           "Stopp","Hjälp","Ladda ned","Postscript","PDF","Startpunkt","Ändpunkt",
                           "VÄND","Ladda","Din Meny","Ta bort","Återställ","Tillbaka","Info",
                           "Visa detta projekt")
