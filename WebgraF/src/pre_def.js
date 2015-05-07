// User dependent variables
if ( window.screen.availWidth > 1400 ) {
    m_item = 'menuitem'
    m_head = 'menuheading'
    m_curr = 'menucurrent'
    m_grey = 'menugrey'
    var twidth 		= '160'
} else {
    m_item = 's_menuitem'
    m_head = 's_menuheading'
    m_curr = 's_menucurrent'
    m_grey = 's_menugrey'
    var twidth 		= '150'
}

// Main menu arrays and variables

var del_menu 	= new Array()
var mlist 	= new Array()
var pos   	= new Array()
var input_list  = new Array() ;
var input_text  = new Array() ;
var my_xml      = new Array() ;
var my_con      = new Array() ;
var my_xml_txt  = new Array() ;
var my_con_txt  = new Array() ;
var my_con_sort = new Array() ;
var con_menu_loc = 'lb'
var my_xml_title = 'Statistics'
var my_con_title = 'View' ;
var my_info      = new Array() ;
var my_info_txt  = new Array() ;
var my_info_title= 'Model info'
var multi  	= false
var mpos   	= -99
var con_ind     = 0
var save_txt 	= ""
var speed 	= 500
var slide_dir 	= 1
var choice_ind  = 0
var hide_help   = true
var slide_sp    = -1
var slide_ep    = -1
var local_save  = true
var gone_home   = false
var del_menu_name = ""
var load_manip 	= true
var baseURL 	=""
var inremlist 	= new String("#")
var start_at 	= new String("#")
var list 	="LIST"
var top 	="TOP"
var on_start 	= true
var wdt         = 0
var hgt         = 0
var dp_src      = "nada"

// User provided 
var v      	= new Array()
var t      	= new Array()
var mname  	= new Array()
var loc    	= new Array()  
var type   	= new Array() 
var download 	= new Array() 
var spec_name	= new Array() 
var order    	= new Array() 
var msep    	= new Array() 
var title  	= ""
var framec              = '#6699ff'
var backgc              = '#ffffcc'
var ext    	= 'gif'
var sep    	= '_'
var pdir   	= ''
var help   	= ""
var info   	= ""
var view_limit  = 50
var do_remember = false
var do_show_remember = false
var do_save 	= true
var do_flip	= false
var do_delete	= false
var do_subset 	= false
var do_manip  	= false
var do_send  	= false
var do_debug  	= false
var do_slide  	= true
var info_start  = false
var user        = "nada"
var size_fig    = 1.0
var do_resize   = true
var host_page   = undefined
var menu_type_0_maxlen = 10

// Language

var lang = new Array( )

function language_def(home,favo,late,save,remo,clea,welc,
              slid,up,down,forw,back,fast,slow,stop,help,
              downl,ps,pdf,stpo,enpo,flip,load,youm,dele,
              rest,mifo,send,mhed,larg,smal,orig,resi,
              saco,cltm,fltm,rtpo) {

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
this.mifo  = mifo
this.send  = send
this.mhed  = mhed
this.larg  = larg
this.smal  = smal
this.orig  = orig
this.resi  = resi
this.saco  = saco
this.cltm  = cltm
this.fltm  = fltm
this.rtpo  = rtpo
}

var pre_lan = 0
var months  = new Array()
var seasons = new Array()

// English
lang[0] = new language_def("WebgraF","Favorites","All","Save","DEL","Clear","Choices",
                           "Slideshow","Up","Down","Forward","Backward","Faster","Slower",
                           "Stop","Help","Download","Postscript","PDF","Start point","End point",
                            "FLIP","Load","Your Menu","Delete","Restore","Info","Send",
                            "Show all","Larger","Smaller","Original","Resize",
                            "Save as a cookie","Clear this menu","Flip this menu","Remove this item")

months[0] = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'] 
seasons[0] = ['DJF','MAM','JJA','SON']


// Swedish
lang[1] = new language_def("WebgraF","Favoriter","Alla","Spara","DEL","Rensa","Val",
                           "Bildspel","Upp","Ned","Framåt","Bakåt","Snabbare","Långsammare",
                           "Stopp","Hjälp","Ladda ned","Postscript","PDF","Startpunkt","Ändpunkt",
                            "VÄND","Ladda","Din Meny","Ta bort","Återställ","Info","Skicka",
                            "Visa alla","Större","Mindre","Original","Ändra storlek",
                            "Spara som en kaka","Rensa denna meny","Vänd denna meny","Tag bort denna post")

months[1] = ['Jan','Feb','Mar','Apr','Maj','Jun','Jul','Aug','Sep','Okt','Nov','Dec']
seasons[1] = ['DJF','MAM','JJA','SON']
