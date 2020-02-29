# Install 
 

 - Set your WebgraF main directory in `WebgraF/bin/WebgraF.pl` where :
 - Tell WebgraF where you have installed it by exporting the environment variable
   `WEBGRAF_BASE = "$SOME_FULL_PATH/WebgraF"`

 - Add `$SOME_FULL_PATH/WebgraF/bin/WebgraF` to you path

 Open `$SOME_FULL_PATH/WebgraF/index.html` in you browser


 # Basic usage
 
```bash
 WebgraF -h will print :

 USAGE: WebgraF -p PROJECT -e ENTRY [ -f FILE ] [ -glrhxXInmatuci ]  

 -h Displays this help 
 -l List all projects and entrys 
 -b Work with a different base directory 
 -p PROJECT name 
 -e ENTRY name 
 -r Remove ENTRY or PROJECT 
 -n Rename PROJECT/ENTRY  
 -f FILE. Add WebgraF page definition file or directory 
    can be FILE.js,FILE.tar or directory 
 -g Get input.js from PROJECT/ENTRY 
 -x Export an PROJECT/ENTRY as a tar file
    This file can be opened as a stand alone web page
 -t Transport an PROJECT/ENTRY 
    Creates a XXX___transport.tar file
 -a Add a file extracted with -t 
 -I Install a new WebgraF webhome, to be used with -b 
 -i INFO html file linked to project or entry 
 -j Set host page for main page like: WebgraF -j https://hirlam.org 
 -m Message on PROJECT link 
 -c CLEANAGE Remove all ( but .html,.js) files older than CLEANAGE days from PROJECT/ENTRY 
 -u Add user name to an entry 
 ```

 `WebgraF -l` will list current projects

 `WebgraF` will print list + help

 `WebgraF -p PROJECT_1 -e RAOBCORE -g` will give you the file definition for `PROJECT_1/RAOBCORE`

 `WebgraF -p PROJECT_1 -i -g` will give you the project description `PROJECT_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -I "My WebgraF"` will install a new WebgraF home directory with title "My WebgraF"
 

Open `$FULL_PATH/NEW_WEBGRAF/index.html` in your browser


 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -f PROJECT_1_RAOBCORE.js` will create new `project/entry PROJECT_1/ENTRY_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -i "http://www.smhi.se"`  will add a new description on `PROJECT_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -f fig.tar` will new plots to `PROJECT_1/ENTRY_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -x` will create a tar file with your plots and definition for `ENTRY_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1            -x` will create a tar file with your plots and definition for `PROJECT_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -c 0` will list all png/gif files in `ENTRY_1`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -c 10` will remove alla png/gif older than 10 days

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -m "Some info"` will add some text to the main page

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -e ENTRY_1 -n ENTRY_2`  will rename `PROJECT_1/ENTRY_1` to `PROJECT_1/ENTRY_2`

`WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_1 -n PROJECT_2`  will rename `PROJECT_1` to `PROJECT_2`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_2 -e ENTRY_2 -r` will remove entry `ENTRY_2`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_2 -r` will remove project `PROJECT_2`

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_2 -t` will create a `PROJECT2___transport.tar` file

 `WebgraF -b $FULL_PATH/NEW_WEBGRAF -p PROJECT_3 -a PROJECT2___transport.tar file` will create a new project using the tar file as input


 These are the most (un)useful commands in WebgraF


 For a description of the options in the entry definition javascript file click INFO on the main page 
 and then `FILE.js` in the description.



 Good Luck!