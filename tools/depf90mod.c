/* 
   Program: depf90mod                                          
   Generation of dependencies for Fortran 90 source code with modules
   
   Author: Kristian S. Mogensen (ksm@dmi.dk)

   Compilation:
   cc depf90mod.c -o depf90mod 

   Usage:
   depf90mod [-E ext ] [-M moduledir] [-I includedir] sourcefiles
   where:
   -E : is allowed extensions (e.g. F90) for Fortran files containing modules
   -M : is a directory to search for modules
   -I : is a directory to search for include files
   sourcefiles : is a wildcard of input source files

   Example of usage:
   depf90mod -E F90 -M ../modules -I ../include 

   Notes:
   Dependencies are written into a file with name "dependencies.inc",
   which can be included in your makefile either manually or with a
   include statement

   Limitations:
   Other preprocessing directives (such as ifdef) are ignored.
   Space between option and arguments are mandatory

   Bugs: probably many.

   Tested on:
   Linux
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/types.h>
#include <dirent.h>
#include <libgen.h>

#define MAXMODULES 200000

struct module_node {
  char *modulename;
  char *moduledirname;
  char *modulefullfilename;
};

void process_file(char* filename,char * objectfilename,
		  char** moduledirs,int nomoduledirs,
		  char** includedirs,int noincludedirs,
		  char** exts,int noexts,
		  int verbose, FILE *fpout,
		  struct module_node modules[],int nomodules
		  );

void compress_modname(char* name) {

  /* Extract the module name only from string */

  int i,j;
  char *tmpname;

  tmpname=(char*)malloc(sizeof(char)*(strlen(name)+1));
  j=0;
  for (i=0;i<strlen(name);i++){
    if ((name[i]=='\n')||(name[i]==',')||(name[i]=='!')) break;
    if (name[i]!=' ') {
      tmpname[j]=name[i];
      tmpname[j]=tolower(tmpname[j]);
      j++;
    }
  }
  tmpname[j]=0;
  for(i=0;i<=j;i++) {
    name[i]=tmpname[i];
  }
  free(tmpname);
}

void compress_incname(char* name) {

  /* Extract the include filename only from string */

  int i,j,start;
  char delim;
  char *tmpname;

  tmpname=(char*)malloc(sizeof(char)*(strlen(name)+1));
  j=0;
  for (i=0;i<strlen(name);i++){
    if (name[i]=='\n') break;
    if (name[i]=='\"') {
      delim='\"';
      break;
    }
    if (name[i]=='\'') {
      delim='\'';
      break;
    }
    if (name[i]=='<') {
      delim='>';
      break;
    }
  }
  start=i+1;
  j=0;
  for(i=start;i<strlen(name);i++) {
    if (name[i]==delim) {
      tmpname[j]=0;
      break;
    } else {
      tmpname[j]=name[i];
      j++;
    }
  }
  for(i=0;i<=j;i++) {
    name[i]=tmpname[i];
  }
  free(tmpname);
}

void search_includedir(char* filename,char* objectfilename,
		       char* includefilename,
		       char** includedirs,int noincludedirs,
		       char** moduledirs,int nomoduledirs,
		       char** exts,int noexts,
		       int verbose,FILE *fpout,
		       struct module_node modules[],int nomodules) {

  /* Search include directories for includefilename */

  int i,found;
  struct stat statbuf;
  char *namebuf;

  namebuf=(char*)malloc(sizeof(char)*1000);
  found=0;
  for (i=0;i<noincludedirs;i++) {
    sprintf(namebuf,"%s/%s",includedirs[i],includefilename);
    if (stat(namebuf,&statbuf)==0){
      fprintf(fpout,"%s : %s\n",objectfilename,namebuf);
      found=1;
      break;
    }
  }
  if (found) {
    process_file(namebuf,objectfilename,
		 moduledirs,nomoduledirs,
		 includedirs,noincludedirs,
		 exts,noexts,
		 verbose,fpout,
		 modules,nomodules);
  }
  if (!found){
    printf("File %s included in %s not found.\n",
	   includefilename,filename);
  }
  free(namebuf);
}

void search_moduledir(char* filename,char *objectfilename,
		      char* modname,
		      char** moduledirs,int nomoduledirs,
		      char** exts,int noexts,
		      char** includedirs,int noincludedirs,
		      int verbose,
		      FILE *fpout,		  
		      struct module_node modules[],int nomodules) {

  /* Search modules directories for a file containing modname module */

  int i,j,found;
  char *tmpbuf;

  tmpbuf=(char*)malloc(sizeof(char)*256);
  found=0;
  for (i=0;i<nomodules;i++){
    if (strcmp(modules[i].modulename,modname)==0) {
      found=1;
      sprintf(tmpbuf,"%s/%s",modules[i].moduledirname,filename);
      if (strcmp(tmpbuf,modules[i].modulefullfilename)==0) {
      } else {
	strcpy(tmpbuf,modules[i].modulefullfilename);
	for (j=(strlen(tmpbuf)-1);j>0;j--) {
	  if (tmpbuf[j-1]=='.') {
	    tmpbuf[j]='o';
	    tmpbuf[j+1]='\0';
	    break;
	  }
	}
	fprintf(fpout,"%s : %s\n",objectfilename,tmpbuf);
      }
      break;
    }
  }
  
  if (!found){
    printf("Module %s in %s not found.\n",modname,filename);
  }

  free(tmpbuf);
}

scan_module_dirs(char ** moduledirs,int nomoduledirs,
		 char ** exts,int noexts,int verbose,
		 struct module_node modules[],int *nomodules) {
 
  /* Scan the module directories to build a list of modules */

  DIR *dir;
  struct dirent *direntry;
  char *mfilename; 
  char *tmpbuf;
  char *ext;
  char *line;
  char *fortmodname;
  int i,j,k,m,lcheckext,ismodule;
  FILE *fp;
  char *mbuffer;
  struct stat statbuf;
  char *checklow="module";
  char *checkupper="MODULE";

  *nomodules=0;
  mfilename=(char*)malloc(sizeof(char)*1000);
  tmpbuf=(char*)malloc(sizeof(char)*256);
  for (i=0;i<nomoduledirs;i++) {
    if (!(dir=opendir(moduledirs[i]))) continue;
    while(direntry=readdir(dir)){
      strcpy(tmpbuf,direntry->d_name);
      ext=tmpbuf;
      for(j=(strlen(tmpbuf)-1);j>0;j--) {
	if (tmpbuf[j-1]=='.') {
	  ext=&tmpbuf[j];
	  break;
	}
      }
      lcheckext=0;
      for(j=0;j<noexts;j++) {
	if (strcmp(ext,exts[j])==0) {
	  lcheckext=1;
	  break;
	}
      }
      if (lcheckext==1) {
	sprintf(mfilename,"%s/%s",moduledirs[i],tmpbuf);
	if (verbose) printf("Input module file = %s\n",mfilename);
	stat(mfilename,&statbuf);
	if (verbose) 
	  printf("Input module file size = %i\n",(int)statbuf.st_size);
	if ( (fp=fopen(mfilename,"r")) == NULL ) continue;
	if (statbuf.st_size <= 1) continue;
        mbuffer=(char*)malloc(sizeof(char)*(statbuf.st_size+1));
	fread(mbuffer,sizeof(char),statbuf.st_size,fp);
	mbuffer[statbuf.st_size]='\0';
	fclose(fp); 
	
	j=0;;
	while (line = (j==0) ? strtok(mbuffer,"\n") : strtok(NULL,"\n")) {
	  j++;
	  if (strlen(line)<6) continue;
	  k=0;
	  ismodule=0;
	  for(m=0;m<strlen(line);m++) {
	    if (line[m]!=' ') {
	      if ((line[m]!=checklow[k])&&(line[m]!=checkupper[k])) break;
	      k++;
	      if (k==6) {
		ismodule=1;
		break;
	      }
	    }
	  }

	  if (ismodule) {
	    k=0;
	    for(j=0;j<strlen(line);j++) {
	      if (line[j]!=' ') {
		tmpbuf[k]=tolower(line[j]);
		k++;
	      }
	      if (line[j]=='\n') break;
	      if (line[j]=='!') break;
	    }
	    tmpbuf[k]='\0'; 
	    fortmodname=&tmpbuf[6];
	    modules[*nomodules].modulename=
	      (char*)malloc(sizeof(char)*(strlen(fortmodname)+1));
	    modules[*nomodules].modulefullfilename=(char*)
	      malloc(sizeof(char)*(strlen(mfilename)+1));

	    modules[*nomodules].moduledirname=(char*)
	      malloc(sizeof(char)*(strlen(moduledirs[i])+1));
	    strcpy(modules[*nomodules].modulename,fortmodname);
	    strcpy(modules[*nomodules].modulefullfilename,mfilename);	
	    strcpy(modules[*nomodules].moduledirname,moduledirs[i]);
	    (*nomodules)++;
	    if ((*nomodules)>MAXMODULES) {
	      printf("Too many modules! Increase MAXMODULES in depf90mod.c\n");
	      exit(1);
	    }
	  }
	}
	free(mbuffer);
      }
    }
    closedir(dir);
  } 
  if (verbose) {
    for (i=0;i<*nomodules;i++){
      printf("Module %s is in %s\n",modules[i].modulename,
	     modules[i].modulefullfilename);
    }
  }
  return 0;
}

void process_file(char* filename,char * objectfilename,
		  char** moduledirs,int nomoduledirs,
		  char** includedirs,int noincludedirs,
		  char** exts,int noexts,
		  int verbose, FILE *fpout,
		  struct module_node modules[],int nomodules
		  ) {
  FILE *fp;
  char *buffer;
  char **lines;
  char **incnames;
  int *inclines;
  char **modnames;
  int *modlines;
  char first4[5];
  char first7[8];
  struct stat statbuf;
  int nlines;
  int nmod,ninc;
  int i,j;

  /* Open and read files */
  if (verbose) printf("Input file = %s\n",filename);
  if (stat(filename,&statbuf)!=0) {
    if (verbose) printf("stat failed\n");
    return;
  }
  if (verbose) printf("Input file size = %i\n",(int)statbuf.st_size);
  fp=fopen(filename,"r");
  buffer=(char*)malloc(sizeof(char)*(statbuf.st_size+1));
  fread(buffer,sizeof(char),statbuf.st_size,fp);
  buffer[statbuf.st_size]='\0';
  fclose(fp); 

  /* Decode file into lines and skip leading whitespaces */
  nlines=0;
  for(i=0;i<statbuf.st_size;i++) {
    if (buffer[i]=='\n') nlines++;
  }

  if (verbose) printf("Input file has %i lines\n",nlines);

  if (nlines>0) {
    lines=(char**)malloc(sizeof(char*)*nlines);
    lines[0]=&buffer[0];
    j=0;
    for(i=0;i<statbuf.st_size;i++) {
      if (buffer[i]=='\n') {
	buffer[i]='\0';
	j++;
	if (j<nlines) {
	  lines[j]=&buffer[i+1];
	}
      };
    }
    for (j=0;j<nlines;j++) {
      for (;lines[j][0]==' ';lines[j]++);
    }

    /* Loop over lines and count number of include files and modules */
    nmod=0;
    ninc=0;
    modlines=(int*)calloc(nlines,sizeof(int));
    inclines=(int*)calloc(nlines,sizeof(int));
    for(i=0;i<nlines;i++){
      if (strlen(lines[i])>strlen("#include")) {
	if (strncmp(lines[i],"#include",strlen("#include"))==0) {
	  inclines[ninc]=i;
	  ninc++;
	}
      }
      if (strlen(lines[i])>strlen("include")) {
	strncpy(first7,lines[i],7);
	first7[7]=0;
	if (strncmp(lines[i],"include",strlen("include"))==0) {
	  inclines[ninc]=i;
	  ninc++;
	}
      }
      if (strlen(lines[i])>strlen("use")) {
	strncpy(first4,lines[i],4);
	first4[4]=0;
	for(j=0;j<4;j++) first4[j]=tolower(first4[j]);
	if (strcmp(first4,"use ")==0){
	  modlines[nmod]=i;
	  nmod++;
	}
      }
    }
    if (verbose) printf("File has %i include files and %i use statements\n",
			ninc,nmod);
    
    /* Extract include filenames */
    incnames=(char**)malloc(sizeof(char*)*ninc);
    for (i=0;i<ninc;i++) {
      if (lines[inclines[i]][0]=='#'){
	incnames[i]=lines[inclines[i]]+8;
      } else {
	incnames[i]=lines[inclines[i]]+7;
      }
      compress_incname(incnames[i]);
    }
    if ((verbose)&&(ninc>0)) {
      printf("List of include files:\n");
      for (i=0;i<ninc;i++) {
	printf("%s\n",incnames[i]);
      }
    }
    
    /* Extract module names */
    modnames=(char**)malloc(sizeof(char*)*nmod);
    for (i=0;i<nmod;i++) {
      modnames[i]=lines[modlines[i]]+3;
      compress_modname(modnames[i]);
    }
    if ((verbose)&&(nmod>0)) {
      printf("List of modules:\n");
      for (i=0;i<nmod;i++) {
	printf("%s\n",modnames[i]);
      }
    }
    

    /* Search module directories for modules and print match */
    for (i=0;i<nmod;i++) {
      search_moduledir(filename,objectfilename,
		       modnames[i],moduledirs,nomoduledirs,
		       exts,noexts,
		       includedirs,noincludedirs,
		       verbose,fpout,
		       modules,nomodules);
    }
    
    /* Search include directories for include files and print match */
    for (i=0;i<ninc;i++) {
      search_includedir(filename,objectfilename,
			incnames[i],includedirs,noincludedirs,	
			moduledirs,nomoduledirs,
			exts,noexts,verbose,fpout,
			modules,nomodules);
    }

    /* Cleanup */
    free(modnames);
    free(incnames);
    free(modlines);
    free(inclines);
    free(lines);
  }
  free(buffer);
}

void print_usage() {
  printf("Usage: depf90mod [-E <mod source ext. e.g. F90> ] [-M moduledir] [-I includedir] sourcefiles\n");
  printf("Multiple -E, -M and -I options are allowed\n");
}


int main(int argc,char*argv[])
{

  /* Input source files */
  char **inputfiles;
  static int noinputfiles=0;

  /* Module directories */
  char **moduledirs;
  static int nomoduledirs=0;

  /* Include directories */
  char **includedirs;
  static int noincludedirs=0;

  /* Include directories */
  char **exts;
  static int noexts=0;

  /* More debug output */
  static int verbose=0;

  int *isfilename;
  int *ismoduledir;
  int *isincludedir;
  int *isexts;

  /* Array holding the names of the modules */
  struct module_node modules[MAXMODULES];
  int nomodules;

  int i,j,k,l,m;
  FILE* fpout;
  char* objectfilename;
  
  /* Check for work to be done */
  if (argc<=1) {
    print_usage();
    return 1;
  }

  /* Check and decode options */
  isfilename=(int*)calloc(argc,sizeof(int));
  ismoduledir=(int*)calloc(argc,sizeof(int));
  isincludedir=(int*)calloc(argc,sizeof(int));
  isexts=(int*)calloc(argc,sizeof(int));
  for (i=1;i<argc;i++){
    if (argv[i][0]=='-') {
      if (strlen(argv[i])!=2) {
	printf("Unknown option: %s\n",argv[i]);
	print_usage();
	return 1;
      }
      if (i<(argc-1)) {
	switch (argv[i][1]) {
	case 'M': 
	  ismoduledir[i+1]=1;
	  nomoduledirs++;
	  i++;
	  break;
	case 'I':
	  isincludedir[i+1]=1;
	  noincludedirs++;
	  i++;
	  break;
	case 'E':
	  isexts[i+1]=1;
	  noexts++;
	  i++;
	  break;
	case 'V':
	  verbose=1;
	  break;
	default:
	  printf("Unknown option: %s\n",argv[i]);
	  print_usage();
	  return 1;
	}
      } else {
	printf("Missing value to option: %s\n",argv[i]);
	print_usage();
	return 1;
      }
    } else {
      isfilename[i]=1;
      noinputfiles++;
    }
  }

  /* Set pointers to filenames and directories*/
  inputfiles=(char**)malloc(sizeof(char*)*noinputfiles);
  moduledirs=(char**)malloc(sizeof(char*)*nomoduledirs);
  includedirs=(char**)malloc(sizeof(char*)*noincludedirs);
  exts=(char**)malloc(sizeof(char*)*noexts);
  j=0;
  k=0;
  l=0;
  m=0;
  for(i=0;i<argc;i++){
    if(isfilename[i]) {
      inputfiles[j]=argv[i];
      j++;
    }
    if(ismoduledir[i]) {
      moduledirs[k]=argv[i];
      k++;
    }
    if(isincludedir[i]) {
      includedirs[l]=argv[i];
      l++;
    }
    if(isexts[i]) {
      exts[m]=argv[i];
      m++;
    }
  }
  
  /* Print information */
  if (verbose) {
    printf("Module directories (in search order):\n");
    for(i=0;i<nomoduledirs;i++) {
      printf("%s\n",moduledirs[i]);
    }
    printf("Include directories (in search order):\n");
    for(i=0;i<noincludedirs;i++) {
      printf("%s\n",includedirs[i]);
    }
    printf("Possible extensions for files containing modules:\n");
    for(i=0;i<noexts;i++) {
      printf("%s\n",exts[i]);
    }
    printf("Source files:\n");
    for(i=0;i<noinputfiles;i++) {
      printf("%s\n",inputfiles[i]);
    }
  }

  scan_module_dirs(moduledirs,nomoduledirs,
		   exts,noexts,verbose,
		   modules,&nomodules);
  
  fpout=fopen("dependencies.inc","w");
  /* Main loop over filenames */
  for(i=0;i<noinputfiles;i++) {
    objectfilename=(char*)malloc((strlen(inputfiles[i])+10)*sizeof(char));
    strcpy(objectfilename,inputfiles[i]);
    for (j=(strlen(objectfilename)-1);j>0;j--) {
      if (objectfilename[j-1]=='.') {
	objectfilename[j]='o';
	objectfilename[j+1]='\0';
	break;
      }
    }
    /* Print default dependency */
    fprintf(fpout,"%s : %s\n",basename(objectfilename),inputfiles[i]);
    process_file(inputfiles[i],basename(objectfilename),
		 moduledirs,nomoduledirs,
		 includedirs,noincludedirs,
		 exts,noexts,
		 verbose,fpout,
		 modules,nomodules);
    free(objectfilename);
  }
  fclose(fpout);
  return 0;
}
