One idea with the HARMONIE verification packages is that it should be easy to share you results with others. This is where WebgraF comes in. It was originally written to mimic the ECMWF "chart" facility like [here](http://www.ecmwf.int/research/demeter/d/inspect/catalog/research/era/diagnostics/fluxes/HBV/). The ECMWF solution is a perl based server solution and needs some installation and WebgraF is a javascript running locally which makes it more portable. The idea with WebgraF is that each page is defined by a simple definition file which spans the space of the menu axes on the page. 

Examples :
 * [GLAMEPS](https://glameps.hirlam.org/forecasted) [Definition file](https://glameps.hirlam.org/forecasted/AccPcp3h.js)
 * [Daily maps](https://hirlam.org/portal/oprint/Charts/CHARTS/1_RCR_area/) [Definition file](https://hirlam.org/portal/oprint/Charts/CHARTS/1_RCR_area/Surface.js)


At the end of both of the scripts Run_verobs_surface/Run_verobs_temp there is a call to [`Create_ver_js.pl`](https://github.com/Hirlam/Monitor/tree/master/scr/Create_ver_js.pl) that builds the webpage depending on your configuration file. It is also possible to (re)generate the webpage directly by running:

```bash
Create_ver_js YOUR_CONFIG_FILE
```

The WebgraF page is controlled by the [`WebgraF` script](https://github.com/Hirlam/Monitor/tree/master/WebgraF/bin/WebgraF). It has commands to e.g. list, add, remove the content of a page. To start mastering your own page you first have to let the script know the location of the page by setting the environment variable `WEBGRAF_BASE` 

```bash
# in bash
export WEBGRAF_BASE=SOME_PATH/monitor/WebgraF
# or in tcsh
setenv WEBGRAF_BASE SOME_PATH/monitor/WebgraF
```

Now you can list the content of you page by

```bash
WebgraF/bin/WebgraF -l 
```

A more comprehensive list of commands can be found in the [README file](README_WebgraF). The rules and functions available for your definition file is found [here](https://github.com/Hirlam/Monitor/tree/master/WebgraF/src/input.html).

Two useful tools is the export and transport commands. Both creates an portable extraction of your verification page but in two different ways.

 * The `export.tar` file is a stand alone web page that you can untar anywhere and open in your browser.
 * The `transport.tar` file is suitable to add to an already existing WebgraF page by  `WebgraF -a TARFILE`

Both are accessible through the script [`Transport_ver`](https://github.com/Hirlam/Monitor/tree/master/scr/Transport_ver) which is used like

```bash
Transport_ver YOUR_CONFIG_FILE
```