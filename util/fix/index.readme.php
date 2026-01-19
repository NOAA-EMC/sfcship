<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

date_default_timezone_set('UTC');
?>

<!doctype html>
<html>
<head>
<link rel="schema.DC" href="http://purl.org/dc/elements/1.1/">
<!-- TemplateBeginEditable name="doctitle" -->
<title>NCO About Surface Marine Monthly Statistics</title>
<!-- TemplateEndEditable -->
<!-- TemplateBeginEditable name="head" -->
<meta name="DC.title" content="NCO About Surface Marine Monthly Statistics">
<meta name="DC.description" content="NCO About Surf. Mar. Monthly Stats">
<meta name="DC.keywords" content="NCO Operational Monitoring Branch, NWS Central Operations, NCEP Central Operations, Central Operations, National Centers for Environmental Prediction, National Weather Service, About Surface Marine Monthly Statistics, Surface Marine Monthly Statistics">
<meta name="DC.date.created" content="2021-02-11">
<meta name="DC.date.reviewed" content="2022-06-23">
<!-- TemplateEndEditable -->
<meta name="DC.creator" content="NWS Central Operations">
<meta name="DC.language" scheme="DCTERMS.RFC1766" content="EN-US">
<meta name="DC.publisher" content="NWS Central Operations">
<meta name="DC.contributor" content="NWS Central Operations">
<meta name="DC.rights" content="https://www.weather.gov/disclaimer.php">
<meta name="rating" content="General">
<meta name="robots" content="index,follow">
<link href="/css/nco_main_structural.css" rel="stylesheet" type="text/css" medial="all">
<link href="/css/nco_main_style.css" rel="stylesheet" type="text/css" media="all">
<!-- TemplateBeginEditable name="OtherScripts" -->
<style>
.content_centered {
  line-height: 1.25em;
}
</style>
<!-- TemplateEndEditable -->

<!-- Analytics - DAP-->
<?php virtual('/includes/nco_analytics.php');?>

</head>

<body>
<a name="About_Surface_Marine_Monthly_Statistics"></a> 
<?php virtual('/includes/nco_topheader.php'); ?>
<div class="center">
  <div class="content">
  <?php virtual('/includes/nco_topnav.php'); ?>
  </div>
  <div class="center-content">
    <?php virtual('/includes/topnews.php'); ?>
    <div class="full-width-border">
      <div class="partial-width-borderbottom">
        <div class="div-full">
	  <div class="div-full breadcrumb">
	    <div class='location-breadcrumb'> 
            <!-- TemplateBeginEditable name="breadcrumb" --> 
               <a href="https://www.weather.gov/">NWS</a> &gt; 
               <a href="https://www.nco.ncep.noaa.gov/">NCO</a> &gt; 
               <a href="https://www.nco.ncep.noaa.gov/omb/">OMB</a> &gt; 
               <a href="https://www.nco.ncep.noaa.gov/status">Ops Status</a> &gt; 
               <a href="https://www.nco.ncep.noaa.gov/omb/dataqc/">Upper Air & Surface Data QC</a> &gt; 
               About Surface Marine Monthly Statistics
            <!-- TemplateEndEditable --> </div>
          </div>
        </div>
        <div class="div-full"> </div>
        <div class="div-full cms-content" id="home_page_content">
          <div class="content_centered"> <!-- TemplateBeginEditable name="content" --> 
<!-- *******************CONTENT AREA ***************************-->


        <h1 style="margin-top:0;margin-bottom:.25em; text-align: center">About Surface Marine Monthly Statistics</h1>
        <center><hr></center>
        <div class="content">
	   <?php virtual('/omb/dataqc/smstats/readme/readme_body.html'); ?>
        </div>

        <div class="content-body"> 
        <center><small><a href="#About_Surface_Marine_Monthly_Statistics">Return to Top</a></small></center>


<!-- *******END of CONTENT AREA *********************************--> 
<!-- TemplateEndEditable --> <!-- TemplateBeginEditable name="Filetime" --> 
<!-- *********************************************************
Please change the filemtime to your file name so the date page date last updated is correct 
********************************************************************************** -->
            <div id="grayTime">
              <?php
              echo "<br>";
              echo "Page last modified: " . date( "d F Y.", filemtime( "index.php" ) );
              ?>
            </div>
            <!-- TemplateEndEditable --> </div>
        </div>
      </div>
    </div>
  </div>
  <div class="full-width-first">
    <div class="full-width-first communication-links">
      <div class="one-half-last nopad"> <span class="txt-rt myforecast-current">
        <?php virtual('/includes/rss.php'); ?>
        </span> </div>
    </div>
    <div style="clear:both;"></div>
  </div>
  <div style="clear:both;"></div>
</div>
<!-- end of <div class="center"> --> 

<!-- sitemap area --> 
<!-- legal footer area -->
<?php virtual('/includes/nco_legalfooter.php'); ?>
<!-- end of <div class="footer-legal"> -->
</body>
</html>
