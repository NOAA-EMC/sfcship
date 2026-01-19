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
<title>NCO ISS Surface Marine Monthly Statistics</title>
<!-- TemplateEndEditable -->
<!-- TemplateBeginEditable name="head" -->
<meta name="DC.title" content="NCO ISS Surface Marine Monthly Statistics">
<meta name="DC.description" content="NCO ISS Surf. Mar. Monthly Stats">
<meta name="DC.keywords" content="NCO Operational Monitoring Branch, NWS Central Operations, NCEP Central Operations, Central Operations, National Centers for Environmental Prediction, National Weather Service, ISS Surface Marine Monthly Statistics, Surface Marine Monthly Statistics">
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
  .collapsible {
     background-color: white;
     color: #ED7A08;
     cursor: pointer;
     width: 100%;
     border: none;
     text-align: center;
     outline: none;
     font-size: 12px; 
     font-weight: bold; 
  }
  .active, .collapsible: hover {
     background-color: white;
  }
  .collapsible:after {
     content: '\25BA';
     color: #ED7A08;
     font-weight: bold;
     margin-left: 5px;
   }
   .active:after {
      content: "\25BC";
   }
   .content_etc {
      padding: 0 12px; 
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.2s ease-out;
      background-color: white;
      white-space: nowrap;
   }
</style>

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
<a name="ISS_Surface_Marine_Monthly_Statistics"></a> 
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
               ISS Surface Marine Monthly Statistics
            <!-- TemplateEndEditable --> </div>
          </div>
        </div>
        <div class="div-full"> </div>
        <div class="div-full cms-content" id="home_page_content">
          <div class="content_centered"> <!-- TemplateBeginEditable name="content" --> 
<!-- *******************CONTENT AREA ***************************-->


        <center>
           <h1>International SeaKeepers Society Surface Marine Monthly Statistics - Mon YYYY</h1>
           <table>
              <tbody>
                 <tr>
                    <td><font color="black">Choose another month: </font></td>
                    <td>
                       <script type="text/javascript">
                          window.onload = function(){
                             location.href = document.getElementByID("archiveMenu").value;
                          }
                       </script>
                       <center>
                          <select id="archiveMenu" size="1" style="color: rgb(0,0,0); font-size; 9pt;" onchange="javascript:location.href = this.value;">
                                <option value="" selected="selected">--Select a Report--</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/">Latest</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/jan/">January</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/feb/">February</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/mar/">March</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/apr/">April</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/may/">May</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/jun/">June</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/jul/">July</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/aug/">August</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/sep/">September</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/oct/">October</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/nov/">November</option>
                                <option value="https://www.nco.ncep.noaa.gov/omb/dataqc/smstats/iss/dec/">December</option>
                             </select>
                          <br>
                          </form>
                    </td>
                 </tr>
              </tbody>
           </table><br>
        <hr width="100%">
        </center>
           <button class="collapsible">About Surface Marine Monthly Statistics</button>
        <div class="content_etc">
           <?php virtual('/omb/dataqc/smstats/readme/readme_body.html'); ?>
           <small><a href="#ISS_Surface_Marine_Monthly_Statistics">Return to Top</a></small>
        </div>
        <script>
           var coll = document.getElementsByClassName("collapsible");
           var i; 

           for (i = 0; i < coll.length; i++) {
              coll[i].addEventListener("click", function() {
                 this.classList.toggle("active");
                 var content = this.nextElementSibling;
                 if (content.style.maxHeight) {
                    content.style.maxHeight = null;
                 } else {
                    content.style.maxHeight = content.scrollHeight + "px";
                 }
              });
            }
        </script>
        <hr width="100%">

        <div class="content-body"> 
           <center>
           <pre>
              <?php include "surf_mar.iss.mon.stats.txt" ?>
           </pre>
           </center>
        </div>

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
