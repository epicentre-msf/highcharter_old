HTMLWidgets.widget({

  name: 'highchart',

  type: 'output',

  factory: function(el, width, height) {

    var chart;

    return {

      initialize: function(el, width, height) {

        return {
          // TODO: add instance fields as required
        };
      },

      renderValue: function(x) {
        
        if(x.debug) {
          window.xclone = JSON.parse(JSON.stringify(x));
          window.elclone = $(el);
          console.log(el);
          console.log("hc_opts", x.hc_opts);
          console.log("theme", x.theme);
          console.log("conf_opts", x.conf_opts);
        }

        if(x.fonts !== undefined) {
          
          x.fonts = ((typeof(x.fonts) == "string") ? [x.fonts] : x.fonts);
        
          x.fonts.forEach(function(s){
            /* http://stackoverflow.com/questions/4724606 */
            var urlfont = 'https://fonts.googleapis.com/css?family=' + s;
            if (!$("link[href='" + urlfont + "']").length) {
              $('<link href="' + urlfont + '" rel="stylesheet" type="text/css">').appendTo("head");
            }
            
          });
          
        }
        
        ResetHighchartsOptions();
        
        if(x.theme !== null) {
          
          if(x.debug) console.log("adding THEME");
          
          Highcharts.setOptions(x.theme);
          
        }
        
        if((x.theme && x.theme.chart.divBackgroundImage !== undefined) |
             (x.hc_opts.chart  && x.hc_opts.chart.divBackgroundImage !== undefined)) {
               
          if(x.debug) console.log("adding BackgroundImage");     
               
          var bkgrnd = x.theme.chart.divBackgroundImage || x.hc_opts.chart.divBackgroundImage;
          
          Highcharts.wrap(Highcharts.Chart.prototype, "getContainer", function (proceed) {
            
            proceed.call(this);
            
            $("#" + el.id).css("background-image", "url(" + bkgrnd + ")");
            $("#" + el.id).css("-webkit-background-size", "cover");
            $("#" + el.id).css("-moz-background-size", "cover");
            $("#" + el.id).css("-o-background-size", "cover");
            $("#" + el.id).css("background-size", "cover");
            
          });
          
        }
        
        Highcharts.setOptions(x.conf_opts);
        
        if(x.type == "chart") {
          if(x.debug) console.log("charting CHART");
          $("#" + el.id).highcharts(x.hc_opts);
        } else if (x.type == "stock") {
          if(x.debug) console.log("charting STOCK");
          $("#" + el.id).highcharts('StockChart', x.hc_opts);  
        } else if (x.type == "map"){
          if(x.debug) console.log("charting MAP");

          x.hc_opts.series = x.hc_opts.series.map(function(e){
            if(e.geojson === true) {
              if(x.debug) console.log("geojson\n\t", e.type, "\n\t", typeof(e.series));
              e.data = Highcharts.geojson(e.data, e.type);
            }
            return e;
          });
          
          $("#" + el.id).highcharts('Map', x.hc_opts); 
          
          if(x.hc_opts.mapNavigation !== undefined && x.hc_opts.mapNavigation.enabled === true){
            /* if have navigation option and enabled true: http://stackoverflow.com/questions/7600454 */
            $("#" + el.id).bind( 'mousewheel DOMMouseScroll', function ( e ) {
              var e0 = e.originalEvent,
              delta = e0.wheelDelta || -e0.detail;
              this.scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
              e.preventDefault();

            });
            
          }
          
        }
        
        if(x.hc_opts.motion !== undefined) {
          
          $("#" + el.id).css({"position" : "relative" });
          
          if(x.debug) console.log("setting MOTION options");
          
          var pc = $($("#" + el.id).find("#play-controls")[0]);
          
          var ct = x.theme.chart;
          
          if(ct.backgroundColor !== undefined) $(pc.find("#play-pause-button")[0]).css({backgroundColor : x.theme.chart.backgroundColor});
          if(ct.style !== undefined)  $(pc.find("#play-output")[0]).css(x.theme.chart.style);
          if(ct.style !== undefined && ct.style.color !== undefined) $(pc.find("#play-pause-button")[0]).css({color : x.theme.chart.style.color});
          
          
        } 

        chart = $("#" +el.id).highcharts();
        
        if (HTMLWidgets.shinyMode) {
        }

      },

      getChart: function(){
        return chart;
      },

      resize: function(width, height) {
        var w = chart.renderTo.clientWidth; 
        var h = chart.renderTo.clientHeight; 
        chart.setSize(w, h); 
      }

    }
  }

});

// From Friss tuto (https://github.com/FrissAnalytics/shinyJsTutorials/blob/master/tutorials/tutorial_03.Rmd)
function get_highchart(id){
  
  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);
  
  // Use the getChart method we created to get the underlying highchart chart
  var hcObj ;
  
  if (typeof htmlWidgetsObj != 'undefined') {
    hcObj = htmlWidgetsObj.getChart();
  }

  return(hcObj);
}

if (HTMLWidgets.shinyMode) {

  Shiny.addCustomMessageHandler('set-title',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        chart.setTitle(
          msg.titleOpts, 
          msg.subtitleOptions, 
          msg.redraw);
      }
  });

  Shiny.addCustomMessageHandler('add-plotline',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        if(msg.axis == 'x') {
          chart.xAxis[0].addPlotLine(options = msg.options);
        } else {
          chart.yAxis[0].addPlotLine(options = msg.options);
        }
      }
  });

  Shiny.addCustomMessageHandler('remove-plotline',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        if(msg.axis == 'x') {
          chart.xAxis[0].removePlotLine(id = msg.band);
        } else {
          chart.yAxis[0].removePlotLine(id = msg.band);
        }
      }
  });

  Shiny.addCustomMessageHandler('add-plotband',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        chart.xAxis[0].addPlotBand(options = msg.options);
      }
  });

  Shiny.addCustomMessageHandler('remove-plotband',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        chart.xAxis[0].removePlotBand(id = msg.band);
      }
  });

  Shiny.addCustomMessageHandler('set-visible',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        var serie = chart.series[msg.serie];
        serie.setVisible(
          visible = msg.visible, 
          redraw = msg.redraw
        );
      }
  });

  Shiny.addCustomMessageHandler('set-extremes',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        var axis = chart.yAxis[0];
        axis.setExtremes(
          newMin = msg.newMin, 
          newMax = msg.newMax, 
          redraw = msg.redraw, 
          animation = msg.animation
        );
      }
  });

  Shiny.addCustomMessageHandler('set-data',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        chart.series[msg.serie].setData(
          data = msg.data,
          redraw = msg.redraw,
          animation = msg.animation,
          updatePoints = msg.updatePoints
        );
      }
  });

  Shiny.addCustomMessageHandler('update-axis',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        chart.yAxis[msg.axis].update(
          options = msg.options,
          redraw = msg.redraw
        );
      }
  });

  Shiny.addCustomMessageHandler('redraw',
    function(msg) {
      var chart = get_highchart(msg.id);
      if (typeof chart != 'undefined') {
        chart.redraw();
      }
  });

}
