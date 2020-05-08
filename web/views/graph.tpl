<!DOCTYPE html>
<!--[if lt IE 7]>      <html class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->
<!--[if IE 7]>         <html class="no-js lt-ie9 lt-ie8"> <![endif]-->
<!--[if IE 8]>         <html class="no-js lt-ie9"> <![endif]-->
<!--[if gt IE 8]><!--> <html class="no-js"> <!--<![endif]-->
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>邮件日志系统</title>
        <meta name="description" content="">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <!-- Place favicon.ico and apple-touch-icon.png in the root directory -->

        <link rel="stylesheet" href="css/normalize.css">
        <link rel="stylesheet" href="css/main.css">
        <link rel="stylesheet" href="css/my.css">
        <link rel="stylesheet" href="css/bootstrap.min.css">
        <script src="js/vendor/modernizr-2.6.2.min.js"></script>
    </head>
    <body>
        <!--[if lt IE 7]>
            <p class="browsehappy">You are using an <strong>outdated</strong> browser. Please <a href="http://browsehappy.com/">upgrade your browser</a> to improve your experience.</p>
        <![endif]-->

        <script src="js/vendor/react-0.8.0/build/react.js"></script>
        <script src="js/vendor/react-0.8.0/build/JSXTransformer.js"></script>

        <script src="js/vendor/jquery-1.10.2.min.js"></script>
        <script src="js/vendor/bootstrap.min.js"></script>
        <script src="js/vendor/underscore-min.js"></script>
        <script src="js/vendor/d3.v3.min.js"></script>
        <script src="js/plugins.js"></script>
    <div class="container">

        <div class="navbar navbar-default">
            <a href="/" class="navbar-brand">邮件日志系统</a>
            <ul class="nav navbar-nav">
                <li><a href="/">首页</a></li>
            </ul>
        </div>
        <style>
         .node {
           stroke: #fff;
           stroke-width: 1.5px;
         }

         .link {
           stroke: #999;
           stroke-opacity: .6;
         }
        </style>

        <div id="graph">
        </div>

        <script>
          function draw(data,threshold,charge,factor,nodesize){
             data0 = _.chain(data['emails'])
                      .filter(function(i){return i['v']>threshold})
                      .filter(function(i){return i.from != "" && i.to != ""})
                      .value();

             domains = _.chain(data0)
                         .map(function(d){return [d['from'],d['to']]})
                         .flatten()
                         .uniq()
                         .value();

             var width = 4000;
             var height = 4000;

             var color = d3.scale.category20();

             var force = d3.layout.force()
                                  .charge(charge)//-20
                                  .linkDistance(function(d,i){return Math.exp(-d.value/10)*factor}) //10
                                  .size([width, height]);

             var svg = d3.select('#graph').append('svg')
                         .attr('width', width)
                         .attr('height', height);

             nodes = _.map(domains, function(i){return {name:i}});
             links = _.map(data0,
                           function(d){return {'source':_.indexOf(domains,d['from']),
                                               'target':_.indexOf(domains,d['to']),
                                               'value': d['v']}});

             force.nodes(nodes)
                  .links(links)
                  .start();

             var link = svg.selectAll(".link")
                           .data(links)
                           .enter().append('line')
                           .attr("class", "link")
                           .style("stroke-width", function(d){return 1;});//Math.sqrt(d.value);});
             var node = svg.selectAll(".node")
                           .data(nodes)
                           .enter().append("circle")
                           .attr("class", "node")
                           .attr("r", nodesize)//5
                           .style("fill", function(d){return d.name.search(/@fudan.edu.cn/) > 0 && "#A00" || "#333"})
                           .call(force.drag);

             node.append("title")
                 .text(function(d){return d.name || "null"});

             force.on("end", function() {
                 link.attr("x1", function(d) { return d.source.x; })
                     .attr("y1", function(d) { return d.source.y; })
                     .attr("x2", function(d) { return d.target.x; })
                     .attr("y2", function(d) { return d.target.y; });

                 node.attr("cx", function(d) { return d.x; })
                     .attr("cy", function(d) { return d.y; });
             });
             n = 0;
             force.on("end", function() {
                 if (n % 100 == 0){
                 link.attr("x1", function(d) { return d.source.x; })
                     .attr("y1", function(d) { return d.source.y; })
                     .attr("x2", function(d) { return d.target.x; })
                     .attr("y2", function(d) { return d.target.y; });

                 node.attr("cx", function(d) { return d.x; })
                     .attr("cy", function(d) { return d.y; });
                     }
                 n++;
             });


          }
          $(document).ready(function(){
             //$.getJSON('fromto', function(d){window.data =d; draw(d,3); draw(d,2)});
          });
        </script>
    </div>
    </body>
</html>
