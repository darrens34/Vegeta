var div = d3.select('body').append('div')

var margin = {top: 20, right: 20, bottom: 30, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

var parseDate = d3.time.format("%Y-%m-%d").parse;
var formatPct = d3.format('.0%')

var x = d3.time.scale()
    .range([0, width]);

var y = d3.scale.linear()
    .range([height, 0]);

var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");

var yAxis = d3.svg.axis()
    .scale(y)
    .tickFormat(formatPct)
    .orient("left");

var line = d3.svg.line()
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.cror); });

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var dataFiltered = {}
var dataNested = {}

d3.json("data.json", function(error, data) {
  data.forEach(function(d) {
    d.date = parseDate(d.date);
    d.cror = +d.cror;
    d.value = +d.value;
  });

  var dataNested = d3.nest()
    .key(function (d) { return d.variable })
    .entries(data)

  div.append('select')
      .attr('id','variableSelect')
      .on('change',variableChange)
    .selectAll('option')  
      .data(dataNested).enter()
    .append('option')
      .attr('value',function (d) { return d.key })
      .text(function (d) { return d.key })

  var dataFiltered = dataNested.filter(function (d) { return d.key===d3.select('#variableSelect').property('value') })

  x.domain(d3.extent(dataFiltered[0].values, function(d) { return d.date; }));
  y.domain(d3.extent(dataFiltered[0].values, function(d) { return d.cror; }));

  svg.append("g")
      .attr("class", "xAxis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);

  svg.append("g")
      .attr("class", "yAxis")
      .call(yAxis)
    .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Cumulative Return");

  svg.append("path")
      .datum(dataFiltered[0].values)
      .attr("class", "line")
      .attr("d", line);

  function variableChange() {
  	var value = this.value
   	var dataFiltered = dataNested.filter(function (d) { return d.key===value })
    x.domain(d3.extent(dataFiltered[0].values, function(d) { return d.date; }));
    y.domain(d3.extent(dataFiltered[0].values, function(d) { return d.cror; }));
    d3.select('.xAxis').transition().duration(1000).call(xAxis)
    d3.select('.yAxis').transition().duration(1000).call(yAxis)
    d3.select('.line').datum(dataFiltered[0].values).attr('d',line)
	 }

});