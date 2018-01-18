function draw(months){
    d3.csv("data/puechabon/month/acp/X_par_heure.csv", function(error, data){
        var date = []
        var sap = []

        data.forEach(function (d){
            if(d.mois == months){
                date.push(d.heure_solaire);
                sap.push(d.SAP_FLOW);
            }
        })

        var scaleX = d3.scaleLinear()
            .domain([0,24])
            .range([0,1000]);

        var scaleY = d3.scaleLinear()
            .domain([d3.min(data, function(d){
                return d.SAP_FLOW;
            }),d3.max(data, function(d){
                return d.SAP_FLOW;
            })])
            .range([500,0]);

        var axisX = d3.axisBottom(scaleX);

        var axisY = d3.axisLeft(scaleY);

        var svg = d3.select("#graph");
        
        svg.select("#axisX")
            .call(axisX.ticks(24))
            .attr("transform","translate(100,560)")
			.attr("font-size",25);

        svg.select("#axisY")
            .call(axisY)
            .attr("transform","translate(90,50)")
			.attr("font-size",25);
                            
        var Lvalues = d3.line()
            .x(function(d,i){
                return scaleX(i)/2
            })
            .y(function(d,i){
                return scaleY(sap[i])
            })
            .curve(d3.curveBasis);

        svg.select("#curve")
            .attr("transform", "translate(100,50)")
            .attr("stroke","green")
			.attr("stroke-width",2 )
            .attr("fill","none")
            .transition()
            .duration(500)
            .attr("d", Lvalues(date));
    })

    d3.select("#months").on("input", function() {
        update(+this.value);
    });

    
}

function setMonth(months){
    var monthNames = ["Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
    "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"
    ];
    return monthNames[months]
}

function update(months) {
    d3.select("#months-value").text(setMonth(months))
    d3.select("#months").property("value", months);
    draw(months)
}

function setGraph(){
    h = 600
    w = 1000

    var svg = d3.select("#graph")
        .attr("width" , w)
        .attr("height", h);

    svg.append("text")
        .attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
        .attr("transform", "translate(100,20)")  // text is drawn off the screen top left, move down and out and rotate
		.attr("font-size",22)
		.text("Flux de sève");

    svg.append("text")
        .attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
        .attr("transform", "translate(500,500)")  // centre below axis
		.attr("font-size",22)
        .text("Heure");
}