function draw(months){
    d3.csv("data/puechabon/month/Puechabon_mean_per_month.csv", function(error, data){
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

        svg.select(".curve")
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

function saveCurve(){
    // garde la trace de la courbe sélectionnée
    var dupplicated = false; // devient true si la courbe a déjà été sauvegardée
    var graph = document.getElementById("curves");
    var curves = document.getElementsByClassName("curve");
    var oldCurve = curves[0].cloneNode(false); // courbe actuellement dessinée par l'utilisateur
    for(i=1;i<curves.length;i++){ // i débute à 1 pour ignorer la courbe que l'on souhaite sauvegarder pendant la vérification
        if(curves[i].getAttribute('d') == oldCurve.getAttribute('d')){
            dupplicated = true
        }
    }
    if(!dupplicated){
        graph.innerHTML += oldCurve.outerHTML;
        curves = document.getElementsByClassName("curve");
        for(i=1;i<curves.length;i++){
            curves[i].setAttribute("opacity",i/curves.length)
        }
    }
}

function resetCurve(){
    // supprime les traces d'anciennes courbes en supprimant tous les enfants de la balise g, sauf la courbe actuellement dessinée
    graph = document.getElementById("curves");
    while (graph.childNodes.length > 2) { // 2 car le saut à la ligne compte comme un child
        graph.removeChild(graph.lastChild);
    }  
}