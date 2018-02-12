   function draw(Netrad){
    d3.csv("data/puechabon/sliders/factMult.csv", function(error, data){
        var X=[];
        var fact_min = [];
        var fact_max = [];
        var step = [];
        var val_min = [];
        var val_max = [];
		//console.log(data);
		//alert(data.length);

        data.forEach(function (d){
            if(d.X == Netrad){
                fact_max.push(d.factMultMax);
                fact_min.push(d.factMultMin);
                step.push(d.step);
                val_min.push(d.minVal);
                val_max.push(d.maxVal);


            }
        })
        for (i=0;i<data.length;i+=1) {
 			d3.slider().axis(true).min(data[i].fact_min).max(data[i].fact_max).step(data[i].step) 
		};

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
            .attr("d", Lvalues(X));
    })

    d3.select("#Netrad").on("input", function() {
        update(+this.value);
    });
    
}
function setVar(Netrad){
    var monthNames = ["Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
    "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"
    ];
    return monthNames[Netrad]
}
function update(Netrad) {
    d3.select("#Netrad-value").text(setVar(Netrad))
    d3.select("#Netrad").property("value", Netrad);
    draw(Netrad)
}

function saveCurve(){
    // garde la trace de la courbe sélectionnée
    var dupplicated = false; // devient true si la courbe a déjà été sauvegardée
	var graph = document.getElementById("courbes");
    var curves = document.getElementsByClassName("courbe");
    var oldCurve = curves[0].cloneNode(false); // courbe actuellement dessinée par l'utilisateur
    for(i=1;i<curves.length;i++){ // i débute à 1 pour ignorer la courbe que l'on souhaite sauvegarder pendant la vérification
        if(curves[i].getAttribute('d') == oldCurve.getAttribute('d')){
            dupplicated = true
        }
    }
    if(!dupplicated){
        graph.innerHTML += oldCurve.outerHTML;
		curves = document.getElementsByClassName("courbe");
        for(i=1;i<curves.length;i++){
            curves[i].setAttribute("opacity",i/curves.length)
        }
    }
}

function resetCurve(){
    // supprime les traces d'anciennes courbes en supprimant tous les enfants de la balise g, sauf la courbe actuellement dessinée
    graph = document.getElementById("courbes");
    while (graph.childNodes.length > 2) { // 2 car le saut à la ligne compte comme un child
        graph.removeChild(graph.lastChild);
    }  
}
