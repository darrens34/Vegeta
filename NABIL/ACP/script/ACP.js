function setGraph(){

d3.queue()
    .defer(d3.csv, "data/X_par_heure.csv",d3.values)
    .defer(d3.csv, "data/data_stand.csv",d3.values)
    .defer(d3.csv, "data/vec_propre.csv",d3.values)
    .defer(d3.csv, "data/beta_eq.csv",d3.values)
    .await(calcul);    
    
    function calcul(error,X_par_heure,data_stand,vec_propre,beta_eq){ 
   	 // TRANSPOSE############
    	var transpose=d3.transpose(X_par_heure); 
    	//NORMALISE ############   
    	var data=[];
    	for (var TIME=1;TIME<49;TIME++) {
    		var dim1=[];
    		var dim2=[];
    		var dim3=[];
	    	transpose[TIME].forEach(
	    		function NORM(value,i){
	    			var CR=(value-data_stand[i][1])/data_stand[i][2] ; //CENTRER REDUIT
	    			dim1.push(CR*vec_propre[i][1]);
	    			dim2.push(CR*vec_propre[i][2]);
	    			dim3.push(CR*vec_propre[i][3]);
	    	});
	    	
	    	var data_DIM=[];
	    	data_DIM.push(d3.sum(dim1),d3.sum(dim2),d3.sum(dim3));
	    	var Y=data_DIM[0]*beta_eq[0][1]+data_DIM[1]*beta_eq[1][1]+data_DIM[2]*beta_eq[2][1]+Number(beta_eq["columns"][1]);
	    	data.push(Y);
    }
    	console.log(data);
    	
    	
		// SVG ##################################################################
		var m = [80, 80, 80, 80]; // margins
		var w = 800 - m[1] - m[3]; // width
		var h = 400 - m[0] - m[2]; // height
		// X scale 
		var x = d3.scaleLinear().domain([0, data.length]).range([0, w]);
		// Y scale 
		var y = d3.scaleLinear().domain([d3.min(data), d3.max(data)]).range([h, 0]);


    	
		// create a line function that can convert data[] into x and y points
		var line = d3.line()
			.x(function(d,i) { 
				//console.log('X value : ' + d + ' using index: ' + i + ' to be at: ' + x(i) + ' using our xScale.');
				// return the X coordinate
				return x(i); 
			})
			.y(function(d) { 
				//console.log('Y value : ' + d + ' situé : ' + y(d) + " avec yScale.");
				// return the Y coordinate 
				return y(d); 
			})

			// Add an SVG element with the desired dimensions and margin.
			var graph = d3.select("#graph").append("svg:svg")
			      .attr("width", w + m[1] + m[3])
			      .attr("height", h + m[0] + m[2])
			    .append("svg:g")
			      .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

			// create xAxis
			var xAxis = d3.axisBottom(x).ticks(24);
			// Add the x-axis.
			graph.append("svg:g")
			      .attr("class", "x-axis")
			      .attr("transform", "translate(0," + h + ")")
			      .call(xAxis);


			// create yAxis
			var yAxisLeft = d3.axisLeft(y).ticks(8);
			// Add the y-axis to the left
			graph.append("svg:g")
			      .attr("class", "y-axis")
			      .attr("transform", "translate(-25,0)")
			      .call(yAxisLeft);
			
  			// TRACE  la COURBE
  			graph.append("svg:path").attr("d", line(data));
			

	}


	
}

