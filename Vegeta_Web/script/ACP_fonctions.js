// Fonction qui trace la courbe "line" + les points "cir"
function Courbe(Y){	
	// Remise à zero de la courbe : 
	var svg = d3.select("#graph");

	// scaleY
	var scaleY = d3.scaleLinear();
	// J'inverse min et max car pour Y c'est inversé
	scaleY.domain([1,0]);
	scaleY.range([0,500]);

	// Axe Y
	var yAxis = d3.axisLeft(scaleY);
	var gyAxis = svg.select("#axisX");
	gyAxis.call(yAxis);
	gyAxis.attr("font-size",24);
	gyAxis.attr("transform","translate(50,50)");
	
	// scaleX
	var scaleX = d3.scaleLinear();
	scaleX.domain([0,24]);
	scaleX.range([0,800]);
	
	// Axe X
	var xAxis = d3.axisBottom(scaleX);
	var gxAxis = svg.select("#axisY");
	gxAxis.call(xAxis.ticks(24));
	gxAxis.attr("font-size",24);
	gxAxis.attr("transform","translate(50,550)");
	
	// Ajout titres des axes
	var tmp ="";
	tmp +='<text x="450" y="620" font-size="28" fill="black" style="text-anchor: middle"  >Heure</text>';
	tmp += ' <text x="100" y="0" font-size="28" fill="black" style="text-anchor: middle"  >Flux de sève (mm/h)</text>';
	var titre_axes = document.getElementById("texte");
	titre_axes.innerHTML = tmp;	

	// line
	var lValues = d3.line();
	lValues.x(function(d,i) { return scaleX(i/2) });
	lValues.y(function(d) { return scaleY(d)});
	var gLine = svg.select(".courbe")
	.attr("transform", "translate(50,50)")
	.attr("stroke", "green")
	.attr("stroke-width",2 )
	.attr("fill", "none")
	.transition()
	.duration(500)
	.on("start", function(d){
		var gPoints = document.getElementById("points");
		gPoints.innerHTML = ""  ;	
	})
	.on("end", function(d){
		// Ajout des points
		cir ="";
		for (j=0;j<Y.length;j++) {
			cir +=' <circle transform = "translate(50,50)" onmouseover="drawInfoBox('+Y[j]+','+j+','+scaleX(j)+','+scaleY(Y[j])+')" onmouseleave="removeInfoBox()" 	cx="'+scaleX(j/2)+'" cy="'+scaleY(Y[j])+'" r="5" fill="green" />' ;
		}
		var gPoints = document.getElementById("points");
		gPoints.innerHTML = cir  ;		
		
	})
	.attr("d", lValues(Y));		
}

// Info box
function drawInfoBox(datay,datax,X,Y){
	var info = '<rect  x=500 y=100 height="70" width="250" fill="grey" opacity=0.5 "/>'
	info += '<text  y=130 x =570 font-size="20" fill="black">'+'A '+(datax/2)+' Heure </text> ';
	info += '<text  y=160 x=550 font-size="20" fill="black">Flux de sève = '+Math.round(datay*100)/100+'</text> ';
	var monSVGinfo = document.getElementById("infobox");
	monSVGinfo.innerHTML = info  ; 	
	//monSVGinfo.setAttribute("transform","translate("+(X-200)+","+(Y+100)+")");	
}
function removeInfoBox(){
	var info="";
	var monSVGinfo = document.getElementById("infobox");
	monSVGinfo.innerHTML = info  ; 	
}

// Fonction pour choisir le facteur multiplicatif associé a chaque X. De 0 à 4 (0.25 : divisé par 4 à 4 : multiplié par 4)	
// Fonction qui créer les SLIDERS
dicoNom = {
	"NETRAD_1h30":"Radiation nette (W m"+"-2".sup()+")",
	"P_1h":"Précipitation (mm)",
	"PA_3h":"Pression athmosphérique (kPa)",
	"PPFD_DIF_1h": "Densité de Flux Photon Photosynthétique diffuse incidente (μmol m"+"-2".sup()+" s"+"-1".sup()+")",
	"PPFD_IN_1h":"Densité de Flux Photon Photosynthétique (μmol m"+"-2".sup()+" s"+"-1".sup()+")",
	"PPFD_OUT_1h":"Densité de Flux Photon Photosynthétique réflechi (μmol m"+"-2".sup()+" s"+"-1".sup()+")",
	"RH":"Humidité relative (%)",
	"SW_IN_1h":"Radiations des ondes courtes incidentes (W m"+"-2".sup()+")",
	"SW_OUT_30m":"Radiations des ondes courtes sortantes (W m"+"-2".sup()+")",
	"TA":"Température de l'air (°C)",
	"TS":"Température du sol (°C)",
	"TS_2": "Température du sol (°C) après 2h",
	"TS_3":"Température du sol (°C) après 3h",
	"WD_1h30":"Direction du vent (Degré)",
	"WS": "Vitesse du vent (m s"+"-1".sup()+")",
	"CO2":"Concentration de CO"+"2".sub()+" (ppm)",
	"FC_1h":"Flux CO2 (μmol CO"+"2".sub()+" m"+"-2".sup()+" s"+"-1".sup()+")",
	"H_1h30":"Flux de chaleur sensible (W m"+"-2".sup()+")",
	"H2O_3h":"Eau (mmol H"+"2".sub()+"O m"+"-2".sup()+"s"+"-1".sup()+")",
	"LE_30m":"Flux de chaleur latente (W m"+"-2".sup()+")",
	"SB":"Stock de chaleur dans la biomasse (W m"+"-2".sup()+")",
	"SC_3h":"Flux de Stockage de CO"+"2".sub()+" (μmol CO"+"2".sub()+" m"+"-2".sup()+" s"+"-1".sup()+")",
	"SH_3h":"Flux de Stockage de chaleur sensible (W m"+"-2".sup()+")",
	"SLE_3h":"Flux de stockage de chaleur latente (W m"+"-2".sup()+")",
	"TAU_30m": "Momentum flux (Kg m"+"-1".sup()+"s"+"-2".sup()+")",
	"USTAR_30m":"Vitesse de frottement (m s"+"-1".sup()+")",
	"ZL_3h":"Paramètre de stabilité (sans unité)",
	"G":"Flux de chaleur du sol (W m"+"-2".sup()+")",
	"VPD":"Déficit de pression de vapeur (kPa)"}



function sliders(nomX) {

	d3.csv("data/puechabon/sliders/factMult_origin.csv", function(error,data){
		

		minFact = [];
		maxFact = [];
		step = [];
		minVal = [];
		maxVal = [];

		data.forEach(function (d){
			minFact.push(d.factMultMin);
			maxFact.push(d.factMultMax);
			step.push(d.step);
			minVal.push(d.minVal);
			maxVal.push(d.maxVal)
		})

		var ID="";
		var ID2 ="";
		var sliderSX ="";
			for (a=0;a<=13;a++) {
				// pour transposer les valeurs des facteurs multiplicateurs dans le même ordre de grandeur que celui des modalités sans modifier les sliders (pour bien recupérer le facteur multiplicateur utilisé par l'équation)
				// ... je dois stocker les différentes valeurs constitutant le slider dans un tableau, avec son minimum, son maximum, et toutes ses valeurs intérmédiaires qui dépendent du pas.
				// cela me permettra de récupérer l'indice du tableau correspondant à chaque valeur, et de l'utiliser comme coefficient multiplicateur.
				// Ex : mon slider part de 0.95 à 1.05 avec un pas de 0.025. Je veux afficher des valeurs qui vont de 390 à 450. 
				// (1.05 - 0.95) / 0.25 = 4, soit l'indice maximal de mon tableau (soit la taille du tableau - 1).
				// Je recrée les valeurs constitutant le slider en partant de la valeur de départ, et on ajoutant le pas à chaque fois, cela me donne le tableau suivant : [0.95, 0.975, 1, 1.025, 1.05].
				// Selon la position du slider, j'utiliserai l'indice du tableau pour transposer ma valeur dans le plan d'arrivée. Par example, si le slider retourne 1.025, je détermine l'indice du tableau correspondant : ici 3.
				// J'effectue ensuite l'opération suivante : 390 + 3 * (450 - 390)/4 => 435.
				// Un facteur multiplicateur de 1.025 correspond donc à 435 dans mes données.

				var rangeValues = []; // tableau qui va contenir les valeurs du sliders
				var numberValues = (maxFact[a] - minFact[a])/step[a]; // nombre de valeurs intermédiaire du range
				var i = 0;
				var value = parseFloat(minFact[a]); // min du slider converti en float
				while(i<=numberValues){
					rangeValues.push(parseFloat(value));
					value += parseFloat(step[a]); // ajout du pas à chaque itération, tant que i < valeur du slider maximum
					value = parseFloat(value.toFixed(3)); 
					i++;
				}
				var ID = "VAL"+a;
				var ID2 = "value"+a;
				printValue = parseFloat(minVal[a])+ parseFloat(rangeValues.indexOf(parseFloat(factMult[a]))*(maxVal[a] - minVal[a])/numberValues) // opération de translation
				printValue = parseFloat(printValue.toFixed(2));
				sliderSX +='<p>'+dicoNom[nomX[a]]+' :  <span id="'+ID2+'">'+ printValue +'</span></p><div class="slidecontainer"><p class ="baliseInf">'+minVal[a]+'</p><input oninput="Change('+ID+','+ID2+','+a+')"  type="range" min="'+minFact[a]+'" max="'+maxFact[a]+'" step="'+step[a]+'" value="'+factMult[a]+'" class="slider" id="'+ID+'"><p class = "baliseSup">'+maxVal[a]+'</p></div>';
				var sliderS1 = document.getElementById("sliderS1");
				sliderS1.innerHTML = sliderSX ;
			}
		var sliderSX ="";
			for (a=14;a<=28;a++) {
				var rangeValues = [];
				var numberValues = (maxFact[a] - minFact[a])/step[a];
				var i = 0;
				var value = parseFloat(minFact[a]);
				while(i<=numberValues){
					rangeValues.push(parseFloat(value));
					value += parseFloat(step[a]);
					value = parseFloat(value.toFixed(3));
					i++;
				}
				var ID = "VAL"+a;
				var ID2 = "value"+a;
				printValue = parseFloat(minVal[a])+ parseFloat(rangeValues.indexOf(parseFloat(factMult[a]))*(maxVal[a] - minVal[a])/numberValues)
				printValue = parseFloat(printValue.toFixed(2));
				sliderSX +='<p>'+dicoNom[nomX[a]]+' :  <span id="'+ID2+'">'+ printValue +'</span></p><div class="slidecontainer"><p class ="baliseInf">'+minVal[a]+'</p><input oninput="Change('+ID+','+ID2+','+a+')"  type="range" min="'+minFact[a]+'" max="'+maxFact[a]+'" step="'+step[a]+'" value="'+factMult[a]+'" class="slider" id="'+ID+'"><p class = "baliseSup">'+maxVal[a]+'</p></div>';
				var sliderS2 = document.getElementById("sliderS2");
				sliderS2.innerHTML = sliderSX ;
			}
		})
}

function Change(ID,ID2,a) {
	var VAL = ID.value; // valeur qu'on récuper
	var VAL2= ID2 ; // et ou est ce qu'on l'affiche 
	VAL2.innerHTML = VAL ;
	factMult[a]=VAL ;
	setGraph(factMult);
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