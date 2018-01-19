var dataDict = {};
var nomX=[];
var betaDict = {};
var factMult=[1,1,1,1,1,1,1,1,1,1,1]; // Facteur multiplicatif des X
var nbBeta = 11;

// Fonction qui récupère les bétas
function readBetaFile(){
	var xhttp2 = new XMLHttpRequest();
	xhttp2.open("GET","data/puechabon/reg_lin/beta_eq.csv", true);
	xhttp2.onreadystatechange = function (){
		if(xhttp2.readyState === 4){
			var data2 = xhttp2.responseText;
			var allTextLines2 = data2.match(/[^(\r\n|\t|,)]+/g);
			for(i=0;i<(nbBeta*2);i+=2){
				betaDict[allTextLines2[i]]= [allTextLines2[i+1]];	
			}
			
		}
	}
	xhttp2.send();
}

// fonction qui récupère les données .CSV 
// qui les nettoie, pour avoir un  un tableau des X variables pour chaque demi-heure 
// et le nom des variables X 
// et qui appelle la fonction EQ et Sliders
function readTextFile(){
	var xhttp = new XMLHttpRequest();
	xhttp.open("GET", "data/puechabon/reg_lin/X_par_heure.csv", true);
	xhttp.onreadystatechange = function (){
		if(xhttp.readyState === 4){
				var data = xhttp.responseText;
				var allTextLines = data.match(/[^(\r\n|\t|,)]+/g);
				for(i=0;i<nbBeta;i++){
					for(j=1;j<=48;j++){
						if ( !dataDict[allTextLines[(i*49)]]){
							dataDict[allTextLines[(i*49)]]= [];
						}
						dataDict[allTextLines[(i*49)]].push(allTextLines[((i)*49)+j]);
					}
					nomX.push(allTextLines[(i*49)]);
				}
						console.log(allTextLines);
		EQ(dataDict,nomX,betaDict,factMult);
		sliders(nomX);
		}
	}
	xhttp.send();
}

// Fonction qui calcule les Y avec l'equation du modèle
// qui prend comme entrée un tableau des X variables pour chaque demi heure 
// qui prend comme 2ème entrée le nom des variables X
function EQ(Donnee,nomX,betaDict,factMult){
	var dataX=[];
	var Y=[];
	for (i=0;i<48 ;i++) {
		
		dataX=[];
		for (j=1;j<nomX.length;j++) {		
					dataX.push(Donnee[nomX[j]][i]);	
		}
		
		// Equation automatique :
		Y[i]=(Number(betaDict["beta0"][0])+
		betaDict[nomX[1]][0]*dataX[0]*factMult[0]+
		betaDict[nomX[2]][0]*dataX[1]*factMult[1]+
		betaDict[nomX[3]][0]*dataX[2]*factMult[2]+
		betaDict[nomX[4]][0]*dataX[3]*factMult[3]+
		betaDict[nomX[5]][0]*dataX[4]*factMult[4]+
		betaDict[nomX[6]][0]*dataX[5]*factMult[5]+
		betaDict[nomX[7]][0]*dataX[6]*factMult[6]+
		betaDict[nomX[8]][0] *dataX[7]*factMult[7]+
		betaDict[nomX[9]][0]*dataX[8] *factMult[8]+
		betaDict[nomX[10]][0]*dataX[9]*factMult[9]) ;
	}
	for(yi=0;yi<Y.length;yi++){
		if(Y[yi] < 0){
			Y[yi] = 0}};  // Limite à O min
	Courbe(Y);
}

// Fonction qui trace la courbe "line" + les points "cir"
function Courbe(Y){	
	// Remise à zero de la courbe : 
	var svg = d3.select("#graph");

	// scaleY
	var scaleY = d3.scaleLinear();
	// J'inverse min et max car pour Y c'est inversé
	scaleY.domain([25,0]);
	scaleY.range([0,500]);

	// Axe Y
	var yAxis = d3.axisLeft(scaleY);
	var gyAxis = svg.select("#axisX");
	gyAxis.call(yAxis);
	gyAxis.attr("font-size",28);
	gyAxis.attr("transform","translate(50,50)");
	
	// scaleX
	var scaleX = d3.scaleLinear();
	scaleX.domain([0,24]);
	scaleX.range([0,1000]);
	
	// Axe X
	var xAxis = d3.axisBottom(scaleX);
	var gxAxis = svg.select("#axisY");
	gxAxis.call(xAxis.ticks(24));
	gxAxis.attr("font-size",28);
	gxAxis.attr("transform","translate(50,550)");
	
	// Ajout titres des axes
	var tmp ="";
	tmp +='<text x="550" y="620" font-size="28" fill="black" style="text-anchor: middle"  >Heure</text>';
	tmp += ' <text x="50" y="0" font-size="28" fill="black" style="text-anchor: middle"  >Flux de sève</text>';
	var titre_axes = document.getElementById("texte");
	titre_axes.innerHTML = tmp;	

	// line
	var lValues = d3.line();
	lValues.x(function(d,i) { return scaleX(i/2) });
	lValues.y(function(d) { return scaleY(d)});
	var gLine = svg.select(".courbe");
	gLine.attr("transform", "translate(50,50)");
	gLine.attr("stroke", "green");
	gLine.attr("fill", "none");
	gLine.attr("stroke-width",2 );
	gLine.attr("d", lValues(Y));
	
	// Ajout des points
	cir ="";
	for (j=0;j<Y.length;j++) {
		cir +=' <circle transform = "translate(50,50)" onmouseover="drawInfoBox('+Y[j]+','+j+','+scaleX(j)+','+scaleY(Y[j])+')" onmouseleave="removeInfoBox()" 	cx="'+scaleX(j/2)+'" cy="'+scaleY(Y[j])+'" r="5" fill="green" />' ;
	}
	var gPoints = document.getElementById("points");
	gPoints.innerHTML = cir  ;			
}


// Info box
function drawInfoBox(datay,datax,X,Y){
	var info = '<rect  x=800 y=100 height="70" width="250" fill="grey" opacity=0.5 "/>'
	info += '<text  y=130 x =870 font-size="20" fill="black">'+'A '+(datax/2)+' Heure </text> ';
	info += '<text  y=160 x=850 font-size="20" fill="black">Flux de sève = '+Math.round(datay*100)/100+'</text> ';
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
	"PPFD_IN_1h":"Densité de Flux Photon Photosynthetique (PPFD)",
	"TA":"Temperature de l'air",
	"TS":"Temperature du sol",
	"WD_1h30":"Direction du vent",
	"CO2":"Concentration de CO2",
	"FC_1h":"Flux CO2",
	"LE_30m":"Flux de chaleur latente",
	"SH_3h":"Flux de Stockage de chaleur sensible",
	"ZL_3h":"Paramètre de stabilité",
	"VPD":"Déficit de pression de vapeur"
	}

function sliders(nomX) {
var ID="";
var ID2 ="";
var sliderSX ="";
	for (a=0;a<=4;a++) {
		var ID = "VAL"+a;
		var ID2 = "value"+a;
		var b = (a+1);
		sliderSX +='<p>'+dicoNom[nomX[b]]+' :  <span id="'+ID2+'">'+factMult[a]+'</span></p><div class="slidecontainer"><input oninput="Change('+ID+','+ID2+','+a+')"  type="range" min="0" max="4" step="0.25" value="'+factMult[a]+'" class="slider" id="'+ID+'"></div>';
		var sliderS1 = document.getElementById("sliderS1");
		sliderS1.innerHTML = sliderSX ;		
	}
var sliderSX ="";
	for (a=5;a<=9;a++) {
		var ID = "VAL"+a;
		var ID2 = "value"+a;
		var b = (a+1);
		sliderSX +='<p>'+dicoNom[nomX[b]]+' :  <span id="'+ID2+'">'+factMult[a]+'</span></p><div class="slidecontainer"><input oninput="Change('+ID+','+ID2+','+a+')"  type="range" min="0" max="4" step="0.25" value="'+factMult[a]+'" class="slider" id="'+ID+'"></div>';
		var sliderS2 = document.getElementById("sliderS2");
		sliderS2.innerHTML = sliderSX ;		
	}
}

function Change(ID,ID2,a) {
	var VAL = ID.value; // valeur qu'on récuper
	var VAL2= ID2 ; // et ou est ce qu'on l'affiche 
	VAL2.innerHTML = VAL ;
	factMult[a]=VAL ;
	readTextFile(factMult);
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