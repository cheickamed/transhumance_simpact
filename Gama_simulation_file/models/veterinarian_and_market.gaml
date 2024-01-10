/**
* Name: generationchemin
* Based on the internal empty template. 
* Author: HP
* Tags: déplacement de troupeaux transhumants en fonction de centre vétérinaires et de marchés pastoraux
* veto_march <- false activate market experiment
* veto_march <- true activate veterinarian experiment
*
*/
model veterinaire

/* Insert your model definition here */
global {
	file shape_file_appetence <- file("../includes/morpho_pedo_carre.shp");
	file shape_file_veto <- file("../includes/parc_a_vaccination_sen_coupe.shp");
	file shape_file_market <- file("../includes/parc_marche_sen_coupe.shp");
	file shape_file_building <- file("../includes/zone_bati_sen_carre.shp");
	file shape_file_zone <- file("../includes/zonage_transhumance.shp");
	geometry shape <- envelope(shape_file_appetence);

	//---------------------------------------les paramètres----------------------------------------
	date starting_date <- date([2020, 10, 15, 7, 0]);
	float step <- 6 #hours;
	int eff_bovin_moy <- 111 min: 0 parameter: 'Boeuf' category: "Effectif Ruminants";
	int eff_ovin_moy <- 257 min: 0 parameter: 'Mouton' category: "Effectif Ruminants";
	int eff_caprin_moy <- 69 min: 0 parameter: 'Caprin' category: "Effectif Ruminants";
	float acc_bovin_moy <- 2.5 min: -10.0 max: 10.0 parameter: 'Bovin' category: "Accroissement (%) Ruminants";
	float acc_ovin_moy <- 3.5 min: -10.0 max: 25.0 parameter: 'Ovin' category: "Accroissement (%) Ruminants";
	float acc_caprin_moy <- 4.5 min: -10.0 max: 35.0 parameter: 'Caprin' category: "Accroissement (%) Ruminants";
	float com_bovin_moy <- (4.5 / 4) min: 1.0 / 4 parameter: 'Bovin' category: "Consommation journalière biomasse";
	float com_ovin_moy <- 1.5 / 4 min: 1.0 / 4 parameter: 'Ovin' category: "Consommation journalière biomasse";
	float com_caprin_moy <- 1.5 / 4 min: 1.0 / 4 parameter: 'Caprin' category: "Consommation journalière biomasse";
	float s_cons_jour <- 0.0;
	float impt_trp_veg <- 0.0;
	float r_g <- 0.0;
	//------------------------------------
	int nb_trp <- 200 min: 3;
	//int nb_jour_transh;
	int cpt_trp_aller;
	int cpt_trp_retour;
	int nb_cycle_aller <- 0;
	int nb_cycle_retour <- 0;
	int moy_cycle_aller <- 0;
	int moy_cycle_retour <- 0;
	float std_cycle_aller_micro <- 0.0;
	float std_cycle_retour_micro <- 0.0;
	float propor_utilisa_aller <- 0.0;
	float propor_utilisa_retour <- 0.0;
	bool veto_march <- false parameter: 'veterinarian=true, market=false';
	//************************ interaction trp_vegetation_grille ****************
	float q_seuil_r <- 25.0 min: 0.0 max: 100.0; // parameter: "Sueil_biomasse" category: "Impact troupeau-végétation"; //le seuil de végétation, ce sueil(25%) est tiré de Dia et Duponnois:désertification
	float qt_pluie <- rnd(100, 500.0) min: 100.0; // parameter: 'Pluviométrie'; //rnd(100.0, 550.0) min: 50.0 max: 550.0 parameter: "Pluie(mm)" category: "Climat";
	string plvt;
	float largeur_cellule <- 9.6 #km;
	float hauteur_cellule <- 9.6 #km;
	bool nettoyage <- false;
	//interaction veto
	int jour_veto <- 4 min: 0 parameter: 'jour(s) veto' category: 'Veterinarian';
	float dist_veto <- 1.0 #km; // min: 0.0 parameter: 'distance(m)' category: 'Veterinaire';
	int jour_marche <- 4 min: 4 max: 14 parameter: 'jour_entre_2_marche';
	bool is_batch <- false;
	bool is_batch_analyse <- false;

	init {
		create zone_batie from: shape_file_building;
		/*create vegetation from: shape_file_appetence with: [pasto::string(get("PASTORAL")), aire::float(get("AREA"))] {
			if pasto = "N" {
				color_vegetation <- #red;
			} else if pasto = "P1" {
				color_vegetation <- #darkgreen; // pâturage généralement de bonne qualité
			} else if pasto = "P2" {
				color_vegetation <- #green; // pâturage généralement de qualité moyenne
			} else if pasto = "P3" {
				color_vegetation <- #lime; // pâturage généralement de qualité médiocre ou faible
			} else if pasto = "P4" {
				color_vegetation <- #aqua; // pâturage uniquement exploitable en saison sèche, inondable
			} else {
				color_vegetation <- #white;
			} }*/
		create zone from: shape_file_zone;
		if veto_march {
			create veterinaire from: shape_file_veto;
		} else {
			create marche from: shape_file_market;
		}

		create troupeau number: nb_trp {
			location <- terr_orig;
		}

		//initialisation de plvt
		if 450.0 <= qt_pluie and qt_pluie <= 550 {
			plvt <- 'bonne';
		} else if 300.0 <= qt_pluie and qt_pluie <= 449 {
			plvt <- 'moyenne';
		} else {
			plvt <- 'sécheresse';
		}
		//mise à jour des taux d'accroissement qui dépendent de la pluviométrie
		if qt_pluie >= 300.0 {
		//write 'mj';
			acc_bovin_moy <- gauss(acc_bovin_moy, 1);
			acc_ovin_moy <- gauss(acc_ovin_moy, 1);
			acc_caprin_moy <- gauss(acc_caprin_moy, 1);
		} else {
			acc_bovin_moy <- gauss(-acc_bovin_moy, 10);
			acc_ovin_moy <- gauss(-acc_ovin_moy, 10);
			acc_caprin_moy <- gauss(-acc_caprin_moy, 10);
		}

	} //fin de l'init du champ global
	reflex trp_veg when: every(step) {
		r_g <- sum(grille collect (each.r));
		impt_trp_veg <- with_precision((1 - r_g / sum(grille collect (each.r_init))) * 100, 10);
	}
	//-------------------------------------------------
	reflex save_dveg when: is_batch_analyse and cpt_trp_retour >= nb_trp - 1 {
		if veto_march {
			save [nb_cycle_aller, moy_cycle_aller, dist_veto / 1000, jour_veto, propor_utilisa_aller] rewrite: false to: "dist_veterinaire_aller.csv" type: csv;
			save [nb_cycle_retour, moy_cycle_retour, dist_veto / 1000, jour_veto, propor_utilisa_retour] rewrite: false to: "dist_veterinaire_retour.csv" type: csv;
		} else {
			save [nb_cycle_aller, moy_cycle_aller, dist_veto / 1000, propor_utilisa_aller, jour_marche] rewrite: false to: "bis_dist_market_aller.csv" type: csv;
			save [nb_cycle_retour, moy_cycle_retour, dist_veto / 1000, propor_utilisa_retour, jour_marche] rewrite: false to: "bis_dist_market_retour.csv" type: csv;
		}

	}

	reflex utilisation_espace_aller when: (nb_trp - 3 <= cpt_trp_aller and cpt_trp_aller < nb_trp - 1) {
		propor_utilisa_aller <- (length(grille where (each.s_nb_trp_inside != 0)) / length(grille)) * 100;
	}

	reflex utilisation_espace_retour when: nb_trp - 2 <= cpt_trp_retour and cpt_trp_retour <= nb_trp {
		propor_utilisa_retour <- length(grille where (each.s_nb_trp_inside != 0)) / length(grille) * 100;
	}

	reflex trp_dure_orig_acc when: nb_trp - 2 <= cpt_trp_aller and cpt_trp_aller < nb_trp {
		nb_cycle_aller <- round(cycle / 4);
		moy_cycle_aller <- round(mean(troupeau collect (each.cycle_aller_micro)));
		std_cycle_aller_micro <- standard_deviation(troupeau collect (each.cycle_aller_micro));

		//do pause;
	}

	reflex trps_terr_acc when: cpt_trp_retour >= nb_trp - 2 {
		nb_cycle_retour <- round(cycle / 4 - nb_cycle_aller);
		moy_cycle_retour <- round(mean(troupeau collect (each.cycle_retour_micro)));
		std_cycle_retour_micro <- standard_deviation(troupeau collect (each.cycle_retour_micro));
		//do pause;
	}

	reflex update_comptage_trp {
		cpt_trp_aller <- sum(troupeau collect (each.presence_terr_acc));
		cpt_trp_retour <- sum(troupeau collect (each.presence_terr_orig));
		if nettoyage {
			nb_trp <- length(troupeau);
			nettoyage <- false;
		}

	}

	reflex remplissage_grille {
		ask grille {
			if length(troupeau inside (self)) != 0 {
				nb_trp_inside <- length(troupeau inside (self));
				s_nb_trp_inside <- s_nb_trp_inside + length(troupeau inside (self));
			}

		}

	}

}

//******************************************************************
grid grille width: 36 height: 38 neighbors: 8 {
	int nb_trp_inside;
	int s_nb_trp_inside;
	bool cellule_bleu <- true;
	bool enregistrement <- true;
	bool enregistrement2 <- true;
	//****************** effet de la végétation dans la grille *************************
	float r <- qt_pluie != 0 ? (4.1 * qt_pluie - 515) * 10 * 10 * 100 : 1; // quantité de végétation en fonction de l'équation de Boudhet 
	float r_init <- (4.1 * qt_pluie - 515) * 10 * 10 * 100;
	float seuil_r <- (q_seuil_r / 100) * r_init; //le seuil de végétation est le 10 de la végétation initiale
	bool ind_presence_bio <- r > 0 ? true : false;
	float impact_trp_aller <- 0.0;
	float impact_trp_retour <- 0.0;

	init {
		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
	}

	reflex impact_trp_vege_grille when: every(step) {
		if cpt_trp_aller <= nb_trp and nb_trp_inside != 0 {
			impact_trp_aller <- with_precision((1 - r / r_init) * 100, 10);
		}

		if cpt_trp_aller >= nb_trp - 1 and nb_trp_inside != 0 {
			impact_trp_retour <- with_precision((1 - r / r_init) * 100, 10);
		}

	}

	reflex densite_aller when: cpt_trp_aller = nb_trp and enregistrement and is_batch {
		if veto_march {
			save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_aller] to: 'veterinaire_aller_50r.csv' rewrite: false type: 'csv';
		} else {
			save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_aller] to: 'market_aller_50r.csv' rewrite: false type: 'csv';
		}

		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
		enregistrement <- false;
	}

	reflex densite_retour when: cpt_trp_retour = nb_trp and enregistrement2 and is_batch {
		if veto_march {
		//save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_retour] to: 'veterinaire_retour_50r.csv' rewrite: false type: 'csv';
		} else {
		//save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_retour] to: 'market_retour_50r.csv' rewrite: false type: 'csv';
		}

		enregistrement2 <- !enregistrement2;
	}

}

//********troupeau********************
species troupeau skills: [moving] {
	float step <- 6 #hours;
	//***************** effectif trp ******************
	int eff_bovin <- poisson(eff_bovin_moy);
	float acc_bovin <- gauss(acc_bovin_moy, 0.1);
	int eff_ovin <- poisson(eff_ovin_moy);
	float acc_ovin <- gauss(acc_ovin_moy, 0.1);
	int eff_caprin <- poisson(eff_caprin_moy);
	float acc_caprin <- gauss(acc_caprin_moy, 0.1);
	float com_bovin <- com_bovin_moy;
	float com_ovin <- com_ovin_moy;
	float com_caprin <- com_caprin_moy;
	float cons_jour <- 0.0;
	bool signal;
	//*****************************************
	grille my_cell <- one_of(grille inside
	polygon([{40326.01028006832, 4481.029275019886, 0.0}, {44771.060577107244, 35596.38135429192, 0.0}, {89777.19483462576, 39485.80036420096, 0.0}, {162009.2621615074, 33929.48749290244, 0.0}, {212016.07800319465, 66711.73343356396, 0.0}, {259244.73740923265, 96160.19165144651, 0.0}, {275913.67602312844, 122274.86214654986, 0.0}, {324253.5980034262, 108939.71125543327, 0.0}, {303139.60909249156, 61155.42056226544, 0.0}, {217572.39087449329, 12815.498581967782, 0.0}, {120336.91562676802, -2186.546170538524, 0.0}, {40326.01028006832, 4481.029275019886, 0.0}]));
	//*************************************************
	date date_dep <- starting_date;
	point
	terr_orig <- any_location_in(polygon([{40326.01028006832, 4481.029275019886, 0.0}, {44771.060577107244, 35596.38135429192, 0.0}, {89777.19483462576, 39485.80036420096, 0.0}, {162009.2621615074, 33929.48749290244, 0.0}, {212016.07800319465, 66711.73343356396, 0.0}, {259244.73740923265, 96160.19165144651, 0.0}, {275913.67602312844, 122274.86214654986, 0.0}, {324253.5980034262, 108939.71125543327, 0.0}, {303139.60909249156, 61155.42056226544, 0.0}, {217572.39087449329, 12815.498581967782, 0.0}, {120336.91562676802, -2186.546170538524, 0.0}, {40326.01028006832, 4481.029275019886, 0.0}])).location;
	point
	terr_acc <- any_location_in(polygon([{0.0, 277584.84043412283, 0.0}, {16418.409398190677, 314330.80432531144, 0.0}, {81310.21797199198, 304948.8560977739, 0.0}, {106328.74657875876, 283057.6435668529, 0.0}, {129001.78812864108, 250220.82477047155, 0.0}, {59419.005441071, 225984.12518266635, 0.0}, {0.0, 277584.84043412283, 0.0}])).location;
	float p <- gauss(15.5, 2) #km / #days;
	float speed <- p > 13.5 #km / #days ? p : 13.5 #km / #days;
	string objectif;
	string phase <- 'aller';
	int m <- rnd(1, 360);
	point the_target <- terr_acc; //a linitialisation il sait quil va au terroir d'accueil
	bool soin <- flip(0.7) ? true : false;
	int jour_veto_trp <- rnd(jour_veto);
	int k1 <- 0;
	int presence_terr_acc <- 0;
	int presence_terr_orig <- 0;
	point ancienne_cible_veto;
	point position_veto;
	int cycle_aller_micro <- 0;
	int cycle_retour_micro <- 0;

	init {
		my_cell.location <- terr_orig;
		location <- my_cell.location; //afin que la position du trp et la var cellule d'espace pointe à la meme adresse
		cpt_trp_aller <- 0;
		cpt_trp_retour <- 0;
	}

	//bool verif_veto <- false;
	//************************************************* variable pour l'exploration
	reflex consommation_hour when: every(step) {
		cons_jour <- (self.eff_bovin * self.com_bovin + self.eff_ovin * self.com_ovin + self.eff_caprin * self.com_caprin);
	}

	reflex trp_broutte when: every(step) {
		if my_cell.ind_presence_bio {
			self.signal <- false;
			my_cell.r <- my_cell.r - self.cons_jour;
		} else {
			self.signal <- true;
		}

	}

	//---------------------------------------------------------------- Les effectifs d'animaux --------------------------------
	reflex dynamique_population when: cpt_trp_aller = nb_trp {
	// on fait croître les effectifs d'un coût afin de pouvoir observer l'impact sur le retour. Quand les trps arivent en ZA il retourne, pas le temps de laisser le reflex tourner
		eff_bovin <- round(eff_bovin + (acc_bovin / 200) * eff_bovin);
		eff_ovin <- round(eff_ovin + (acc_ovin / 200) * eff_ovin);
		eff_caprin <- round(eff_caprin + (acc_caprin / 200) * eff_caprin);
	}
	//-------------------------------- déplacement du trp -------------------------------------
	reflex compt_jour {
		if m < 361 {
			m <- m + 1;
		} else {
			m <- 1;
			soin <- !soin; //je met à jour le carnet de vaccination du troupeau
		}

	}

	reflex position_veto when: veto_march and self.soin = false {
		ask veterinaire where (each distance_to self <= dist_veto) {
			myself.objectif <- 'veterinaire';
			myself.position_veto <- one_of(self.location);
		}

	}

	reflex position_marche when: veto_march = false and every(jour_marche #day) {
		ask marche where (each distance_to self <= dist_veto) {
			myself.objectif <- 'veterinaire';
			myself.position_veto <- one_of(self.location);
		}

	}

	reflex aller_veterinaire_ou_marche when: objectif = 'veterinaire' {
		ancienne_cible_veto <- the_target;
		//write ancienne_cible_veto;
		the_target <- position_veto;
		//write the_target = ancienne_cible;
		objectif <- 'deplacement_veto';
	}

	reflex chez_veterinaire when: objectif = 'deplacement_veto' {
		if location = position_veto {
			if veto_march {
				the_target <- nil;
				k1 <- k1 + 1;
				if k1 >= jour_veto_trp {
					soin <- true;
					objectif <- 'quitter';
				}

			} else { // on est dans le cas des marchés
				objectif <- 'quitter';
			}

		}

	}

	reflex quitter_veterinaire when: objectif = 'quitter' {
		the_target <- terr_acc;
		k1 <- 0;
	}

	reflex phase_transitoire when: cpt_trp_aller <= nb_trp + 1 {
		if cpt_trp_aller >= nb_trp {
			phase <- 'retour';
			the_target <- terr_orig;
		}

	}

	bool bool_cycle_aller <- true;
	bool bool_cycle_retour <- true;

	reflex deplacement when: the_target != nil and current_date >= date_dep {
		do goto target: the_target;
		if location = terr_acc { //location represente le terroir d'accueil, il s'y arrete temporairement je vais devoir remplacer
			the_target <- nil;
			self.presence_terr_acc <- 1;
			p <- gauss(17.5, 2) #km / #days;
			self.speed <- p > 15 #km / #days ? p : 15 #km / #days;
			if bool_cycle_aller {
				cycle_aller_micro <- round(cycle / 4);
				bool_cycle_aller <- false;
			}

		}

		if location = terr_orig {
			presence_terr_orig <- 1;
			the_target <- nil;
			if bool_cycle_retour {
				cycle_retour_micro <- round(cycle / 4 - nb_cycle_aller);
				bool_cycle_retour <- false;
			}

		}

		if self.terr_orig.y > self.location.y and phase != 'retour' {
			nettoyage <- true;
			do die;
		}

	}

	aspect asp_trp {
		draw circle(3000) color: #black;
		if soin and veto_march {
			draw circle(3000) color: #black;
		} else {
			draw circle(3000) color: #red;
		}

	}

}

//***************************************************************
species zone {

	aspect asp_zone {
		draw shape color: #gamaorange;
	}

}
//************************************* Marche ******************
species marche {

	aspect market {
		draw square(6000) color: #orange;
	}

}

//****************************** veto ***************************
species veterinaire {

	aspect veto {
		draw square(6000) color: #maroon;
	}

}

//*************************************************** zone batie **********************************
species zone_batie {
	bool destruction <- true;

	reflex nettoyage_forage when: destruction {
		if veto_march {
			ask veterinaire where (each overlaps self) {
				do die;
			}

		} else {
			ask marche where (each overlaps self) {
				do die;
			}

		}

		destruction <- false;
	}

}

//**************************************************************
experiment modele_veto {
/*parameter 'distance(m)' var: dist_veto <- 30.0 #km category: 'Veterinaire';
	parameter 'jour(s)' var: jour_veto <- 7 min: 0 max: 7 category: 'Veterinaire' step: 1;

	init {
		create simulation with: [qt_pluie::500.0];
		create simulation with: [qt_pluie::350.0];
		create simulation with: [qt_pluie::200.0];
	}

	permanent {
		display multi_graphique {
			chart "Impact of herds on the vegetation" type: series {
				loop s over: simulations {
					data "Impact of herds on vegetation " + s.plvt value: s.impt_trp_veg marker: false style: line thickness: 4;
				}

			}

		}

	}*/
	output {
		layout #split;
		monitor "Pluviométrie " value: plvt refresh: every(1 #month);
		display mvt_forage type: java2D {
		//species vegetation aspect: asp_vegetation_base;
			grid grille border: #grey;
			species zone aspect: asp_zone transparency: 0.3;
			species veterinaire aspect: veto;
			species marche aspect: market;
			species troupeau aspect: asp_trp;
		}

		/*display graf {
			chart "Quantitté hebdomadaire de végétation" type: series size: {1, 0.5} position: {0, 0.5} {
			//datalist ["Végétation"] value: [sum(grille collect (each.r))] color: [#green];
			//data "Troupeau" value: s_cons_jour color: #blue;
				data "Impact of herds on vegetation" value: impt_trp_veg;
			}

		}*/
//utile avec le batch
		/*display chemin_transhumants type: java2D {
			chart '' type: scatter size: {1, 0.5} position: {0, 0.5} {
				data 'nb_cycles' value: mean(simulations collect (each.cycle));
				data 'd_veto(km)' value: simulations collect (each.dist_veto / 1000) accumulate_values: true;
				data 'days_veto' value: simulations collect (each.jour_veto) accumulate_values: true line_visible: true;
			}

			graphics 'my_graphics' position: {0, 0.7} {
				draw string('day_veto=' + jour_veto) at: {266000, 45000} font: font('Helvetica', 18, #plain) color: #black;
				draw string('d_veto (km)=' + dist_veto / 1000) at: {244000, 75000} font: font('Helvetica', 18, #plain) color: #black;
			}

		}*/
	}

}

//***************************************************************** Veterinaire impact temporelle *********************************************************
/*experiment analyse_nb_jour_veto type: batch repeat: 50 keep_seed: true until: cpt_trp_retour = nb_trp {
	parameter 'Batch mode' var: is_batch <- true;
	parameter 'distance(m)' var: dist_veto <- 30.0 #km category: 'Veterinaire';
	parameter 'jour(s)' var: jour_veto <- 0 min: 0 max: 7 category: 'Veterinaire' step: 1;
	permanent {
		display jour_veterinaire background: #white refresh: every(cycle) {
			chart '' type: scatter size: {1, 0.5} {
				data 'nb_cycles' value: mean(simulations collect (each.cycle / 4)); // on divise par 4 pour avoir la donnée en jour
				//data 'd_veto(km)' value: simulations collect (each.dist_veto / 1000) accumulate_values: true;
				data 'days_veto' value: simulations collect (each.jour_veto) accumulate_values: true line_visible: true;
			}

			graphics 'my_graphics' {
				draw string('day_veto=' + jour_veto) at: {266000, 45000} font: font('Helvetica', 18, #plain) color: #black;
				//draw string('d_veto (km)=' + dist_veto / 1000) at: {244000, 75000} font: font('Helvetica', 18, #plain) color: #black;
			}

		}

	}

}*/
//*********************************** Sauvegarde du SIG **************************************
experiment sauv_donne type: batch repeat: 50 until: cpt_trp_retour >= nb_trp {
	parameter 'Batch mode' var: is_batch <- true;
	parameter 'true=veto, false=marche' var: veto_march;

	reflex save {
		if veto_march {
			save [sum(simulations collect (each.nb_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_aller)), sum(simulations mean_of
			(each.moy_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_aller)), sum(simulations collect
			(each.propor_utilisa_aller)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_aller)), simulation.dist_veto / 1000] rewrite: false to:
			"cycle_veterinaire_aller.csv" type: csv;
			save [sum(simulations collect (each.nb_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_retour)), sum(simulations collect
			(each.moy_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_retour)), sum(simulations collect
			(each.propor_utilisa_retour)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_retour)), simulation.dist_veto / 1000] rewrite: false to:
			"cycle_veterinaire_retour.csv" type: csv;
		} else {
			save [sum(simulations collect (each.nb_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_aller)), sum(simulations mean_of
			(each.moy_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_aller)), sum(simulations collect
			(each.propor_utilisa_aller)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_aller)), simulation.dist_veto / 1000] rewrite: false to:
			"bis_cycle_market_aller.csv" type: csv;
			save [sum(simulations collect (each.nb_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_retour)), sum(simulations collect
			(each.moy_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_retour)), sum(simulations collect
			(each.propor_utilisa_retour)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_retour)), simulation.dist_veto / 1000] rewrite: false to:
			"bis_cycle_market_retour.csv" type: csv;
		}

	}

}
//************************************************************************************************************
experiment analyse_veto type: batch repeat: 20 keep_seed: true until: cpt_trp_retour >= nb_trp {
	parameter 'Batch mode' var: is_batch_analyse <- true;
	parameter 'true=veto, false=marche' var: veto_march <- true category: 'veto_marche';
	parameter 'distance(m)' var: dist_veto <- 15.0 #km min: 15.0 #km max: 25 #km step: 5 #km category: 'Vétérinarian';
	parameter 'jour_veto' var: jour_veto <- 4 min: 0 max: 6 step: 1 category: 'Vétérinarian';
}
//-------------------------------------------------------
experiment analyse_marche type: batch repeat: 20 keep_seed: true until: cpt_trp_retour >= nb_trp {
	parameter 'Batch mode' var: is_batch_analyse <- true;
	parameter 'true=veto, false=marche' var: veto_march <- false category: 'veto_marche';
	parameter 'distance(m)' var: dist_veto <- 17.0 #km min: 15.0 #km max: 25 #km step: 2 #km category: 'Marché';
	parameter 'jour_entre_2_marche' var: jour_marche min: 4 max: 14 step: 1 category: 'Marché';
	//-----------------------------------------------------------------------
}
