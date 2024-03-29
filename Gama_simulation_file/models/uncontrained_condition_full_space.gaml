model temoin

global {
	file shape_file_appetence <- file("../includes/morpho_pedo_carre.shp");
	//file shape_file_forage <- file("../includes/forage_carre.shp");
	file shape_file_departement <- file("../includes/departement.shp");
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
	float com_bovin_moy <- 4.5 / 4 min: 1.0 / 4 parameter: 'Bovin' category: "Consommation journalière biomasse";
	float com_ovin_moy <- 1.5 / 4 min: 1.0 / 4 parameter: 'Ovin' category: "Consommation journalière biomasse";
	float com_caprin_moy <- 1.5 / 4 min: 1.0 / 4 parameter: 'Caprin' category: "Consommation journalière biomasse";
	float impt_trp_veg <- 0.0;
	float r_g <- 0.0;

	//------------------------------------
	int nb_trp <- 200 min: 3;
	//int nb_jour_transh;
	int cpt_trp_aller;
	int cpt_trp_retour;
	float propor_utilisa_aller <- 0.0;
	float propor_utilisa_retour <- 0.0;
	//************** interaction troupeau vegetation grille ********************
	float largeur_cellule <- 9.6 #km;
	float hauteur_cellule <- 9.6 #km;
	bool couleur_veg <- true;
	int cpt <- 0;
	bool nettoyage <- false;
	//************************ interaction trp_vegetation_grille ****************
	float q_seuil_r <- 25.0 min: 0.0 max: 100.0 parameter: "Sueil_biomasse" category: "Impact troupeau-végétation"; //le seuil de végétation, ce sueil(25%) est tiré de Dia et Duponnois:désertification
	float qt_pluie <- rnd(100, 500.0) min: 100.0 parameter: 'Pluviométrie'; //rnd(100.0, 550.0) min: 50.0 max: 550.0 parameter: "Pluie(mm)" category: "Climat";
	string plvt;
	//******************** batch experiment ************************
	int nb_cycle <- 0;
	int nb_cycle_aller <- 0;
	int nb_cycle_retour <- 0;
	int moy_cycle_aller <- 0;
	int moy_cycle_retour <- 0;
	float std_cycle_aller_micro <- 0.0;
	float std_cycle_retour_micro <- 0.0;
	bool is_batch <- false;
	bool is_batch_analyse <- true;

	init {
	//create forage from: shape_file_forage with: [debit::float(get("Débit_expl"))];
		create vegetation from: shape_file_appetence with: [pasto::string(get("PASTORAL")), aire::float(get("AREA"))] {
			if pasto = "N" {
			// nettoyage de l'espace de simulation, permet davoir uniquement les zones souhaitées en rouges
				if self overlaps
				(polygon([{40326.01028006832, 4481.029275019886, 0.0}, {44771.060577107244, 35596.38135429192, 0.0}, {89777.19483462576, 39485.80036420096, 0.0}, {162009.2621615074, 33929.48749290244, 0.0}, {212016.07800319465, 66711.73343356396, 0.0}, {259244.73740923265, 96160.19165144651, 0.0}, {275913.67602312844, 122274.86214654986, 0.0}, {324253.5980034262, 108939.71125543327, 0.0}, {303139.60909249156, 61155.42056226544, 0.0}, {217572.39087449329, 12815.498581967782, 0.0}, {120336.91562676802, -2186.546170538524, 0.0}, {40326.01028006832, 4481.029275019886, 0.0}]))
				{
					color_vegetation <- one_of(#darkgreen, #green, #lime);
				} else {
					color_vegetation <- #red;
				}

			} else if pasto = "P1" {
				color_vegetation <- #darkgreen; // pâturage généralement de bonne qualité
			} else if pasto = "P2" {
				color_vegetation <- #green; // pâturage généralement de qualité moyenne
			} else if pasto = "P3" {
				color_vegetation <- #lime; // pâturage généralement de qualité médiocre ou faible
			} else {
				color_vegetation <- #aqua; // pâturage uniquement exploitable en saison sèche, inondable
			} }

			//create roads from: shape_file_roads with: [sous_type::string(get("SOUSTYP"))];
		create zone from: shape_file_zone;
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
		} } //************** fin du champ init *********
	reflex trp_veg when: every(step) {
		r_g <- sum(grille collect (each.r));
		impt_trp_veg <- with_precision((1 - r_g / sum(grille collect (each.r_init))) * 100, 10);
	}
	//---------------------------------------------------
	reflex save_dveg when: is_batch_analyse and cpt_trp_retour >= nb_trp - 1 {
		save [nb_cycle_aller, moy_cycle_aller, propor_utilisa_aller, nb_trp] rewrite: false to: "dist_temoin_aller.csv" type: csv;
		save [nb_cycle_retour, moy_cycle_retour, propor_utilisa_retour, nb_trp] rewrite: false to: "dist_temoin_retour.csv" type: csv;
	}

	reflex utilisation_espace_aller when: (nb_trp - 5 <= cpt_trp_aller and cpt_trp_aller < nb_trp - 1) {
		propor_utilisa_aller <- (length(grille where (each.s_nb_trp_inside >= 1)) / length(grille)) * 100;
	}

	reflex utilisation_espace_retour when: nb_trp - 5 <= cpt_trp_retour and cpt_trp_retour <= nb_trp {
		propor_utilisa_retour <- length(grille where (each.s_nb_trp_inside >= 1)) / length(grille) * 100;
	}

	reflex trp_dure_orig_acc when: nb_trp - 5 <= cpt_trp_aller and cpt_trp_aller < nb_trp {
		nb_cycle_aller <- round(cycle / 4);
		moy_cycle_aller <- round(mean(troupeau collect (each.cycle_aller_micro)));
		std_cycle_aller_micro <- standard_deviation(troupeau collect (each.cycle_aller_micro));
		//write nb_cycle_aller;
		//do pause;
	}

	reflex trps_terr_acc when: cpt_trp_retour >= nb_trp - 5 {
		nb_cycle_retour <- round(cycle / 4 - nb_cycle_aller);
		moy_cycle_retour <- round(mean(troupeau collect (each.cycle_retour_micro)));
		std_cycle_retour_micro <- standard_deviation(troupeau collect (each.cycle_retour_micro));
		nb_cycle <- nb_cycle_aller + nb_cycle_retour;
		/*if cpt_trp_retour >= nb_trp {
			do pause;
		}*/
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
				self.nb_trp_inside <- length(troupeau inside (self));
				self.s_nb_trp_inside <- self.s_nb_trp_inside + length(troupeau inside (self));
				//color <- rgb(nb_trp_inside mod 255);
			}

		}

	}

	reflex grille_couleur_vegetation when: couleur_veg {
		ask grille {
			ask vegetation overlapping (self) {
				myself.color <- self.color_vegetation;
			}

		}

		couleur_veg <- false;
	}

	//-------------------- Fin du global --------------------------
}

//**************************************************** troupeau **************************************
species troupeau skills: [moving] {
	float step <- 6 #hours;
	float p <- gauss(15.5, 2) #km / #days;
	float speed <- p > 13.5 #km / #days ? p : 13.5 #km / #days;
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
	//***************************************************************
	date date_dep <- starting_date; //date([2020, 10, flip(0.5) ? j1 : j2, rnd(7, 8), 0]);
	point
	terr_orig <- any_location_in(polygon([{40326.01028006832, 4481.029275019886, 0.0}, {44771.060577107244, 35596.38135429192, 0.0}, {89777.19483462576, 39485.80036420096, 0.0}, {162009.2621615074, 33929.48749290244, 0.0}, {212016.07800319465, 66711.73343356396, 0.0}, {259244.73740923265, 96160.19165144651, 0.0}, {275913.67602312844, 122274.86214654986, 0.0}, {324253.5980034262, 108939.71125543327, 0.0}, {303139.60909249156, 61155.42056226544, 0.0}, {217572.39087449329, 12815.498581967782, 0.0}, {120336.91562676802, -2186.546170538524, 0.0}, {40326.01028006832, 4481.029275019886, 0.0}])).location;
	point
	terr_acc <- any_location_in(polygon([{0.0, 277584.84043412283, 0.0}, {16418.409398190677, 314330.80432531144, 0.0}, {81310.21797199198, 304948.8560977739, 0.0}, {106328.74657875876, 283057.6435668529, 0.0}, {129001.78812864108, 250220.82477047155, 0.0}, {59419.005441071, 225984.12518266635, 0.0}, {0.0, 277584.84043412283, 0.0}])).location;
	string objectif;
	string phase <- 'aller';
	point the_target <- terr_acc; //a linitialisation il sait quil va au terroir d'accueil
	bool test_retour <- true;
	int presence_terr_acc <- 0;
	int presence_terr_orig <- 0;
	int cycle_aller_micro <- 0;
	int cycle_retour_micro <- 0;

	init {
		my_cell.location <- terr_orig;
		location <- my_cell.location; //afin que la position du trp et la var cellule d'espace pointe à la meme adresse
		cpt_trp_aller <- 0;
		cpt_trp_retour <- 0;
	}

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
	//---------------------------------- deplacement du trp ------------------------------
	reflex phase_transitoire when: cpt_trp_aller >= nb_trp and test_retour {
	//if cpt_trp_aller >= nb_trp {
		phase <- 'retour';
		the_target <- terr_orig;
		test_retour <- !test_retour;
		//}

	}

	bool bool_cycle_aller <- true;
	bool bool_cycle_retour <- true;

	reflex deplacement when: the_target != nil and current_date >= date_dep {
		do goto target: the_target;
		my_cell.location <- self.location;
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

		if location = terr_orig and phase = 'retour' {
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
	}

}

//*************************************************** végétation ***********************************
species vegetation {
	string pasto;
	float aire;
	rgb color_vegetation;

	aspect asp_vegetation_base {
		draw shape color: color_vegetation;
	}

}
//***************************************************** Zone ****************************************
species zone schedules: [] {

	aspect asp_zone {
		draw shape color: #gamaorange;
	}

}

//****************************************************grille ****************************************
grid grille width: 36 height: 38 neighbors: 8 {
	int nb_trp_inside;
	int s_nb_trp_inside;
	bool enregistrement <- true;
	bool enregistrement2 <- true;
	//****************** effet de la végétation dans la grille *************************
	float r <- qt_pluie != 0 ? (4.1 * qt_pluie - 515) * largeur_cellule * hauteur_cellule : 1; // quantité de végétation en fonction de l'équation de Boudhet 
	float r_init <- qt_pluie != 0 ? (4.1 * qt_pluie - 515) * largeur_cellule * hauteur_cellule : 1;
	float seuil_r <- (q_seuil_r / 100) * r_init; //le seuil de végétation est le 10 de la végétation initiale
	bool ind_presence_bio <- r > 0 ? true : false;
	float impact_trp_aller <- 0.0;
	float impact_trp_retour <- 0.0;
	int a11;
	int c11;
	int a22;
	int c22;

	init {
		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
	}

	reflex impact_trp_vegetation_grille when: every(step) {
		if cpt_trp_aller <= nb_trp and nb_trp_inside != 0 {
			impact_trp_aller <- with_precision((1 - r / r_init) * 100, 10);
		}

		if cpt_trp_aller >= nb_trp - 1 and nb_trp_inside != 0 {
			impact_trp_retour <- with_precision((1 - r / r_init) * 100, 10);
		}

	}

	//--------------------------------------------------------------------------
	reflex densite_aller when: cpt_trp_aller >= nb_trp - 1 and enregistrement and is_batch {
		save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_aller] to: 'temoin_aller_50r.csv' rewrite: false type: 'csv';
		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
		enregistrement <- false;
	}

	reflex densite_retour when: cpt_trp_retour >= nb_trp - 1 and enregistrement2 and is_batch {
		save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_retour] to: 'temoin_retour_50r.csv' rewrite: false type: 'csv';
		enregistrement2 <- !enregistrement2;
	}

}

experiment temoin type: gui {
	output {
		layout #split;
		monitor "Pluviométrie " value: plvt refresh: every(1 #month);
		display mvt_vegetation type: java2D {
			grid grille border: #grey;
			//species vegetation aspect: asp_vegetation_base;
			species zone aspect: asp_zone transparency: 0.3;
			species troupeau aspect: asp_trp;
		}

		display graf type: java2D {
			chart "Quantitté hebdomadaire de végétation" type: series size: {1, 0.5} position: {0, 0.5} {
			//datalist ["Végétation"] value: [sum(grille collect (each.r))] color: [#green];
			//data "Troupeau" value: s_cons_jour color: #blue;
			//ajouter le graphique de l'impact de la micro faune
				data "Impact of herds on vegetation" value: impt_trp_veg;
			}

		}

	}

}

//******************************* batch *************************
experiment sauv_donne type: batch repeat: 50 until: cpt_trp_retour >= nb_trp {
	parameter 'Batch mode' var: is_batch <- true;

	//*************** sauvegarde du nombre de cycle moyen dans un fichier
	reflex save {
		save [sum(simulations collect (each.nb_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_aller)), sum(simulations collect
		(each.moy_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_aller)), sum(simulations collect
		(each.propor_utilisa_aller)) / length(simulations), sum(simulations collect (each.propor_utilisa_aller)) / length(simulations), standard_deviation(simulations collect
		(each.propor_utilisa_aller))] to: "cycle_temoin_aller_50r.csv" type: csv;
		save [sum(simulations collect (each.nb_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_retour)), sum(simulations collect
		(each.moy_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_retour)), sum(simulations collect
		(each.propor_utilisa_aller)) / length(simulations), sum(simulations collect (each.propor_utilisa_retour)) / length(simulations), standard_deviation(simulations collect
		(each.propor_utilisa_retour))] to: "cycle_temoin_retour_50r.csv" type: csv;
		save [sum(simulations collect (each.nb_cycle)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle))] to: "cycle_temoin_50r.txt" type: text;
	}

}

