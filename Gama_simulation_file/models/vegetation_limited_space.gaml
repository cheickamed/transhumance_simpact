/**
* Name: vegetationespacecirconscit
* Based on the internal empty template. 
* Author: Cheick Amed Diloma Gabriel TRAORE
* Tags: 
*/
model vegetationespacecirconscit

/* Insert your model definition here */
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
	float d_veg <- 25 #km min: 15.0 #km max: 30 #km;
	bool couleur_veg <- true;
	int cpt <- 0;
	bool nb_1_bloc <- false parameter: "nb_block 1=true or 2=false";
	//************************ interaction trp_vegetation_grille ****************
	float q_seuil_r <- 25.0 min: 0.0 max: 100.0 parameter: "Sueil_biomasse" category: "Impact troupeau-végétation"; //le seuil de végétation, ce sueil(25%) est tiré de Dia et Duponnois:désertification
	float qt_pluie <- rnd(100, 500.0) min: 100.0 parameter: 'Pluviométrie'; //rnd(100.0, 550.0) min: 50.0 max: 550.0 parameter: "Pluie(mm)" category: "Climat";
	string plvt;
	bool nettoyage <- false;
	//******************** batch experiment ************************
	int nb_cycle_aller <- 0;
	int nb_cycle_retour <- 0;
	int moy_cycle_aller <- 0;
	int moy_cycle_retour <- 0;
	float std_cycle_aller_micro <- 0.0;
	//float std_cycle_retour_micro <- 0.0;
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
	reflex save_dveg when: is_batch_analyse and (cpt_trp_retour >= nb_trp - 1 or cycle >= 548) {
		if nb_1_bloc {
			save [nb_cycle_aller, moy_cycle_aller, d_veg / 1000, propor_utilisa_aller] rewrite: false to: "dist_vegetation_circ_aller_1bloc.csv" type: csv;
			save [nb_cycle_retour, moy_cycle_retour, d_veg / 1000, propor_utilisa_retour] rewrite: false to: "dist_vegetation_circ_retour_1bloc.csv" type: csv;
		} else {
			save [nb_cycle_aller, moy_cycle_aller, d_veg / 1000, propor_utilisa_aller] rewrite: false to: "dist_vegetation_circ_aller_2bloc.csv" type: csv;
			save [nb_cycle_retour, moy_cycle_retour, d_veg / 1000, propor_utilisa_retour] rewrite: false to: "dist_vegetation_circ_retour_2bloc.csv" type: csv;
		}

	}

	reflex utilisation_espace_aller when: (nb_trp - 3 <= cpt_trp_aller and cpt_trp_aller < nb_trp - 1) {
		propor_utilisa_aller <- (length(grille where (each.s_nb_trp_inside >= 1)) / length(grille)) * 100;
	}

	reflex utilisation_espace_retour when: nb_trp - 3 <= cpt_trp_retour and cpt_trp_retour <= nb_trp {
		propor_utilisa_retour <- length(grille where (each.s_nb_trp_inside >= 1)) / length(grille) * 100;
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
		//std_cycle_retour_micro <- standard_deviation(troupeau collect (each.cycle_retour_micro));
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
				nb_trp_inside <- length(troupeau inside (self));
				s_nb_trp_inside <- s_nb_trp_inside + length(troupeau inside (self));
				//color_value <- rgb(nb_trp_inside mod 255);
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

	reflex trp_veg_evitement {
		ask troupeau where (each.objectif != 'evitement' and each.objectif != 'evitement2' and each.objectif != 'quitter_evitement' and each.objectif != 'terroir') {
			ask grille where (each.color = #red) {
				if myself overlaps (self) {
					myself.objectif <- 'evitement';
				}

			}

		}

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
	point position_vegetation; //position de vegetation quotidienne le plus proche du troupeau
	point position_evitement;
	list<point> position_vegetation_visite_aller;
	list<point> position_vegetation_visite_retour;
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
	reflex trp_vegetation when: (self.objectif != 'vegetation' or self.objectif != 'evitement2') {
		if phase = 'aller' {
			if my_cell.voisins != [] {
				ask my_cell.voisins {
					if one_of(self.color = #darkgreen and self.location >= myself.my_cell.location and self.location in myself.position_vegetation_visite_aller = false and
					self.location.y <= myself.terr_acc.y) {
						myself.objectif <- 'vegetation';
						//write self.pasto;
						myself.position_vegetation <- one_of(self.location);
						myself.position_vegetation_visite_aller <- myself.position_vegetation_visite_aller + [myself.position_vegetation];
					} else if one_of(self.color = #green and self.location >= myself.my_cell.location and self.location in myself.position_vegetation_visite_aller = false and
					self.location.y <= myself.terr_acc.y) {
						myself.objectif <- 'vegetation';
						myself.position_vegetation <- one_of(self.location);
						myself.position_vegetation_visite_aller <- myself.position_vegetation_visite_aller + [myself.position_vegetation];
					} else if one_of(self.color = #lime and self.location >= myself.my_cell.location and self.location in myself.position_vegetation_visite_aller = false and
					self.location.y <= myself.terr_acc.y) {
						myself.objectif <- 'vegetation';
						myself.position_vegetation <- one_of(self.location);
						myself.position_vegetation_visite_aller <- myself.position_vegetation_visite_aller + [myself.position_vegetation];
					} else {
					// il na rien trouver de bon, il continue vers son terroir d'accueil
						myself.the_target <- myself.terr_acc;
					} } } } else {
					//on est dans la phase retour
			if my_cell.voisins != [] {
				ask my_cell.voisins {
					if one_of(self.color = #darkgreen and self.location <= myself.my_cell.location and self.location in myself.position_vegetation_visite_retour = false and
					self.location.y >= myself.terr_orig.y) {
						myself.objectif <- 'vegetation';
						myself.position_vegetation <- self.location;
						myself.position_vegetation_visite_retour <- myself.position_vegetation_visite_retour + [myself.position_vegetation];
					} else if one_of(self.color = #green and self.location <= myself.my_cell.location and self.location in myself.position_vegetation_visite_retour = false and
					self.location.y >= myself.terr_orig.y) {
						myself.objectif <- 'vegetation';
						myself.position_vegetation <- self.location;
						myself.position_vegetation_visite_retour <- myself.position_vegetation_visite_retour + [myself.position_vegetation];
					} else if one_of(self.color = #lime and self.location <= myself.my_cell.location and self.location in myself.position_vegetation_visite_retour = false and
					self.location.y >= myself.terr_orig.y) {
						myself.objectif <- 'vegetation';
						myself.position_vegetation <- self.location;
						myself.position_vegetation_visite_retour <- myself.position_vegetation_visite_retour + [myself.position_vegetation];
					} else if one_of(self.color = #aqua and self.location <= myself.my_cell.location and self.location in myself.position_vegetation_visite_retour = false and
					self.location.y >= myself.terr_orig.y) {
						myself.objectif <- 'vegetation';
						myself.position_vegetation <- self.location;
						myself.position_vegetation_visite_retour <- myself.position_vegetation_visite_retour + [myself.position_vegetation];
					} else {
					// il na rien trouver de bon, il continue vers son terroir d'origine
						myself.the_target <- myself.terr_orig;
					} } } } }

	reflex trp_a_vegetation when: objectif = 'vegetation' {
		the_target <- position_vegetation;
		if location = position_vegetation {
		//write 'au pâturage';
			objectif <- 'quitter_vegetation';
		}

	}

	reflex troupeau_quitte_vegetation when: objectif = 'quitter_vegetation' {
		if phase = 'aller' {
			the_target <- terr_acc;
		}

		if phase = 'retour' {
			the_target <- terr_orig;
		}

	}

	reflex evitement when: (objectif = 'evitement' and objectif != 'evitement2') and objectif != 'terroir' {
		ask grille where (each.color != #red and (each.location in self.position_vegetation_visite_aller = false or each.location in self.position_vegetation_visite_retour = false))
		closest_to (self) {
			myself.objectif <- 'evitement2';
			myself.position_evitement <- one_of(self.location);
			myself.the_target <- myself.position_evitement;
			if myself.phase = 'aller' {
				myself.position_vegetation_visite_aller <- myself.position_vegetation_visite_aller + [myself.position_evitement]; // evite que le troupeau parte deux fois au même forage

			} else {
				myself.position_vegetation_visite_retour <- myself.position_vegetation_visite_retour + [myself.position_evitement]; // evite que le troupeau parte deux fois au même forage

			}

		}

	}

	reflex trp_evite when: objectif = 'evitement2' {
	//the_target <- position_evitement; //vas vers la cellule cible
		if location = position_evitement {
			objectif <- 'quitter_evitement';
			if phase = 'aller' {
				the_target <- terr_acc;
			}

			if phase = 'retour' {
				the_target <- terr_orig;
			}

		}

	}

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
				cycle_aller_micro <- cycle;
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

		// le terroir d'origine peut également être un lieu de mauvais pâturage
		if location.y >= terr_acc.y - 15 #km and phase != 'retour' {
			objectif <- 'terroir';
			the_target <- terr_acc;
		}

		if location.y <= terr_orig.y + 15 #km and phase = 'retour' {
			objectif <- 'terroir';
			the_target <- terr_orig;
		}

		if self.terr_orig.y > self.location.y and phase != 'retour' {
			nettoyage <- true;
			do die;
		}

	}

	aspect asp_trp {
		draw circle(3000) color: #black;
	} }

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
	bool suppres <- true;
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
	int a1;
	int a2;
	int c1;
	int c2;
	list<grille> voisins <- self neighbors_at (d_veg / largeur_cellule);
	//list<grille> voisins_evit <- self neighbors_at (d_veg / largeur_cellule) where (each.color != #red);
	init {
		if nb_1_bloc {
			a1 <- 12;
			c1 <- 23;
			//grid_y
			a2 <- 9;
			c2 <- 24;
		} else {
			a1 <- 0;
			a11 <- 18;
			c1 <- 5;
			c11 <- 23;
			//grid_y
			a2 <- 9;
			c2 <- 24;
		}
		//------------------------------------------------------
		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
	}

	reflex rectangle_grille when: suppres {
		if a1 <= grid_x and grid_x <= c1 and a2 <= grid_y and grid_y <= c2 {
			color <- #red;
		}

		suppres <- false;
		if nb_1_bloc = false and a11 <= grid_x and grid_x <= c11 and a2 <= grid_y and grid_y <= c2 {
			color <- #red;
		}

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
	reflex densite_aller when: cpt_trp_aller = nb_trp - 1 and enregistrement and is_batch {
		if nb_1_bloc {
			save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_aller] to: 'vegetation_circ_1bloc_aller_50r.csv' rewrite: false type: 'csv';
		} else {
			save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_aller] to: 'vegetation_circ_2bloc_aller_50r.csv' rewrite: false type: 'csv';
		}

		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
		enregistrement <- false;
	}

	reflex densite_retour when: cpt_trp_retour >= nb_trp - 1 and enregistrement2 and is_batch {
		if nb_1_bloc {
			save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_retour] to: 'vegetation_circ_1bloc_retour_50r.csv' rewrite: false type: 'csv';
		} else {
			save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_retour] to: 'vegetation_circ_2bloc_retour_50r.csv' rewrite: false type: 'csv';
		}

		enregistrement2 <- !enregistrement2;
	}

}

experiment vegetation_espace_circonscit type: gui {

/*	init {
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
experiment sauv_donne type: batch repeat: 50 until: cpt_trp_retour >= nb_trp or cycle >= 550 {
	parameter 'Batch mode' var: is_batch <- true;
	parameter 'nb_bloc 1=true 2=false' var: nb_1_bloc;
	//*************** sauvegarde du nombre de cycle moyen dans un fichier
	reflex save {
		if nb_1_bloc {
			save [sum(simulations collect (each.nb_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_aller)), sum(simulations collect
			(each.moy_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_aller)), sum(simulations collect
			(each.propor_utilisa_aller)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_aller))] to: "cycle_vegetation_circ_aller_1bloc_50r.txt" type:
			text;
			save [sum(simulations collect (each.nb_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_retour)), sum(simulations collect
			(each.moy_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_retour)), sum(simulations collect
			(each.propor_utilisa_retour)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_retour))] to: "cycle_vegetation_circ_retour_1bloc_50r.txt"
			type: text;
		} else {
			save [sum(simulations collect (each.nb_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_aller)), sum(simulations collect
			(each.moy_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_aller)), sum(simulations collect
			(each.propor_utilisa_aller)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_aller))] to: "cycle_vegetation_circ_aller_2bloc_50r.txt" type:
			text;
			save [sum(simulations collect (each.nb_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_retour)), sum(simulations collect
			(each.moy_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_retour)), sum(simulations collect
			(each.propor_utilisa_retour)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_retour))] to: "cycle_vegetation_circ_retour_2bloc_50r.txt"
			type: text;
		}

	}

}

/*experiment rectangle_deplace type: batch repeat: 50 until: cpt_trp_retour = nb_trp or cycle >= 1800 {
	parameter 'Batch mode' var: is_batch <- true;
	// nombre de simulation 11*28*2
	parameter 'Point_haut_x' var: a1 <- 0;
	parameter 'Point_haut_y' var: a2 <- 14 min: 14 max: 24;
	parameter 'Point_bas_x' var: c1 <- 0 min: 0 max: 27;
	parameter 'Point_bas_y' var: c2 <- 24;

	//*************** sauvegarde du nombre de cycle moyen dans un fichier
	reflex save {
		save [simulations mean_of (each.nb_cycle_aller), simulations mean_of (each.nb_cycle_retour), simulations mean_of (each.impt_trp_veg), simulations mean_of (each.impt_trp_veg)]
		to: "cycle_vegetation_circ_moy_carre_50r.txt" type: text;
	}

}*/
experiment analyse_dist_vegetation type: batch repeat: 20 keep_seed: true until: cpt_trp_retour >= nb_trp or cycle >= 550 {
	parameter 'Batch mode' var: is_batch_analyse <- true;
	parameter 'nb_bloc 1=true 2=false' var: nb_1_bloc;
	parameter 'distance(m)' var: d_veg <- 15 #km min: 15 #km max: 25 #km step: 5 #km category: 'Végétation';
}