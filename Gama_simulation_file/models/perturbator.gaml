/**
* Name: perturbateur
* Based on the internal empty template. 
* Author: Cheick Amed Diloma Gabriel TRAORE
* Tags: 
*/
model perturbateur

/* Insert your model definition here */
global {
	file shape_file_appetence <- file("../includes/morpho_pedo_carre.shp");
	file shape_file_hydro_line <- file("../includes/hydro_ligne_carre.shp");
	file shape_file_zone <- file("../includes/zonage_transhumance.shp");
	geometry shape <- envelope(shape_file_appetence);

	//---------------------------------------les paramètres----------------------------------------
	date starting_date <- date([2020, 10, 15, 7, 0]);
	float step <- 6 #hours;
	int eff_bovin_moy <- 111 min: 0 parameter: 'Bovin' category: "Effectif Ruminants";
	int eff_ovin_moy <- 257 min: 0 parameter: 'Ovin' category: "Effectif Ruminants";
	int eff_caprin_moy <- 69 min: 0 parameter: 'Caprin' category: "Effectif Ruminants";
	float acc_bovin_moy <- 2.5 min: -10.0 max: 10.0 parameter: 'Bovin' category: "Accroissement (%) Ruminants";
	float acc_ovin_moy <- 2.5 min: -10.0 max: 25.0 parameter: 'Ovin' category: "Accroissement (%) Ruminants";
	float acc_caprin_moy <- 3.5 min: -10.0 max: 35.0 parameter: 'Caprin' category: "Accroissement (%) Ruminants";
	float com_bovin_moy <- 2.5 / 4 min: 1.0 / 4 max: 5.5 parameter: 'Bovin' category: "Consommation joulanlière biomasse";
	float com_ovin_moy <- 1.5 / 4 min: 1.0 / 4 max: 5.5 parameter: 'Ovin' category: "Consommation joulanlière biomasse";
	float com_caprin_moy <- 1.5 / 4 min: 1.0 / 4 max: 5.5 parameter: 'Caprin' category: "Consommation joulanlière biomasse";
	//interaction veto-perturbateur
	float dist_perturb <- 10 #km min: 2 #km; //parameter: 'dis_surete(m)' category: 'Perturbateur';
	int jour_perturb <- 2 min: 1 max: 20 parameter: 'nb_jours' category: 'Perturbateur';
	bool is_batch <- false;
	bool is_batch_analyse <- false;
	int nb_trp <- 200 min: 3;
	int cpt_trp <- sum(troupeau collect (each.presence_ter_acc)) update: sum(troupeau collect (each.presence_ter_acc));
	//interaction trp_grille
	float largeur_cellule <- 9.6 #km;
	float hauteur_cellule <- 9.6 #km;
	float micro_faune <- 10.0 min: 0.0 max: 100.0 parameter: "Micro-faune" category: "Impact troupeau-végétation";
	float q_seuil_r <- 25.0 min: 0.0 max: 100.0 parameter: "Sueil_biomasse" category: "Impact troupeau-végétation"; //le seuil de végétation, ce sueil(25%) est tiré de Dia et Duponnois:désertification
	float qt_pluie <- rnd(100.0, 550.0) min: 50.0 max: 550.0 parameter: "Pluie(mm)" category: "Climat";
	string plvt;
	int cpt_trp_aller;
	int cpt_trp_retour;
	int nb_cycle_aller <- 0;
	int nb_cycle_retour <- 0;
	float propor_utilisa_aller <- 0.0;
	float propor_utilisa_retour <- 0.0;
	bool nettoyage <- false;
	//int nb_jour_transh;
	float s_cons_jour <- 0.0;
	float impt_trp_veg <- 0.0;
	float r_g <- 0.0;
	//----------------- le calendrier pastoral ------------------------------
	date date_mj_pluie <- date([2021, 10, 10]);
	date date_mj_biomasse <- date([2021, 10, 1]);
	date d_cetcelde <- date([2021, 5, 15]);
	date f_cetcelde <- date([2021, 6, 30]);
	date fin_sai_pluie <- date([2021, 9, 30]);
	date fin_transh_au_plus_tard <- date([2021, 7, 15]);
	bool couleur_veg <- true;
	int moy_cycle_aller <- 0;
	int moy_cycle_retour <- 0;
	float std_cycle_aller_micro <- 0.0;
	float std_cycle_retour_micro <- 0.0;

	init {
	//create forage from: shape_file_forage with: [forage_debit::float(get("Débit_expl"))];
	//create hydro_poly from: shape_file_hydro_poly;
	//create hydro_line from: shape_file_hydro_line;
		create vegetation from: shape_file_appetence with: [pasto::string(get("PASTORAL")), aire::float(get("AREA"))] {
			if pasto = "N" {
				color_vegetation <- rgb(165, 38, 10); //#red;
			} else if pasto = "P1" {
				color_vegetation <- rgb(58, 137, 35); // pâturage généralement de bonne qualité
			} else if pasto = "P2" {
				color_vegetation <- rgb(1, 215, 88); // pâturage généralement de qualité moyenne
			} else if pasto = "P3" {
				color_vegetation <- rgb(34, 120, 15); // pâturage généralement de qualité médiocre ou faible
			} else if pasto = "P4" {
				color_vegetation <- rgb(176, 242, 182); // pâturage uniquement exploitable en saison sèche, inondable
			} else {
				color_vegetation <- #white;
			} }

		create zone from: shape_file_zone;
		create bandi_contrainte number: 20;
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
		} } //fin de l'init du champ global

	//---------------------------------------------
	reflex trp_veg_grille when: every(step) {
		r_g <- sum(grille collect (each.r));
		impt_trp_veg <- with_precision((1 - r_g / sum(grille collect (each.r_init))) * 100, 10);
	}
	//-------------------------------------------------
	reflex save_dveg when: is_batch_analyse and cpt_trp_retour >= nb_trp - 1 {
		save [nb_cycle_aller, moy_cycle_aller, dist_perturb / 1000, jour_perturb, propor_utilisa_aller] rewrite: false to: "analyse_perturbateur_aller.csv" type: csv;
		save [nb_cycle_retour, moy_cycle_retour, dist_perturb / 1000, jour_perturb, propor_utilisa_retour] rewrite: false to: "analyse_perturbateur_retour.csv" type: csv;
	}

	reflex utilisation_espace_aller when: (nb_trp - 3 <= cpt_trp_aller and cpt_trp_aller < nb_trp - 1) {
		propor_utilisa_aller <- (length(grille where (each.s_nb_trp_inside >= 1)) / length(grille)) * 100;
	}

	reflex utilisation_espace_retour when: nb_trp - 2 <= cpt_trp_retour and cpt_trp_retour <= nb_trp {
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
				//color_value <- rgb(nb_trp_inside mod 255);
			}

		}

	} }

	//********troupeau********************
species troupeau skills: [moving] {
	float step <- 6 #hours;
	int eff_bovin <- poisson(eff_bovin_moy);
	float acc_bovin <- gauss(acc_bovin_moy, 1);
	int eff_ovin <- poisson(eff_ovin_moy);
	float acc_ovin <- gauss(acc_ovin_moy, 1);
	int eff_caprin <- poisson(eff_caprin_moy);
	float acc_caprin <- gauss(acc_caprin_moy, 1);
	float com_bovin <- gauss(com_bovin_moy, 1);
	float com_ovin <- gauss(com_ovin_moy, 1);
	float com_caprin <- gauss(com_caprin_moy, 1);
	date date_dep <- starting_date; //date([2020, 10, rnd(15, 30), 7, 0]);
	point
	terr_orig <- any_location_in(polygon([{40326.01028006832, 4481.029275019886, 0.0}, {44771.060577107244, 35596.38135429192, 0.0}, {89777.19483462576, 39485.80036420096, 0.0}, {162009.2621615074, 33929.48749290244, 0.0}, {212016.07800319465, 66711.73343356396, 0.0}, {259244.73740923265, 96160.19165144651, 0.0}, {275913.67602312844, 122274.86214654986, 0.0}, {324253.5980034262, 108939.71125543327, 0.0}, {303139.60909249156, 61155.42056226544, 0.0}, {217572.39087449329, 12815.498581967782, 0.0}, {120336.91562676802, -2186.546170538524, 0.0}, {40326.01028006832, 4481.029275019886, 0.0}])).location;
	point
	terr_acc <- any_location_in(polygon([{0.0, 277584.84043412283, 0.0}, {16418.409398190677, 314330.80432531144, 0.0}, {81310.21797199198, 304948.8560977739, 0.0}, {106328.74657875876, 283057.6435668529, 0.0}, {129001.78812864108, 250220.82477047155, 0.0}, {59419.005441071, 225984.12518266635, 0.0}, {0.0, 277584.84043412283, 0.0}])).location;
	float p <- gauss(15.5, 2) #km / #days;
	float speed <- p > 13.5 #km / #days ? p : 13.5 #km / #days;
	string objectif <- 'deplacement';
	int m <- rnd(1, 360);
	point the_target <- terr_acc; //a linitialisation il sait quil va au terroir d'accueil
	//bool fin_transhumance <- false;
	//int k1 <- 0; //cpt le nb de jour chez le veterinaire
	int k2; // cpt le nb de jour chez l'élément de reseau social
	int presence_ter_acc <- 0;
	list<point> point_chemin_aller <- nil;
	list<point> point_chemin_retour <- nil;
	bool cause_arret <- nil;
	//interaction avec le perturbateur
	float cons_jour <- 0.0;
	bool signal;
	//*****************************************
	grille my_cell <- one_of(grille inside
	polygon([{40326.01028006832, 4481.029275019886, 0.0}, {44771.060577107244, 35596.38135429192, 0.0}, {89777.19483462576, 39485.80036420096, 0.0}, {162009.2621615074, 33929.48749290244, 0.0}, {212016.07800319465, 66711.73343356396, 0.0}, {259244.73740923265, 96160.19165144651, 0.0}, {275913.67602312844, 122274.86214654986, 0.0}, {324253.5980034262, 108939.71125543327, 0.0}, {303139.60909249156, 61155.42056226544, 0.0}, {217572.39087449329, 12815.498581967782, 0.0}, {120336.91562676802, -2186.546170538524, 0.0}, {40326.01028006832, 4481.029275019886, 0.0}]));
	point modif_perturbation <- nil;
	int cpt_perturbation <- 0;
	int jour_perturbation <- rnd(1, jour_perturb);
	int presence_terr_acc <- 0;
	int presence_terr_orig <- 0;
	string phase <- 'aller';
	//bool test_retour <- true;
	int cycle_aller_micro <- 0;
	int cycle_retour_micro <- 0;

	init {
		my_cell.location <- terr_orig;
		location <- my_cell.location; //afin que la position du trp et la var cellule d'espace pointe à la meme adresse
		cpt_trp_aller <- 0;
		cpt_trp_retour <- 0;
	}
	//---------------------------------------------------------------- Les effectifs d'animaux --------------------------------
	reflex dynamique_population when: every(3 #month) {
		eff_bovin <- round(eff_bovin + (acc_bovin / 400) * eff_bovin);
		eff_ovin <- round(eff_ovin + (acc_ovin / 400) * eff_ovin);
		eff_caprin <- round(eff_caprin + (acc_caprin / 400) * eff_caprin);
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

	//---------------------------------------------------------------- déplacement du troupeau --------------------------------
	reflex perturbation when: every(step) {
		ask bandi_contrainte where (each distance_to self <= dist_perturb and self.cause_arret != true) {
			myself.objectif <- 'arret_perturbation';
			if self.status = 'voleur' {
				myself.cause_arret <- true;
			} else {
				myself.cause_arret <- false;
			}

		}

	}

	reflex arret_perturbation when: objectif = 'arret_perturbation' {
		if self.cause_arret {
			the_target <- nil;
			cpt_perturbation <- cpt_perturbation + 1;
			if cpt_perturbation >= jour_perturbation {
				objectif <- 'fin_perturbation';
			}

		} else {
			my_cell <- one_of(my_cell.neighbors2);
			objectif <- 'fin_perturbation';
		}

	}

	reflex reprise_chemin when: objectif = 'fin_perturbation' {
		cpt_perturbation <- 0;
		//the_target <- terr_acc;
		if phase = 'aller' {
			the_target <- terr_acc;
			objectif <- 'deplacement';
		}

		if phase = 'retour' {
			the_target <- terr_orig;
			objectif <- 'deplacement';
		}

		cause_arret <- false;
	}

	reflex phase_transitoire when: cpt_trp_aller >= nb_trp and phase != 'retour' {
	//if cpt_trp_aller >= nb_trp {
		phase <- 'retour';
		objectif <- 'retour';
		the_target <- terr_orig;
		//}

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
	}

}

//***************************
species vegetation {
	string pasto;
	float aire;
	rgb color_vegetation;

	aspect asp_vegetation_base {
		draw shape color: color_vegetation;
		//draw string(f) color: #black;
	}

}
//***************************************************************
species zone {

	aspect asp_zone {
		draw shape color: #gamaorange;
	}

}

species bandi_contrainte skills: [moving] {
//point
//location <- any_location_in(polygon([{-227093.6267222073, 1845400.1808777836, 0.0}, {-227093.85325360735, 1845400.223352421, 0.0}, {-227093.64088041979, 1845401.794914009, 0.0}, {-227092.7913876695, 1845402.1063946842, 0.0}, {-227092.2816920193, 1845402.0639200467, 0.0}, {-227091.64457245663, 1845401.7241229466, 0.0}, {-227091.5454649691, 1845400.7613644963, 0.0}, {-227092.4940652069, 1845400.1808777836, 0.0}, {-227093.6267222073, 1845400.1808777836, 0.0}, {-227093.6267222073, 1845400.1808777836, 0.0}]));
	string status <- flip(0.35) ? 'voleur' : 'antagoniste';

	reflex deplacement {
		if status = 'voleur' {
			do wander speed: 50 #km / #day;
		}

	}

	aspect asp_bandi {
		if status = 'voleur' {
			draw triangle(8000) color: #red;
		} else {
			draw square(8000) color: #red;
		}

	}

}
//**************************** grille *********************
grid grille width: 36 height: 38 neighbors: 8 {
	int nb_trp_inside;
	int s_nb_trp_inside;
	//bool cellule_bleu <- true;
	bool enregistrement <- true;
	bool enregistrement2 <- true;
	//****************** effet de la végétation dans la grille *************************
	float r <- qt_pluie != 0 ? (4.1 * qt_pluie - 515) * 10 #km * 10 #km * 100 : 1; // quantité de végétation en fonction de l'équation de Boudhet 
	float r_init <- (4.1 * qt_pluie - 515) * 10 #km * 10 #km * 100;
	float seuil_r <- (q_seuil_r / 100) * r_init; //le seuil de végétation est le 10 de la végétation initiale
	bool ind_presence_bio <- r > 0 ? true : false;
	float impact_trp_aller <- 0.0;
	float impact_trp_retour <- 0.0;
	list<grille> neighbors2 <- (self neighbors_at 1);

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

	reflex densite_aller when: cpt_trp_aller >= nb_trp - 1 and enregistrement and is_batch {
	//save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_aller] to: 'perturbateur_aller_50r.csv' rewrite: false type: 'csv';
		nb_trp_inside <- 0;
		s_nb_trp_inside <- 0;
		enregistrement <- false;
	}

	reflex densite_retour when: cpt_trp_retour = nb_trp - 1 and enregistrement2 and is_batch {
	//save [grid_x, grid_y, nb_trp_inside, s_nb_trp_inside, impact_trp_retour] to: 'perturbateur_retour_50r.csv' rewrite: false type: 'csv';
		enregistrement2 <- !enregistrement2;
	}

}

//**************************************************************
experiment Perturbateur {
/*init {
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
		monitor "Pluviométrie " value: plvt refresh: every(1 #month);
		display affichage_sig_zone type: java2D {
			grid grille border: #black;
			//species vegetation aspect: asp_vegetation_base;
			species zone aspect: asp_zone transparency: 0.3;
			//species hydro_line aspect: asp_hydro_line;
			species bandi_contrainte aspect: asp_bandi;
			species troupeau aspect: asp_trp;
		}

		display graf type: java2D {
			chart "Quantitté hebdomadaire de végétation" type: series size: {1, 0.5} position: {0, 0.5} {
			//datalist ["Végétation"] value: [sum(grille collect (each.r))] color: [#green];
			//data "Troupeau" value: s_cons_jour color: #blue;
			//ajouter le graphique de l'impact de la micro faune
				data "Impact of herds on vegetation" value: impt_trp_veg color: #green;
			}

		}

	}

}

experiment sauv_donne type: batch repeat: 50 until: cpt_trp_retour >= nb_trp {
	parameter 'Batch mode' var: is_batch <- true;
	//*************** sauvegarde du nombre de cycle moyen dans un fichier
	reflex save {
		save [sum(simulations collect (each.nb_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_aller)), sum(simulations collect
		(each.moy_cycle_aller)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_aller)), simulation.dist_perturb / 1000, sum(simulations collect
		(each.propor_utilisa_aller)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_aller))] rewrite: false to: "cycle_perturbateur_aller.csv" type:
		csv;
		save [sum(simulations collect (each.nb_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.nb_cycle_retour)), sum(simulations collect
		(each.moy_cycle_retour)) / length(simulations), standard_deviation(simulations collect (each.moy_cycle_retour)), simulation.dist_perturb / 1000, sum(simulations collect
		(each.propor_utilisa_retour)) / length(simulations), standard_deviation(simulations collect (each.propor_utilisa_retour))] rewrite: false to: "cycle_perturbateur_retour.csv"
		type: csv;
	}

}

//-------------------------------------
experiment analyse_dist_perturb type: batch repeat: 15 keep_seed: true until: cpt_trp_retour >= nb_trp {
	parameter 'Batch mode' var: is_batch_analyse <- true;
	parameter 'distance(m)' var: dist_perturb <- 5.0 #km min: 1.0 #km max: 20 #km step: 3 #km category: 'perturbateur';
	parameter 'nb_jours_perturb' var: jour_perturb <- 1 min: 1 max: 4 step: 1 category: 'Perturbateur';
}
//-------------------------------------------------------------------------------------------
/*experiment analyse_dist_perturbateur2 type: batch repeat: 50 keep_seed: true until: cpt_trp = nb_trp {
	parameter 'Batch mode' var: is_batch <- true;
	//parameter 'jour(s)' var: jour_perturb <- 0 category: 'Perturbateur';
	parameter 'distance(m)' var: dist_perturb <- 5.0 #km min: 5 #km max: 15 #km step: 1 #km category: 'Perturbateur';
	permanent {
		display perturbateur_dist background: #white refresh: every(cycle) {
			chart 'Duration depending on the distance to the disturber' type: scatter size: {0.5, 0.7} {
			//x_label <- 'distance (km)';
			//y_label<-'duration (days)
			//---------------------------- il faut nommer les axes ---------------------
				data 'jour_aller' value: round(mean(simulations collect (each.nb_cycle_aller)) / 24);
				data 'jour_retour' value: round(mean(simulations collect (each.nb_cycle_retour)) / 24);
				data 'd_perturb (km)' value: simulations collect (each.dist_perturb / 1000) accumulate_values: true;
			}

		}

	}

}*/

