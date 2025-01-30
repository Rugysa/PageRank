with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Matrice_Exceptions;         use Matrice_Exceptions;
with Matrice_Creuse;

procedure test_matrice_creuse is

-- Instanciation des matrices
package Matrice_Creuse_Reel is
		new Matrice_Creuse( T_Reel => Float, Zero => 0.0, Taille_Tab => 250);
	use Matrice_Creuse_Reel;
	
-- Instanciation de la fonction Afficher
procedure Afficher_T_Reel_Float (Val : in Float) is
	begin
		Put(Val,1);
end Afficher_T_Reel_Float;

procedure Afficher_Mat is
            new Matrice_Creuse_Reel.Afficher(Afficher_T_Reel_Float);

procedure Tester_Initialiser is
	Mat : T_Matrice_Creuse;
	begin
		begin
		
			-- Test d'une matrice carré
			Initialiser(Mat, 4,4);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Mat) = 4);
			pragma Assert (Nombre_Colonnes(Mat) = 4);
			-- Vérification du pointeur vers la matrice creuse
			pragma Assert (Est_Vide(Mat));
			Detruire(Mat);
			
			-- Test d'une matrice ligne
			Initialiser(Mat, 1,4);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Mat) = 1);
			pragma Assert (Nombre_Colonnes(Mat) = 4);
			-- Vérification du pointeur vers la matrice creuse
			pragma Assert (Est_Vide(Mat));
			Detruire(Mat);
			
			-- Test d'une matrice colonne
			Initialiser(Mat, 7,1);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Mat) = 7);
			pragma Assert (Nombre_Colonnes(Mat) = 1);
			-- Vérification du pointeur vers la matrice creuse
			pragma Assert (Est_Vide(Mat));
			Detruire(Mat);
			
			-- Cas problématiques
			
			-- Dimension colonne négative
			Initialiser(Mat,5,-2);
			Detruire(Mat);
			
			exception 
				when INDICE_INVALIDE_EXCEPTION => Put_Line("Dimensions nulles ou négatives -> matrice non initialisée");
			end;
			
		-- Dimension ligne négative
		Initialiser(Mat,-1,0);
		Detruire(Mat);
		
		exception 
			when INDICE_INVALIDE_EXCEPTION => Put_Line("Dimensions nulles ou négatives -> matrice non initialisée");
		
		Put_Line("Fin Tester_Initialiser");
		
end Tester_Initialiser;

procedure Tester_Enregistrer is
	Mat: T_Matrice_Creuse;
	begin
		begin
			begin
			
				-- Cas d'enregistrement de valeur positive dans une matrice carré
				Initialiser(Mat, 4,4);
				
				-- Enregistrement sur un coefficient diagonal
				Enregistrer(Mat,1,1,50.0);
				Enregistrer(Mat,2,2,14.0);
				
				-- Enregistrement sur un coefficient non diagonal
				Enregistrer(Mat,2,1,46.0);
				Enregistrer(Mat,2,4,4.0);
				Enregistrer(Mat,2,3,14.0);
				-- Vérification des valeurs
				pragma Assert (Obtenir_Val(Mat,2,2) = 14.0);
				pragma Assert (Obtenir_Val(Mat,2,1) = 46.0);
				Detruire(Mat);
				
				
				-- Cas d'enregistrement de valeurs négatives ou nulles dans une matrice ligne
				Initialiser(Mat,1,3);
				
				-- Enregistrement d'une valeur négative
				Enregistrer(Mat,1,2,-5.0);
				pragma Assert (Obtenir_Val(Mat,1,2) = -5.0);
				-- Enregistrement d'une valeur négative
				Enregistrer(Mat,1,3,-52.0);
				pragma Assert (Obtenir_Val(Mat,1,3) = -52.0);
				-- Enregistrement d'une valeur nulle
				Enregistrer(Mat,1,1,0.0);
				pragma Assert (Obtenir_Val(Mat,1,1) = 0.0);
				
				
				-- On teste ici qu'un nouvel enregistrement sur une valeur déjà enregistrée fonctionne
				Enregistrer(Mat,1,2,0.0);
				pragma Assert (Obtenir_Val(Mat,1,2) = 0.0);
				
				-- Cas problématiques
				
				-- On enregistre sur une ligne qui n'existe pas dans la matrice
				Enregistrer(Mat,5,3,-15.75);
				
				exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non enregistrée");
				end;
			-- Les coordonnées sont négatives
			Enregistrer(Mat,-4,-2,-5.0);
			
			exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non enregistrée");
			end;
		-- Une coordonnée est nulle
		Enregistrer(Mat,1,0,-5.0);
		exception 
				when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non enregistrée");

		Detruire(Mat);
		Put_Line("Fin Tester_Enregistrer");
end Tester_Enregistrer;

procedure Tester_Obtenir_Valeur is
	Mat : T_Matrice_Creuse;
	begin
		begin
			begin
				-- Cas classiques
		
				Initialiser(Mat,4,4);
				Enregistrer(Mat,2,2,50.0);
				Enregistrer(Mat,3,1,46.0);
				
				-- Coordonnées où les valeurs ont été enregistrées et sont non nulles
				pragma Assert (Obtenir_Val(Mat,2,2) = 50.0);
				pragma Assert (Obtenir_Val(Mat,3,1) = 46.0);
				-- Coordonnées où les valeurs n'ont pas été enregistrées et sont nulles
				pragma Assert (Obtenir_Val(Mat,1,3) = 0.0);
				pragma Assert (Obtenir_Val(Mat,1,3) = 0.0);
				
				-- Cas problématiques
				-- Indice de ligne trop grand
				Put(Obtenir_Val(Mat,5,2));
				
				exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non lue");
				end;
		
			-- Indice de ligne négatif
			Put(Obtenir_Val(Mat,-3,1));
			
			exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non lue");
			end;
		-- Indice de ligne nul et indice de colonne négatif
		Put(Obtenir_Val(Mat,0,-3));
		
		exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non lue");
		Detruire(Mat);
		Put_Line("Fin Tester_Obtenir_Valeur");
		
end Tester_Obtenir_Valeur;

procedure Tester_Transposer is 
	Mat : T_Matrice_Creuse;
	Trans : T_Matrice_Creuse;
	begin
	
		-- Cas de transposée d'une matrice carrée à valeur positive
		Initialiser(Mat,4,4);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		
		Transposer(Mat,Trans);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Colonnes(Trans));
		pragma Assert (Nombre_Lignes(Trans)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Trans,j,i));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Trans);
		
		-- Cas de transposée d'une matrice non carrée à valeur positive et négative
		Initialiser(Mat,3,2);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		Enregistrer(Mat,1,2,-15.75);
		
		Transposer(Mat,Trans);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Colonnes(Trans));
		pragma Assert (Nombre_Lignes(Trans)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Trans,j,i));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Trans);
		
		Put_Line("Fin Tester_Transposer");
		
end Tester_Transposer;

procedure Tester_Copier is
	Mat : T_Matrice_Creuse;
	Res : T_Matrice_Creuse;
	begin
		-- Copie d'une matrice carrée à valeur positive
		Initialiser(Mat,4,4);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		
		Copier(Mat,Res);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Lignes(Res));
		pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Res,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Res);
		
		-- Copie d'une matrice non carrée à valeur positive et négative
		Initialiser(Mat,3,2);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		Enregistrer(Mat,1,2,-15.75);
		
		Copier(Mat,Res);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Lignes(Res));
		pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Res,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Res);
		
		-- Copie d'une matrice ligne à valeur négative
		Initialiser(Mat, 1,4);
		Enregistrer(Mat,1,3,-15.75);
		Enregistrer(Mat,1,4,-465.0);
		
		Copier(Mat,Res);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Lignes(Res));
		pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Res,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Res);
		
		Put_Line("Fin Tester_Copier");
		
end Tester_Copier;

procedure Tester_Produit is
	A : T_Matrice_Creuse;
	B : T_Matrice_Creuse;
	Res : T_Matrice_Creuse;
	somme_ligneA_colonneB: float;
	begin
		begin
			-- Produit de deux matrices carrées à valeur positive et négative
			Initialiser(A,4,4);
			Initialiser(B,4,4);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,46.0);
			Enregistrer(A,1,3,-15.75);
			
			Initialiser(Res,4,4);
			Produit(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Res)=Nombre_Lignes(A));
			pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(B));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1.. Nombre_Colonnes(B) loop
					somme_ligneA_colonneB := 0.0;
					for k in 1.. Nombre_Colonnes(A) loop
						somme_ligneA_colonneB := somme_ligneA_colonneB + Obtenir_Val(A,i,k)*Obtenir_Val(B,k,j);
					end loop;
					pragma Assert(somme_ligneA_colonneB = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			Detruire(A);
			Detruire(B);
			Detruire(Res);
			
			-- Produit de d'une matrice carrée et d'une matrice non carrée à valeur positive et négative
			Initialiser(A,4,4);
			Initialiser(B,4,2);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Initialiser(Res,4,2);
			Produit(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Res)=Nombre_Lignes(A));
			pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(B));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1.. Nombre_Colonnes(B) loop
					somme_ligneA_colonneB := 0.0;
					for k in 1.. Nombre_Colonnes(A) loop
						somme_ligneA_colonneB := somme_ligneA_colonneB + Obtenir_Val(A,i,k)*Obtenir_Val(B,k,j);
					end loop;
					pragma Assert(somme_ligneA_colonneB = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			Detruire(A);
			Detruire(B);
			Detruire(Res);
			
			-- Cas d'erreurs
			-- Dimensions incompatibles
			Initialiser(A,4,5);
			Initialiser(B,4,2);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Initialiser(Res,4,2);
			Produit(A,B,Res);
			exception
				when PRODUIT_INDEFINI_EXCEPTION => Put_Line("Dimensions des matrices incompatibles");
			end;
		Detruire(A);
		Detruire(B);
		Detruire(Res);
		Put_Line("Fin Tester_Produit");
		
end Tester_Produit;

procedure Tester_Produit_Tab_Creux is
	Res : T_Tableau;
	Tab : T_Tableau;
	Creux :T_Matrice_Creuse;
	somme_produit : Float;
	begin
		-- Initialisation du tableau résultat
		Tab.Taille := 250;
		for i in 1..Tab.Taille loop
			Tab.Elements(i) := 5.0;
		end loop;
		
		-- Création de la matrice creuse
		Initialiser(Creux, 250,250);
		for i in 1..Nombre_Lignes(Creux) loop
			Enregistrer(Creux,i,i,2.0);
			for j in 1..Nombre_Colonnes(Creux)/2 loop
				Enregistrer(Creux,i,j,3.0);
			end loop;
		end loop;
            
           	Res :=Produit_Tab_Creux(Tab,Creux);
           	
           	-- Vérification des dimensions
		pragma Assert (Res.Taille=Nombre_Colonnes(Creux));
           	-- Vérification des coefficients
		for j in 1..Nombre_Colonnes(Creux) loop
			somme_produit := 0.0;
			for k in 1.. Tab.Taille loop
				somme_produit := somme_produit + Obtenir_Val(Creux,k,j)*Tab.Elements(k);
			end loop;
			pragma Assert(somme_produit = Res.Elements(j));
		end loop;

		Detruire(Creux);
           	
end Tester_Produit_Tab_Creux;

procedure Tester_Sommer is
	A : T_Matrice_Creuse;
	B : T_Matrice_Creuse;
	Res : T_Matrice_Creuse;
	begin
		begin
			-- Cas d'addition de matrices carrées à valeur positive et négative
			Initialiser(A,4,4);
			Initialiser(B,4,4);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,46.0);
			Enregistrer(A,1,3,-15.75);
			
			Initialiser(Res,4,4);
			Sommer(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(A)=Nombre_Lignes(Res));
			pragma Assert (Nombre_Colonnes(A)=Nombre_Colonnes(Res));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1..Nombre_Colonnes(A) loop
					pragma Assert (Obtenir_Val(A,i,j) + Obtenir_Val(B,i,j) = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			Detruire(A);
			Detruire(B);
			Detruire(Res);

			-- Cas d'addition de matrices non carrées à valeur positive et négative
			Initialiser(A,4,2);
			Initialiser(B,4,2);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,2,-15.75);
			
			Initialiser(Res,4,2);
			Sommer(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(A)=Nombre_Lignes(Res));
			pragma Assert (Nombre_Colonnes(A)=Nombre_Colonnes(Res));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1..Nombre_Colonnes(A) loop
					pragma Assert (Obtenir_Val(A,i,j) + Obtenir_Val(B,i,j) = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			Detruire(A);
			Detruire(B);
			Detruire(Res);
			
			-- Cas d'erreurs
			-- Taille de matrice incompatible
			Initialiser(A,4,5);
			Initialiser(B,4,2);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Initialiser(Res,4,2);
			Sommer(A,B,Res);
			exception
				when SOMME_INDEFINIE_EXCEPTION => Put_Line("Dimensions des matrices incompatibles");
			end;
		Detruire(A);
		Detruire(B);
		Detruire(Res);
			
		Put_Line("Fin Tester_Sommer");
end Tester_Sommer;

procedure Tester_Produit_Const is
	Mat :  T_Matrice_Creuse;
	Copie : T_Matrice_Creuse;
	begin
		-- Cas d'une matrice carrée
		Initialiser(Mat,5,5);
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				Enregistrer(Mat,i,j,1.0);
			end loop;
		end loop;
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Produit_Const(5.0,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (5.0*Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Copie);
		
		-- Cas d'une matrice non carrée
		Initialiser(Mat,2,6);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Produit_Const(-3.84653,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (-3.84653*Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Copie);
		
		-- Cas d'une matrice colonne
		Initialiser(Mat,7,1);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Produit_Const(-3.84653,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (-3.84653*Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Copie);
		
		Put_Line("Fin Tester_Produit_Const");
		
end Tester_Produit_Const;
			
procedure Tester_Sommer_Const is
	Mat :  T_Matrice_Creuse;
	Copie :  T_Matrice_Creuse;
	begin
		-- Cas d'une matrice carrée
		Initialiser(Mat,5,5);
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				Enregistrer(Mat,i,j,1.0);
			end loop;
		end loop;
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Sommer_Const(5.0,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop		
				pragma Assert (5.0 + Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Copie);
		
		-- Cas d'une non matrice carrée
		Initialiser(Mat,2,6);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Sommer_Const(-3.84653,Mat);
		-- Vérification des coefficient
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Copie,i,j)-3.84653 - Obtenir_Val(Mat,i,j)< 0.000000000000000000001); -- l'égalité ne fonctionne pas, pb de flotants
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Copie);
		
		-- Cas d'une matrice colonne
		Initialiser(Mat,7,1);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Sommer_Const(-3.84653,Mat);
		-- Vérification des coefficient
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Copie,i,j)-3.84653 - Obtenir_Val(Mat,i,j)< 0.000000000000000000001);
			end loop;
		end loop;
		Detruire(Mat);
		Detruire(Copie);
		
		
		
		Put_Line("Fin Tester_Sommer_Const");
		
end Tester_Sommer_Const;

procedure Tester_Afficher is
	Mat : T_Matrice_Creuse;
	begin
		-- Affichage d'une matrice carrée avec des coefficients positifs
		Initialiser(Mat, 4,4);
				
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		Put_Line("Matrice 4x4 avec comme coefficients 0.0 et 50.0 en 2,2 et 46.0 en 3,1");
		Afficher_Mat(Mat);
		Detruire(Mat);
		New_Line;
	
		-- Affichage d'une matrice non carrée avec des coefficients positifs
		Initialiser(Mat,4,5);
		Enregistrer(Mat,2,1,88.0);
		Put_Line("Matrice 4x5 avec comme coefficients 0.0 et 88.0 en 2,1");
		Afficher_Mat(Mat);
		Detruire(Mat);
		New_Line;
		
		-- Affichage d'une matrice ligne avec des coefficients positifs
		Initialiser(Mat,1,5);
		Enregistrer(Mat,1,1,88.0);
		Enregistrer(Mat,1,2,-8.0);
		Enregistrer(Mat,1,4,-37.6544);
		Put_Line("Matrice 1x5 avec comme coefficients 88.0 en 1,1, -8.0 en 1,2 et -37.6544) en 1,4");
		Afficher_Mat(Mat);
		Detruire(Mat);
		New_Line;
		
		-- Affichage d'une matrice carrée avec des coefficients négatifs
		Initialiser(Mat,3,3);
		Enregistrer(Mat,1,2,-1.0);
		Put_Line("Matrice 3x3 avec comme coefficients 0.0 et -1.0 en 1,2");
		Afficher_Mat(Mat);
		Detruire(Mat);
		New_Line;
		
		
		Put_Line("Fin Tester_Afficher");
		
end Tester_Afficher;

	begin
	
	Tester_Initialiser;
	New_Line;
	Tester_Enregistrer;
	New_Line;
	Tester_Obtenir_Valeur;
	New_Line;
	Tester_Copier;
	New_Line;
	Tester_Transposer;
	New_Line;
	Tester_Produit;
	New_Line;
	Tester_Produit_Tab_Creux;
	New_Line;
	Tester_Produit_Const;
	New_Line;
	Tester_Sommer;
	New_Line;
	Tester_Sommer_Const;
	New_Line;
	Tester_Afficher;
	New_Line;
	
	Put_Line("Fin des tests");
		
end test_matrice_creuse;
