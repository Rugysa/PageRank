with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Matrice_Pleine;


--R0: Renvoyer le PageRank dans un fichier .pr et le poids de chaque noeud du graphe dans un fichier .prw
--à partir d’un graphe orienté dans un fichier .net avec des matrices pleines

package body p_matrice_pleine is

    procedure matricepleine
       (K         : Integer; epsilon : Float; alpha : Float;
        prefixe   : Unbounded_String; N : Integer; N2 : Integer;
        nom_sujet : String)
    is

        package Matrice_Reel is new Matrice_Pleine
           (T_Reel => Float, Num_Colonne => N, Num_Ligne => N, Zero => 0.0);
        use Matrice_Reel;

        F_sujet    : Ada.Text_IO.File_Type; --Format du fichiern d'entrée
        File       : Ada.Text_IO.File_Type; --Format du fichier de sortie
        distance   : Float; --distance entre les vecteurs k et k-1
        N3         : Float; --valeur des lignes dans l'
        max        : Float; --valeur du poids maximum a chaque boucle
        supp        : Float; -- termes supplémentaires
        val_pi     : Float; --valeur initiale de chaque element de pi
        Entier_d   : Integer; --entier de depart lu par le fichier
        Entier_a   : Integer; --entier d'arrivée lu par le fichier
        imax       : Integer; --indice du poids maximum a chaque boucle
        i          : Integer; --variable de boucle while
        compt      : Integer; --variable compteur
        class      : Integer; --variable de classement
        mat : T_Matrice_Pleine; --matrice contenant les resultats du programme
        pi         : T_Matrice_Pleine; --pi a la recurrence k
        pik        : T_Matrice_Pleine; --pi à la recurrence k-1
        e          : T_Matrice_Pleine; --matrice de 1 de taille N*1
        G          : T_Matrice_Pleine; --matrice de google
        S          : T_Matrice_Pleine; --matrice S
        S1         : T_Matrice_Pleine; --copie de la matrice S
        list       :
           T_Matrice_Pleine; --liste des vecteurs partant d'un sommet pour un sommet différent a chaque boucle
        ecartm     : T_Matrice_Pleine; --matrice 1*1 de l'ecart entre pik et pi
        prefixepr  : Unbounded_String; --prefixe du fichier .pr
        prefixeprw : Unbounded_String; --prefixe du fichier .prw
    begin

        --Initialiser le programme

        --Générer e
        supp := (1.0 - alpha) / Float (N);
        Initialiser (e, N, N, supp);

        --Générer S

        --Préparer le fichier
        Open (F_sujet, In_File, nom_sujet);
        Get (F_sujet, Entier_a);
        Initialiser (S, N, N, 0.0);

        --Remplir H avec des 1
        for i in 1 .. N2 loop
            Get (F_sujet, Entier_d);
            Get (F_sujet, Entier_a);
            Enregistrer (S, Entier_d + 1, Entier_a + 1, 1.0);
        end loop;

        for z in 0 .. N - 1 loop

            --préparation des valeurs de la ligne z de la matrice H
            compt := 0;
            Initialiser (list, N, 1, 0.0);

            --Ajout des valeurs de la ligne z dans la matrice S
            N3 := 0.0;
            if Ligne_Vide (z + 1, S) then

                --remplacer la valeur de chaque coordonnées de la ligne par 1/N
                for j in 1 .. N loop
                    Enregistrer (S, z + 1, j, 1.0 / Float (N));
                end loop;

            else

                --lister les sommets d'arrivées qui partent de z
                for i in 1 .. N loop
                    if Obtenir_Val (S, z + 1, i) = 1.0 then
                        compt := compt + 1;
                        Enregistrer (list, compt, 1, Float (i));
                    end if;
                end loop;

                --remplir la ligne z de S
                N3 := 1.0 / Float (compt);
                i  := 1;
                while Obtenir_Val (list, i, 1) /= 0.0 loop
                    Enregistrer
                       (S, z + 1, Integer (Obtenir_Val (list, i, 1)), N3);
                    i := i + 1;
                end loop;

            end if;
        end loop;

        --Calculer G
        S1 := S;
        Produit_Const (alpha, S1);
        Copier (Sommer_f (S1, e), G);

        --Calculer le poids des différentes pages
        Initialiser (mat, N, 2, 0.0);

        --Preparer les calculs de pi
        i        := 0;
        distance := epsilon + 1.0;
        val_pi := 1.0 / Float (N);
        Initialiser (pi, 1, N, val_pi);

        --Calculer le poids de chaque page en fonction de k
        if epsilon /= 0.0 then

            --Calculer le poids en prenant en compte la distance
            while i < K and distance >= epsilon loop
                Produit (pi, G, pi);

                --calcul de la distance
                Copier (pi, pik);
                Produit_Const (-1.0, pik);
                Sommer (pi, pik, ecartm);
                distance :=
                   Sqrt
                      (Obtenir_Val
                          (Produit_f (ecartm, Transposer (ecartm)), 1, 1));

                i        := i + 1;
            end loop;
        else

            --Calculer le poids sans prendre en compte la distance
            while i < K loop
                Produit (pi, G, pik);
                pi := pik;
                i  := i + 1;
            end loop;
            
        end if;

        --Générer les fichiers résultats

        --Trier les pages et leur poids dans une matrice
        class := 0;
        while class < N loop

            --Préparer la recherche du max
            class := class + 1;
            max   := 0.0;
            imax  := 1;

            --Chercher le max
            for i in 1 .. N loop
                if Obtenir_Val (pi, 1, i) > max then
                    max  := Obtenir_Val (pi, 1, i);
                    imax := i;
                end if;
            end loop;

            --Remplir les tableaux
            Enregistrer (mat, class, 1, Float (imax - 1));
            Enregistrer (mat, class, 2, max);
            Enregistrer (pi, 1, imax, 0.0);

        end loop;

        --Créer le fichier sujet.prw

        --Nommer prefixe.prw
        prefixeprw := prefixe;
        Append (prefixeprw, ".prw");

        --initialiser sujet.prw
        Create (File, Out_File, To_String (prefixeprw));
        Put (File, N);
        Put (File, " ");
        Put (File, alpha);
        Put (File, " ");
        Put (File, K);
        New_Line (File);

        for i in 1 .. N loop
            Put (File, Obtenir_Val (mat, i, 2));
            New_Line (File);
        end loop;
        Close (File);

        --Créer le fichier sujet.pr

        --Nommer prefixe.pr
        prefixepr := prefixe;
        Append (prefixepr, ".pr");

        --initialiser sujet.pr
        Create (File, Out_File, To_String (prefixepr));
        Put (File, N);
        New_Line (File);
        
        for i in 1 .. N loop
            Put (File, Integer (Obtenir_Val (mat, i, 1)));
            New_Line (File);
        end loop;
        Close (File);

    end matricepleine;

end p_matrice_pleine;
