with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Matrice_Creuse;

--R0: Renvoyer le PageRank dans un fichier .pr et le poids de chaque noeud du graphe dans un fichier .prw 
--à partir d’un graphe orienté dans un fichier .net avec des matrices creuses
package body p_matrice_creuse is

    procedure matricecreuse
       (K         : Integer; epsilon : Float; alpha : Float;
        prefixe   : Unbounded_String; N : Integer; N2 : Integer;
        nom_sujet : String)
    is

        type T_Result is array (1 .. 2, 1 .. N + 1) of Float;

        package Matrice_Creuse_Reel is new Matrice_Creuse
           (T_Reel => Float, Zero => 0.0, Taille_Tab => N + 1);
        use Matrice_Creuse_Reel;

        F_sujet    : Ada.Text_IO.File_Type; --Format du fichiern d'entrée
        File       : Ada.Text_IO.File_Type; --Format du fichier de sortie
        sum_vide        : Float;--somme pour lignes vides
        supp        : Float;--terme supplementaires
        distance   : Float; --distance entre les vecteurs k et k-1
        N3         : Float; --valeur des lignes dans l'
        max        : Float; --valeur du poids maximum a chaque boucle
        vide       : Float;--valeur pour une ligne vide
        val        : Float;--valeur a rajouter pour g
        ecart       : Float;--ecart entre pik+1 et pik
        Entier_d   : Integer; --entier de depart lu par le fichier
        Entier_a   : Integer; --entier d'arrivée lu par le fichier
        imax       : Integer; --indice du poids maximum a chaque boucle
        i          : Integer; --variable de boucle while
        compt      : Integer; --variable compteur
        class      : Integer; --variable de classement
        lvide      : Integer; --indice de ligne vide
        result     : T_Result; --matrice contenant les resultats du programme
        somme      : T_Tableau; --Tableau qui contient le nombre de vecteur qui partent de chaque sommet
        pi         : T_Tableau; --pi a la recurrence k
        pik1        : T_Tableau; --pi à la recurrence k+1
        Lignevide  : T_Matrice_Creuse;  --Matrice qui contient les lignes vides
        H          : T_Matrice_Creuse;  --Matrice H qui devient alpha*H
        Ligne      : T_Matrice_Creuse;  --Matrice des sommets d'arrivée pour chaque ligne
        prefixepr  : Unbounded_String; --prefixe du fichier .pr
        prefixeprw : Unbounded_String; --prefixe du fichier .prw
    begin

        --initialiser les valeurs

        --Initialiser les matrices
        Initialiser (H, N, N);
        Initialiser (Lignevide, N, 1);
        Initialiser (Ligne, N, N);

        --Initialiser les tableaux
        somme.Taille   := N;
        pi.Taille      := N;
        pik1.Taille     := N;
        somme.Elements := (others => 0.0);

        --Générer H

        --Préparer le fichier
        Open (F_sujet, In_File, nom_sujet);
        Get (F_sujet, Entier_a);

        --Remplir H avec des 1
        for i in 1 .. N2 loop
            Get (F_sujet, Entier_d);
            Get (F_sujet, Entier_a);

            --Enregistrer les valeurs nécessaire pour générer H
            Enregistrer (H, Entier_d + 1, Entier_a + 1, 1.0);
            somme.Elements (Entier_d + 1) :=
               somme.Elements (Entier_d + 1) + 1.0;
            Enregistrer
               (Ligne, Integer (somme.Elements (Entier_d + 1)), Entier_d + 1,
                Float (Entier_a + 1));
        end loop;

        compt := 0;

        --Extraire les informations de somme
        for z in 1 .. N loop
            N3 := 0.0;
            if somme.Elements (z) /= 0.0 then
                --Compléter H

                --préparer les valeurs pour remplir la ligne z
                N3  := 1.0 / somme.Elements (z);
                i   := 1;
                val := Obtenir_Val (Ligne, i, z);

                --remplir la ligne z
                while val /= 0.0 loop
                    Enregistrer (H, z, Integer (val), N3);
                    i   := i + 1;
                    val := Obtenir_Val (Ligne, i, z);
                end loop;
            else

                --Récuperer les indices de ligne vide
                compt := compt + 1;
                Enregistrer (Lignevide, compt, 1, Float (z));
            end if;
        end loop;


        --Calculer le poids des différentes pages

        --Preparer les calculs de pi
        Produit_Const (alpha, H);
        val  := (1.0 - alpha) / Float (N);
        vide := alpha * 1.0 / Float (N);
        for j in 1 .. N loop
            pi.Elements (j) := 1.0 / Float (N);
        end loop;
        i        := 0;

        --Calculer le poids de chaque page en fonction de k
        if epsilon /= 0.0 then

            --Calculer le poids en prenant en compte la distance
            distance := epsilon +1.0;
            while i < K and distance >= epsilon loop

                --calcul du terme supplémentaire
                supp := 0.0;
                for j in 1 .. N loop
                    supp := supp + pi.Elements (j) * val;
                end loop;

                --calcul des lignes vides

                --initialisation du calcul de lignes vides
                compt := 1;
                sum_vide   := 0.0;
                lvide := Integer (Obtenir_Val (Lignevide, compt, 1));

                while lvide /= 0 loop
                    sum_vide   := sum_vide + pi.Elements (lvide) * vide;
                    compt := compt + 1;
                    lvide := Integer (Obtenir_Val (Lignevide, compt, 1));
                end loop;

                --Calcul de pik+1
                pik1 := Produit_Tab_Creux (pi, H);
                for j in 1 .. N loop
                    pik1.Elements (j) := pik1.Elements (j) + supp + sum_vide;
                end loop;

                --calcul de la distance
                ecart := 0.0;
                for j in 1 .. N loop
                    ecart := ecart + (pik1.Elements (j) - pi.Elements (j))**2;
                end loop;
                distance := Sqrt (ecart);

                --préparation de la nouvelle boucle
                i        := i + 1;
                pi := pik1;
            end loop;
        else

            --Calculer le poids sans prendre en compte la distance
            while i < K loop

               --calcul du terme supplémentaire
                supp := 0.0;
                for j in 1 .. N loop
                    supp := supp + pi.Elements (j) * val;
                end loop;

                --calcul des lignes vides

                --initialisation du calcul de lignes vides
                compt := 1;
                sum_vide   := 0.0;
                lvide := Integer (Obtenir_Val (Lignevide, compt, 1));

                while lvide /= 0 loop
                    sum_vide   := sum_vide + pi.Elements (lvide) * vide;
                    compt := compt + 1;
                    lvide := Integer (Obtenir_Val (Lignevide, compt, 1));
                end loop;

                --calcul de pik+1
                pik1 := Produit_Tab_Creux (pi, H);
                for j in 1 .. N loop
                    pik1.Elements (j) := pik1.Elements (j) + supp + sum_vide;
                end loop;

                --préparation de la nouvelle boucle
                pi := pik1;
                i  := i + 1;
            end loop;
        end if;

        --Libération des matrices
        Detruire (Lignevide);
        Detruire (H);
        Detruire (Ligne);

        --Générer les fichiers résultats

        --Trier les pages et leur poids dans une matrice
        class := 0;
        while class < N loop

            --Préparer la recherche du max
            class := class + 1;
            max   := 0.0;
            imax  := 1;

            --Chercher le max
            for j in 1 .. N loop
                if pi.Elements (j) > max then
                    max  := pi.Elements (j);
                    imax := j;
                end if;
            end loop;

            --Remplir les tableaux
            result (1, class)  := Float (imax - 1);
            result (2, class)  := max;
            pi.Elements (imax) := -1.0;
            
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
            Put (File, result (2, i));
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
            Put (File, Integer (result (1, i)));
            New_Line (File);
        end loop;
        Close (File);

    end matricecreuse;

end p_matrice_creuse;
