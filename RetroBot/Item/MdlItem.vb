Module MdlItem

#Region "Ajoute"

    Sub GiItemAjoute(index As Integer, data As String, dico As Dictionary(Of Integer, CItem))

        With Comptes(index)

            Try

                ' 262c1bc   ~ 241      ~ 5        ~ 1                 ~ 64#2#4#0#1d3+1  ,
                ' Id unique ~ Id Objet ~ Quantité ~ Numéro Equipement ~ Caractéristique , etc... ; tchatItem Suivant

                If data <> "" Then

                    Dim separateData As String() = Split(data, ";")

                    For i = 0 To separateData.Count - 2

                        Dim separateItem As String() = Split(separateData(i), "~")

                        Dim newItem As New CItem

                        Try

                            With newItem

                                .IdObjet = Convert.ToInt64(separateItem(1), 16)

                                .IdUnique = Convert.ToInt64(separateItem(0), 16)

                                .Nom = VarItems(Convert.ToInt64(separateItem(1), 16)).Nom

                                .Quantiter = Convert.ToInt64(separateItem(2), 16)

                                .Caracteristique = ItemCaracteristique(separateItem(4), Convert.ToInt64(separateItem(1), 16))

                                .CaracteristiqueBrute = separateItem(4)

                                .Categorie = VarItems(.IdObjet).Catégorie

                                If separateItem(3) <> "" Then

                                    .Equipement = Convert.ToInt64(separateItem(3), 16)

                                ElseIf VarItems(Convert.ToInt64(separateItem(1), 16)).Catégorie = "24" Then

                                    .Equipement = "Quete"

                                Else

                                    .Equipement = ""

                                End If

                            End With

                        Catch ex As Exception

                            ErreurFichier(index, .Personnage.NomDuPersonnage, "GiItemAjouteNewItem", data & vbCrLf & ex.Message)

                        End Try

                        If dico.ContainsKey(newItem.IdUnique) Then

                            dico(newItem.IdUnique) = newItem

                        Else

                            dico.Add(newItem.IdUnique, newItem)

                        End If

                    Next

                    If separateData(separateData.Length - 1).Contains("G") Then

                        .Echange.Moi.Kamas = Mid(separateData(separateData.Length - 1), 2, separateData(separateData.Length - 1).Length)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiItemAjoute", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiBonusEquipementAjoute(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OS+ 5               | 2476     ; 2478    | 76#a#0#0,7d#a#0#0,77#a#0#0,7b#a#0#0,7c#a#0#0,7e#a#0#0
                ' OS+ Numéro_Panoplie | ID_Objet ; ID_Objet| Caractéristique

                data = Mid(data, 4)

                Dim separateData As String() = Split(data, "|")

                Dim newBonus As New CBonusEquipement

                With newBonus

                    .NumeroPanoplie = separateData(0)
                    .IDObjet = Split(separateData(1), ";")
                    .Caracteristique = ItemCaracteristique(separateData(2))
                    .CaracteristiqueBrute = separateData(2)

                End With

                If .BonusEquipement.ContainsKey(separateData(0)) Then

                    .BonusEquipement(separateData(0)) = newBonus

                Else

                    .BonusEquipement.Add(separateData(0), newBonus)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiBonusEquipementAjoute", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Supprime"

    Sub GiBonusEquipementSupprime(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OS- 5               
                ' OS- Numéro_Panoplie 

                data = Mid(data, 4) ' OS-5 

                If .BonusEquipement.ContainsKey(data) Then

                    .BonusEquipement.Remove(data)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiBonusEquipementSupprime", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Equipement"

    Public Sub GiPlayerChangeEquipment(index As Integer, data As String)

        With Comptes(index)

            Try

                ' Oa 1234567   | 553 , 2412~16~1 , 2411~17~1 ,          , 2509
                ' Oa ID Unique | Cac , Coiffe    , Cape      , Familier , Bouclier

                data = Mid(data, 3)

                Dim separateData As String() = Split(data, "|") '1234567 | 553,2412~16~1,2411~17~1,,2509

                Dim idUnique As String = separateData(0) '1234567

                separateData = Split(separateData(1), ",") '553,2412~16~1,2411~17~1,,2509

                If .Map.Entite.ContainsKey(idUnique) Then

                    With .Map.Entite(idUnique)

                        If separateData(1).Contains("~") Then

                            Dim separateObvijevan As String() = Split(separateData(1), "~")

                            ' .Information = ReplaceInformation(.Information, "Coiffe", "Coiffe : " & DicoItems(Convert.ToInt64(separateObvijevan(0), 16)).NameItem)

                            Dim coiffeLv As String = Convert.ToInt64(separateObvijevan(1), 16)
                            Dim coiffeForme As String = Convert.ToInt64(separateObvijevan(2), 16)

                        Else

                            If separateData(1) <> Nothing Then

                                ' .Information = ReplaceInformation(.Information, "Coiffe", "Coiffe : " & DicoItems(Convert.ToInt64(separateData(1), 16)).NameItem)

                            Else

                                ' .Information = ReplaceInformation(.Information, "Coiffe", "")

                            End If

                        End If

                        If separateData(2).Contains("~") Then

                            Dim separateObvijevan As String() = Split(separateData(2), "~")

                            '  .Information = ReplaceInformation(.Information, "Cape", "Cape : " & DicoItems(Convert.ToInt64(separateObvijevan(0), 16)).NameItem)

                            Dim capeLv As String = Convert.ToInt64(separateObvijevan(1), 16)
                            Dim capeForm As String = Convert.ToInt64(separateObvijevan(2), 16)

                        Else

                            If separateData(2) <> Nothing Then

                                '   .Information = ReplaceInformation(.Information, "Coiffe", "Coiffe : " & DicoItems(Convert.ToInt64(separateData(2), 16)).NameItem)

                            Else

                                ' .Information = ReplaceInformation(.Information, "Coiffe", "")

                            End If

                        End If

                        ' .Information = ReplaceInformation(.Information, "Cac", If(separateData(0) <> Nothing, "Cac : " & DicoItems(Convert.ToInt64(separateData(0), 16)).NameItem, ""))
                        ' .Information = ReplaceInformation(.Information, "Familier", If(separateData(0) <> Nothing, "Familier : " & DicoItems(Convert.ToInt64(separateData(0), 16)).NameItem, ""))
                        '  .Information = ReplaceInformation(.Information, "Bouclier", If(separateData(0) <> Nothing, "Bouclier : " & DicoItems(Convert.ToInt64(separateData(0), 16)).NameItem, ""))

                    End With

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPlayerChangeEquipment", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Function"

    Public Function ItemCaracteristique(caracteristique As String, Optional id As Integer = 0) As CItemCaractéristique

        ' 76 # a      # 0      # 0      # 0d0+1  , next
        ' 7b # 1      # 0      # 0      # 0d0+1  , next
        ' ID # Divers # Divers # Divers # Aléatoire (exemple CAC) , Caractéristique suivante

        '1d3+5 = Un chiffre alétoire entre 1 à 3, puis rajoute à ça +5

        Dim resultat As New CItemCaractéristique

        If caracteristique <> "" Then

            Dim separateCaracteristique As String() = Split(caracteristique, ",") ' 76#a#0#0,7b#1#0#0#0d0+1

            Try

                With resultat

                    For i = 0 To separateCaracteristique.Count - 1 ' 76#a#0#0

                        If separateCaracteristique(i) <> "" Then

                            Dim separate As String() = Split(separateCaracteristique(i), "#")

                            Dim choixCaractéristique As Integer = If(separate(0) <> "-1", Convert.ToInt64(separate(0), 16), separate(0)) ' 76
                            '15
                            '3
                            Dim valeur1 As Integer = Convert.ToInt64(separate(1), 16)
                            Dim valeur2 As Integer
                            Dim valeur3 As Integer = If(separate(3) <> "", Convert.ToInt64(separate(3), 16), 0)

                            Select Case choixCaractéristique

                                Case -1

                                Case 93

                                    .VoleAir = valeur1 & " a " & Convert.ToInt64(separate(2), 16)

                                Case 96

                                    .DommageEau = valeur1 & " a " & Convert.ToInt64(separate(2), 16)

                                Case 97

                                    .DommageTerre = valeur1 & " a " & Convert.ToInt64(separate(2), 16)

                                Case 98 'Dégât air

                                    .DommageAir = valeur1 & " a " & Convert.ToInt64(separate(2), 16)

                                Case 99

                                    .DommageFeu = valeur1 & " a " & Convert.ToInt64(separate(2), 16)

                                Case 100 ' 64 = Dommage neutre ?

                                    .DommageNeutre = valeur1 & " a " & Convert.ToInt64(separate(2), 16)

                                Case 101 ' 65 = PA -

                                    .PertePA = -valeur1

                                Case 110

                                    .Pdv = valeur1

                                Case 118 ' 76 = Force +

                                    .Force = valeur1

                                Case 157 ' 9d = Force -

                                    .Force = valeur1

                                Case 125 ' 7d = Vitalité +

                                    .Vitalité = valeur1

                                Case 153 ' 99 = Vitalité -

                                    .Vitalité = valeur1

                                Case 124 ' 7c = Sagesse +

                                    .Sagesse = valeur1

                                Case 156 ' 9c = Sagesse -

                                    .Sagesse = valeur1

                                Case 126 ' 7e = Intelligence +

                                    .Intelligence = valeur1

                                Case 155 ' 9b = Intelligence -

                                    .Intelligence = valeur1

                                Case 123 ' 7b = Chance +

                                    .Chance = valeur1

                                Case 152 ' 98 = Chance -

                                    .Chance = valeur1

                                Case 119 ' 77 = Agilité +

                                    .Agilité = valeur1

                                Case 154 ' 9a = Agilité -

                                    .Agilité = valeur1

                                Case 111 ' 6f = PA +

                                    .PA = valeur1

                                Case 128 ' 80 = PM +

                                    .PM = valeur1

                                Case 127 ' 7f = PM -

                                    .PM = valeur1

                                Case 117 ' 75 = PO +

                                    .PO = valeur1

                                Case 116 ' 74 = PO -

                                    .PO = valeur1

                                Case 182 ' b6 = Invocation +

                                    .Invocation = valeur1

                                Case 174 ' ae = Initiative +

                                    .Initiative = valeur1

                                Case 175 ' af = Initiative -

                                    .Initiative = valeur1

                                Case 176 ' b0 = Prospection +

                                    .Prospection = valeur1

                                Case 177 ' b1 = Prospection -

                                    .Prospection = valeur1

                                Case 158 ' 9e = Pods +

                                    .Pods = valeur1

                                Case 115 ' 73 = Coups Critiques +   

                                    .CC = valeur1

                                Case 112 ' 70 = Dommage +

                                    .Dommage = valeur1

                                Case 138 ' 8a = %Dommage +

                                    .PcDommage = valeur1

                                Case 225 ' e1 = Dommage Piège +

                                    .DommagePiege = valeur1

                                Case 226 ' e2 = %Dommage Piège +

                                    .PcDommagePiege = valeur1

                                Case 178 ' b2 = Soin +

                                    .Soin = valeur1

                                Case 110 ' 6e = Régénération +

                              '  résultat &= "Regeneration : " & valeur1 & vbCrLf

                                Case 193

                                   ' resultat &= "Effet : " & DicoItems(valeur3).NameItem & vbCrLf

                                Case 240 ' f0 = Résistance Terre +

                                    .ResistanceTerre = valeur1

                                Case 241 ' f1 = Résistance Eau +

                                    .ResistanceEau = valeur1

                                Case 242 ' f2 = Résistance Air +

                                    .ResistanceAir = valeur1

                                Case 243 ' f3 = Résistance Feu +

                                    .ResistanceFeu = valeur1

                                Case 244 ' f4 = Résistance Neutre +

                                    .ResistanceNeutre = valeur1

                                Case 210 ' d2 = %Résistance Terre +

                                    .PcResistanceTerre = valeur1

                                Case 215 ' d7 = %Résistance Terre -

                                    .PcResistanceTerre = valeur1

                                Case 211 ' d3 = %Résistance Eau +

                                    .PcResistanceEau = valeur1

                                Case 216 ' d8 = %Résistance Eau -

                                    .PcResistanceEau = valeur1

                                Case 212 ' d4 = %Résistance Air  +

                                    .PcResistanceAir = valeur1

                                Case 217 ' d9 = %Résistance Air  -

                                    .PcResistanceAir = valeur1

                                Case 213 ' d5 = %Résistance Feu +

                                    .PcResistanceFeu = valeur1

                                Case 218 ' da = %Résistance Feu -

                                    .PcResistanceFeu = valeur1

                                Case 214 ' d6 = %Résistance Neutre +

                                    .PcResistanceNeutre = valeur1

                                Case 219 ' db = %Résistance Neutre -

                                    .PcResistanceNeutre = valeur1

                                Case 100 '64 = Corps à Corps +

                                    .Cac = separate(4)

                                Case 101 ' 65 = PA perdus à la cible : X à Y

                               ' résultat &= "Pa perdus a la cible : " & valeur1 & vbCrLf & " a " & Convert.ToInt64(separate(2), 16) & vbCrLf

                                Case 108 ' 6c = PDV rendus : X à Y

                               ' résultat &= "Pdv rendus : " & valeur1 & vbCrLf & " a " & Convert.ToInt64(separate(2), 16) & vbCrLf

                                Case 600

                                  '  resultat &= "Potion de : Rappel" & vbCrLf

                                Case 601 ' 259

                                    '  resultat &= "Potion de cite : "

                                    Select Case Convert.ToInt64(separate(2), 16)

                                        Case 6167

                                          '  resultat &= "Brakmar" & vbCrLf

                                        Case 6159

                                            '  resultat &= "Bonta" & vbCrLf

                                        Case Else

                                            ErreurFichier(0, "Unknow", "ItemsCharacteristics", caracteristique & vbCrLf & separateCaracteristique(i))

                                    End Select

                                Case 605 ' 25d

                                ' 1# 
                                ' 3e8# = 1000
                                ' 0#
                                ' 1d1000+0

                             '   résultat &= "Xp gagnee : " & Convert.ToInt64(separate(1), 16) & " a " & Convert.ToInt64(separate(2), 16) & vbCrLf

                                Case 614

                                  '  resultat &= "+ " & valeur3 & "d'XP dans le métier " & DicoJob(Convert.ToInt64(separate(2), 16)).NameJob

                                Case 622

                                    '26e#0#0#0
                                  '  resultat &= "Potion de : Foyer" & vbCrLf


                                Case 623 '26f = Pierre d'âme 

                                    '26f#0#0#93,26f#0#0#94,26f#0#0#94,26f#0#0#94,26f#0#0#65,26f#0#0#65,26f#0#0#65,26f#0#0#65;
                                    If IsNothing(.PierreAme) Then

                                        .PierreAme = New List(Of String)

                                    End If

                                    .PierreAme.Add(valeur3)

                                Case 699

                                 '   resultat &= "Lier son métier : " & DicoJob(valeur1).NameJob & vbCrLf

                                Case 701

                                    .Puissance = valeur1

                                Case 795

                                   ' resultat &= "Arme de chasse" & vbCrLf

                                Case 800 '320 = Point de vie +

                                    '320 #5      #48     #7
                                    .FamilierPdv = valeur3
                                 '   resultat &= "Point de Vie : " & valeur3 & vbCrLf

                                Case 806 ' 326 = 'Repas et Corpulence 

                                    ' 326#1#0#1ab

                                    valeur2 = Convert.ToInt64(separate(2), 16)

                                    If valeur3 >= 7 Then

                                        valeur3 = If(valeur3 > 100, 100, valeur3)

                                        .FamilierRepas = -valeur3
                                        .FamilierCorpulence = "Maigrichon"

                                    ElseIf valeur2 >= 7 Then

                                        .FamilierRepas = valeur3
                                        .FamilierCorpulence = "Obese"

                                    Else

                                        .FamilierRepas = "0"
                                        .FamilierCorpulence = "Normal"

                                    End If

                                Case 807 ' 327 = Dernier Repas (objet utilisé)

                                    '327#0#0#734

                                    Select Case valeur3

                                        Case 2114

                                            .FamilierDernierRepas = "Aliment inconnu"

                                        Case "0"

                                            .FamilierDernierRepas = "Aucun"

                                        Case Else

                                            .FamilierDernierRepas = VarItems(valeur3).Nom

                                    End Select

                                Case 808 '"328" 'Date / Heure  

                                    ' 328 # 28a   # cc          # 398   = A mangé le : 04/03/650 9:20
                                    ' 328 # Année # Mois + Jour # Heure

                                    valeur2 = Convert.ToInt64(separate(2), 16)

                                    Dim Année As Integer = valeur1 + 1370

                                    Dim Mois As Integer = If(valeur2 < 100, 1, Mid(valeur2, 1, valeur2.ToString.Length - 2) + 1)
                                    Dim Jour As Integer = If(valeur2 < 100, valeur2, Mid(valeur2, valeur2.ToString.Length - 1, 2))

                                    Dim Heure As String = valeur3.ToString.Insert(valeur3.ToString.Length - 2, ":")
                                    If Heure.Length = 3 Then Heure = "00" & Heure

                                    .FamilierDateRepas = Jour & "/" & Mois & "/" & Année & " " & Heure

                                    .FamilierDateProchainRepas = .FamilierDateRepas.AddHours(VarFamilier(id).IntervalRepasMin)

                                Case 811 'Malédiction/booste bonbon etc....

                                    Select Case valeur1

                                        Case 15 ' Malédiction du ballotin

                                            .TourRestant = valeur3

                                    End Select


                                Case 812

                                    .ResistanceItem = valeur1 & "/" & valeur3

                                    Select Case VarItems(id).Catégorie

                                        Case 5, 19, 8, 22, 7, 3, 4, 6, 20, 21, 83

                                            .Etheree = True

                                        Case Else

                                            .Etheree = False

                                    End Select

                                Case 830

                                    '  resultat &= "Potion de : "

                                    Select Case valeur3

                                        Case 1

                                          '  resultat &= "Foyer de guilde" & vbCrLf

                                        Case 2

                                            ' resultat &= "Enclos de guilde" & vbCrLf

                                    End Select

                                Case 850 ' ?

                                Case 851 ' ? Vole or 1d5+5 d'or ?

                                Case 940 '"3ac" 'Capacité accrue Familier

                                    '3ac#0#0#a
                                    ' a = 10, donc le familier peut avoir +10 en caract, etc... selon le familier.
                                    .FamilierCapaciteAccrue = True

                                Case 948

                                   ' resultat &= "Objet pour enclos" & vbCrLf

                                Case 970

                                   ' resultat &= "Apparence : " & DicoItems(valeur3).NameItem & vbCrLf

                                Case 971

                            '3cb#0#0#0
                            'Aucune idée, mais Objivevan

                                Case 972

                                   ' resultat &= "Niveau : " & valeur3 & vbCrLf

                                Case 973

                            '3cd#0#0#10
                            'Indique la forme à afficher (sprite)
                            '10 = le forme (sprite) à afficher

                                Case 974

                            '3ce#0#0#17c
                            'Info Objivevan inconnu

                                Case 985

                                 '   resultat &= "Modifie par : " & separate(4) & vbCrLf

                                Case 988

                                   ' resultat &= "Fabrique par : " & separate(4) & vbCrLf

                                Case 994 ' 3e2

                                    .DragodindeDate = TimeOfDay

                                Case 995 '3e3 = ID de la dragodinde pour avoir les caractéristiques (quand elle se trouve dans l'inventaire)
                                    '3e3#c0a#1710bbb0c60#0

                                    .DragodindeIdUnique = "Rd" & valeur1 & vbCrLf & "|" & separate(2)

                                Case 996 ' 3e4 = Nom du joueur qui posséde la dragodinde.
                                    '3e4#0#0#0#Linaculer

                                    .DragodindePossesseur = separate(4)

                                Case 997 '3e5 = Nom de la dragodinde
                                    '3e5#15#0#0#Linaculeur

                                    .DragodindeNom = separate(4)

                                Case 998 '"3e6" ' Jour/ heure / minute restant.
                                    '3e6#13#17#3b

                                    .DragodindeDateEnParchemin = DateAdd(DateInterval.Day, valeur1, Date.Today).ToString & " " & Convert.ToInt64(separate(2), 16) & ":" & valeur3

                                Case 805 '"325" 'Divers

                                    '  resultat &= "Divers : Certificat Dopeul" & vbCrLf


                                Case Else

                                    ErreurFichier(0, "Unknow", "ItemsCharacteristics", caracteristique & vbCrLf & separateCaracteristique(i))

                            End Select

                        End If

                    Next

                End With

            Catch ex As Exception

            End Try

        End If

        Return resultat

    End Function

    Private Function ReturnCaracteristique(caracteristique As String) As CItemCaractéristique

        Dim NewCaracteristique As New CItemCaractéristique

        Try

            Dim separateCaracteristique As String() = Split(caracteristique, " | ")

            For i = 0 To separateCaracteristique.Count - 1

                Dim separate As String() = Split(separateCaracteristique(i), " : ")

                With NewCaracteristique

                    Select Case separate(0).ToLower

                        Case "etheree"

                            .Etheree = CBool(separate(1))

                        Case "force"

                            .Force = separate(1)

                        Case "agiliter", "agilite"

                            .Agilité = separate(1)

                        Case "sagesse"

                            .Sagesse = separate(1)

                        Case "chance"

                            .Chance = separate(1)

                        Case "intelligence"

                            .Intelligence = separate(1)

                        Case "vitaliter", "vitalite"

                            .Vitalité = separate(1)

                        Case "dommage"

                            .Dommage = separate(1)

                        Case "dommage pc"

                            .PcDommage = separate(1)

                        Case "piege"

                            .DommagePiege = separate(1)

                        Case "piege pc"

                            .PcDommagePiege = separate(1)

                        Case "resistance neutre"

                            .ResistanceNeutre = separate(1)

                        Case "resistance feu"

                            .ResistanceFeu = separate(1)

                        Case "resistance eau"

                            .ResistanceEau = separate(1)

                        Case "resistance terre"

                            .ResistanceTerre = separate(1)

                        Case "resistance air"

                            .ResistanceAir = separate(1)

                        Case "resistance neutre pc"

                            .PcResistanceNeutre = separate(1)

                        Case "resistance feu pc"

                            .PcResistanceFeu = separate(1)

                        Case "resistance eau pc"

                            .PcResistanceEau = separate(1)

                        Case "resistance terre pc"

                            .PcResistanceTerre = separate(1)

                        Case "resistance air pc"

                            .PcResistanceAir = separate(1)

                        Case "pa"

                            .PA = separate(1)

                        Case "pm"

                            .PM = separate(1)

                        Case "po"

                            .PO = separate(1)

                        Case "prospection"

                            .Prospection = separate(1)

                        Case "pods"

                            .Pods = separate(1)

                        Case "soin"

                            .Soin = separate(1)

                        Case "initiative"

                            .Initiative = separate(1)

                        Case "invocation"

                            .Invocation = separate(1)

                    End Select

                End With

            Next

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "ReturnCaracteristique", ex.Message)

        End Try

        Return NewCaracteristique

    End Function

    Public Function ComparateurCaractéristiqueObjets(Item As CItemCaractéristique, CaracteristiqueVoulu As String) As Boolean

        Try

            Dim voulu As CItemCaractéristique = ReturnCaracteristique(CaracteristiqueVoulu)

            If voulu.DommageEau <> "0" Then

                If CInt(Item.DommageEau) < CInt(Split(voulu.DommageEau, " a ")(0)) OrElse CInt(Item.DommageEau) > CInt(Split(voulu.DommageEau, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.DommageFeu <> "0" Then

                If CInt(Item.DommageFeu) < CInt(Split(voulu.DommageFeu, " a ")(0)) OrElse CInt(Item.DommageFeu) > CInt(Split(voulu.DommageFeu, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.DommageTerre <> "0" Then

                If CInt(Item.DommageTerre) < CInt(Split(voulu.DommageTerre, " a ")(0)) OrElse CInt(Item.DommageTerre) > CInt(Split(voulu.DommageTerre, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.DommageAir <> "0" Then

                If CInt(Item.DommageAir) < CInt(Split(voulu.DommageAir, " a ")(0)) OrElse CInt(Item.DommageAir) > CInt(Split(voulu.DommageAir, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.DommageNeutre <> "0" Then

                If CInt(Item.DommageNeutre) < CInt(Split(voulu.DommageNeutre, " a ")(0)) OrElse CInt(Item.DommageNeutre) > CInt(Split(voulu.DommageNeutre, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Force <> "0" Then

                If CInt(Item.Force) < CInt(Split(voulu.Force, " a ")(0)) OrElse CInt(Item.Force) > CInt(Split(voulu.Force, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Vitalité <> "0" Then

                If CInt(Item.Vitalité) < CInt(Split(voulu.Vitalité, " a ")(0)) OrElse CInt(Item.Vitalité) > CInt(Split(voulu.Vitalité, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Agilité <> "0" Then

                If CInt(Item.Agilité) < CInt(Split(voulu.Agilité, " a ")(0)) OrElse CInt(Item.Agilité) > CInt(Split(voulu.Agilité, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Sagesse <> "0" Then

                If CInt(Item.Sagesse) < CInt(Split(voulu.Sagesse, " a ")(0)) OrElse CInt(Item.Sagesse) > CInt(Split(voulu.Sagesse, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Intelligence <> "0" Then

                If CInt(Item.Intelligence) < CInt(Split(voulu.Intelligence, " a ")(0)) OrElse CInt(Item.Intelligence) > CInt(Split(voulu.Intelligence, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Chance <> "0" Then

                If CInt(Item.Chance) < CInt(Split(voulu.Chance, " a ")(0)) OrElse CInt(Item.Chance) > CInt(Split(voulu.Chance, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PA <> "0" Then

                If CInt(Item.PA) < CInt(Split(voulu.PA, " a ")(0)) OrElse CInt(Item.PA) > CInt(Split(voulu.PA, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PM <> "0" Then

                If CInt(Item.PM) < CInt(Split(voulu.PM, " a ")(0)) OrElse CInt(Item.PM) > CInt(Split(voulu.PM, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PO <> "0" Then

                If CInt(Item.PO) < CInt(Split(voulu.PO, " a ")(0)) OrElse CInt(Item.PO) > CInt(Split(voulu.PO, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Invocation <> "0" Then

                If CInt(Item.Invocation) < CInt(Split(voulu.Invocation, " a ")(0)) OrElse CInt(Item.Invocation) > CInt(Split(voulu.Invocation, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Initiative <> "0" Then

                If CInt(Item.Initiative) < CInt(Split(voulu.Initiative, " a ")(0)) OrElse CInt(Item.Initiative) > CInt(Split(voulu.Initiative, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Prospection <> "0" Then

                If CInt(Item.Prospection) < CInt(Split(voulu.Prospection, " a ")(0)) OrElse CInt(Item.Prospection) > CInt(Split(voulu.Prospection, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Pods <> "0" Then

                If CInt(Item.Pods) < CInt(Split(voulu.Pods, " a ")(0)) OrElse CInt(Item.Pods) > CInt(Split(voulu.Pods, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.CC <> "0" Then

                If CInt(Item.CC) < CInt(Split(voulu.CC, " a ")(0)) OrElse CInt(Item.CC) > CInt(Split(voulu.CC, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Dommage <> "0" Then

                If CInt(Item.Dommage) < CInt(Split(voulu.Dommage, " a ")(0)) OrElse CInt(Item.Dommage) > CInt(Split(voulu.Dommage, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcDommage <> "0" Then

                If CInt(Item.PcDommage) < CInt(Split(voulu.PcDommage, " a ")(0)) OrElse CInt(Item.PcDommage) > CInt(Split(voulu.PcDommage, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.DommagePiege <> "0" Then

                If CInt(Item.DommagePiege) < CInt(Split(voulu.DommagePiege, " a ")(0)) OrElse CInt(Item.DommagePiege) > CInt(Split(voulu.DommagePiege, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcDommagePiege <> "0" Then

                If CInt(Item.PcDommagePiege) < CInt(Split(voulu.PcDommagePiege, " a ")(0)) OrElse CInt(Item.PcDommagePiege) > CInt(Split(voulu.PcDommagePiege, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Soin <> "0" Then

                If CInt(Item.Soin) < CInt(Split(voulu.Soin, " a ")(0)) OrElse CInt(Item.Soin) > CInt(Split(voulu.Soin, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.ResistanceTerre <> "0" Then

                If CInt(Item.ResistanceTerre) < CInt(Split(voulu.ResistanceTerre, " a ")(0)) OrElse CInt(Item.ResistanceTerre) > CInt(Split(voulu.ResistanceTerre, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.ResistanceEau <> "0" Then

                If CInt(Item.ResistanceEau) < CInt(Split(voulu.ResistanceEau, " a ")(0)) OrElse CInt(Item.ResistanceEau) > CInt(Split(voulu.ResistanceEau, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.ResistanceFeu <> "0" Then

                If CInt(Item.ResistanceFeu) < CInt(Split(voulu.ResistanceFeu, " a ")(0)) OrElse CInt(Item.ResistanceFeu) > CInt(Split(voulu.ResistanceFeu, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.ResistanceAir <> "0" Then

                If CInt(Item.ResistanceAir) < CInt(Split(voulu.ResistanceAir, " a ")(0)) OrElse CInt(Item.ResistanceAir) > CInt(Split(voulu.ResistanceAir, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.ResistanceNeutre <> "0" Then

                If CInt(Item.ResistanceNeutre) < CInt(Split(voulu.ResistanceNeutre, " a ")(0)) OrElse CInt(Item.ResistanceNeutre) > CInt(Split(voulu.ResistanceNeutre, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcResistanceTerre <> "0" Then

                If CInt(Item.PcResistanceTerre) < CInt(Split(voulu.PcResistanceTerre, " a ")(0)) OrElse CInt(Item.PcResistanceTerre) > CInt(Split(voulu.PcResistanceTerre, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcResistanceEau <> "0" Then

                If CInt(Item.PcResistanceEau) < CInt(Split(voulu.PcResistanceEau, " a ")(0)) OrElse CInt(Item.PcResistanceEau) > CInt(Split(voulu.PcResistanceEau, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcResistanceFeu <> "0" Then

                If CInt(Item.PcResistanceFeu) < CInt(Split(voulu.PcResistanceFeu, " a ")(0)) OrElse CInt(Item.PcResistanceFeu) > CInt(Split(voulu.PcResistanceFeu, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcResistanceAir <> "0" Then

                If CInt(Item.PcResistanceAir) < CInt(Split(voulu.PcResistanceAir, " a ")(0)) OrElse CInt(Item.PcResistanceAir) > CInt(Split(voulu.PcResistanceAir, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.PcResistanceNeutre <> "0" Then

                If CInt(Item.PcResistanceNeutre) < CInt(Split(voulu.PcResistanceNeutre, " a ")(0)) OrElse CInt(Item.PcResistanceNeutre) > CInt(Split(voulu.PcResistanceNeutre, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Puissance <> "0" Then

                If CInt(Item.Puissance) < CInt(Split(voulu.Puissance, " a ")(0)) OrElse CInt(Item.Puissance) > CInt(Split(voulu.Puissance, " a ")(1)) Then

                    Return False

                End If

            End If

            If voulu.Etheree <> "0" Then

                If Item.Etheree <> voulu.Etheree Then

                    Return False

                End If

            End If

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "ComparateurCaractéristiqueObjets", ex.Message)

        End Try

        Return True

    End Function

#End Region

End Module

#Region "Class"

Public Class CItem

    Public IdObjet As Integer = 0
    Public IdUnique As Integer = 0
    Public Nom As String = ""
    Public Quantiter As Integer = 0
    Public Caracteristique As New CItemCaractéristique
    Public CaracteristiqueBrute As String = ""
    Public Equipement As String = ""
    Public Categorie As Integer = 0

End Class

Public Class CItemCaractéristique

    Public VoleAir As String = "0"
    Public DommageEau As String = "0"
    Public DommageFeu As String = "0"
    Public DommageTerre As String = "0"
    Public DommageAir As String = "0"
    Public DommageNeutre As String = "0"
    Public PertePA As String = "0"
    Public Pdv As String = "0"
    Public Force As String = "0"
    Public Vitalité As String = "0"
    Public Sagesse As String = "0"
    Public Intelligence As String = "0"
    Public Chance As String = "0"
    Public Agilité As String = "0"
    Public PA As String = "0"
    Public PM As String = "0"
    Public PO As String = "0"
    Public Invocation As String = "0"
    Public Initiative As String = "0"
    Public Prospection As String = "0"
    Public Pods As String = "0"
    Public CC As String = "0"
    Public Dommage As String = "0"
    Public PcDommage As String = "0"
    Public DommagePiege As String = "0"
    Public PcDommagePiege As String = "0"
    Public Soin As String = "0"
    Public ResistanceTerre As String = "0"
    Public ResistanceEau As String = "0"
    Public ResistanceFeu As String = "0"
    Public ResistanceAir As String = "0"
    Public ResistanceNeutre As String = "0"
    Public PcResistanceTerre As String = "0"
    Public PcResistanceEau As String = "0"
    Public PcResistanceFeu As String = "0"
    Public PcResistanceAir As String = "0"
    Public PcResistanceNeutre As String = "0"
    Public Cac As String = "0"
    Public PierreAme As List(Of String)
    Public Puissance As String = "0"
    Public FamilierPdv As String = "0"
    Public FamilierRepas As Integer = 0
    Public FamilierCorpulence As String = "Normal"
    Public FamilierDernierRepas As String = "Aliment Inconnu"
    Public FamilierDateRepas As Date = TimeOfDay
    Public FamilierDateProchainRepas As Date = TimeOfDay
    Public FamilierCapaciteAccrue As Boolean = False
    Public ResistanceItem As String = "0"
    Public DragodindeDate As String = ""
    Public DragodindeIdUnique As String = ""
    Public DragodindePossesseur As String = ""
    Public DragodindeNom As String = ""
    Public DragodindeDateEnParchemin As Date = TimeOfDay
    Public Etheree As Boolean = False
    Public TourRestant As String = 0

End Class


#End Region