Module Chargement

    Public Sub ChargeServeur()

        Try

            VarServeur.Clear()

            'J'ouvre et je lis le fichier.
            Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Data/Serveur.txt")

            Do Until swLecture.EndOfStream

                Dim Ligne As String = swLecture.ReadLine

                If Ligne <> "" Then

                    Dim Separation() As String = Split(Ligne.Replace(" ", ""), "|")

                    If Not VarServeur.ContainsKey(Separation(0)) Then

                        Dim varServer As New ClassServeur

                        With varServer

                            .Nom = Separation(0)
                            .IP = Separation(1)
                            .Port = Separation(2)
                            .ID = Separation(3)

                        End With

                        VarServeur.Add(Separation(0), varServer)

                    End If

                End If

            Loop

            'Puis je ferme le fichier.
            swLecture.Close()

        Catch ex As Exception

            ErreurFichier(0, "Load", "ChargeServeur", ex.Message)

        End Try

    End Sub

    Public Sub ChargeItems()

        Try

            'J'ouvre et je lis le fichier.
            Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Items.txt")

            Do Until swReading.EndOfStream

                Dim lige As String = swReading.ReadLine

                If lige <> "" Then

                    Dim separate As String() = Split(lige, "|")

                    If Not VarItems.ContainsKey(separate(0)) Then

                        Dim varItems As New sItems

                        With varItems

                            .ID = separate(0)
                            .Nom = separate(1)
                            .Catégorie = separate(2)
                            .Pods = separate(3)

                        End With

                        Variable.VarItems.Add(separate(0), varItems)

                    End If

                End If

            Loop

            'Puis je ferme le fichier.
            swReading.Close()

        Catch ex As Exception

            ErreurFichier(0, "Load", "ChargeItems", ex.Message)

        End Try

    End Sub

    Public Sub ChargeSort()

        Try

            'Si le fichier sort éxiste.
            If IO.File.Exists("Data\Sort.txt") Then

                'Je l'ouvre.
                Dim swReading As New IO.StreamReader("Data\Sort.txt")

                'Puis je regarde chaque ligne jusqu'à la fin du fichier.
                Do Until swReading.EndOfStream

                    'Je met la ligne dans une variable string.
                    Dim line As String = swReading.ReadLine

                    'Si elle n'est pas vide alors.
                    If line <> "" Then

                        'Je sépare les informations.
                        Dim separate() As String = Split(line, "|")
                        '161|1|Flèche Magique|1-7|4|0-0|2|0-0|O|O|N|N|N|0-0|N|1|Classe
                        'ID Sort | Level | Nom | PO min max | PA | Nombre de lancer par tour | Nombre de lancer par tour par joueur | Nombre de tour entre 2 lancé | PO Modifiable | Ligne de vue
                        '| Lancer en ligne | Cellule Libre | Echec fini tour | Zone du sort | Champ d'action 'X/L/Z/N" | Next level

                        Dim _varSort As New CSort

                        With _varSort

                            .ID = separate(0)
                            .Niveau = separate(1)
                            .Nom = separate(2)
                            .POMinimum = Split(separate(3), "-")(0)
                            .POMaximum = Split(separate(3), "-")(1)
                            .PA = separate(4)
                            .NombreLancerParTour = If(separate(5).Contains("-"), Split(separate(5), "-")(1), separate(5))
                            .NombreLancerParTourParJoueur = separate(6)
                            .NombreToursEntreDeuxLancers = If(separate(7).Contains("-"), Split(separate(7), "-")(1), separate(7))
                            .POModifiable = CBool(separate(8))
                            .LigneDeVue = CBool(separate(9))
                            .LancerEnLigne = CBool(separate(10))
                            .CelluleLibre = CBool(separate(11))
                            .ECFiniTour = CBool(separate(12))
                            .ZoneMinimum = Split(separate(13), "-")(0)
                            .ZoneMaximum = Split(separate(13), "-")(1)
                            .ZoneEffet = separate(14)
                            .NiveauRequisUp = separate(15)
                            .SortClasse = separate(16)
                            .Definition = separate(17)

                        End With


                        If VarSort.ContainsKey(separate(0)) Then

                            VarSort(separate(0)).Add(separate(1), _varSort)

                        Else

                            VarSort.Add(separate(0), New Dictionary(Of Integer, CSort) From
                                         {{
                                         separate(1), _varSort
                                         }})



                        End If

                    End If

                Loop

                'Je ferme mon fichier.
                swReading.Close()

            End If

        Catch ex As Exception

            ErreurFichier(0, "chargesort", "ChargeSort", ex.Message)

        End Try

    End Sub

    Public Sub ChargeQuête()

        Dim swReading As New IO.StreamReader("Data/Quête.txt")

        Do Until swReading.EndOfStream

            Dim Line As String = swReading.ReadLine

            If Line <> "" Then

                Dim separate() As String = Split(Line, "|")

                VarQuête.Add(separate(0), separate(1))

            End If

        Loop

        swReading.Close()

    End Sub

    Public Sub ChargeMap()

        Try

            VarMap.Clear()

            Dim swReading As New IO.StreamReader("Data/Maps.txt")

            Do Until swReading.EndOfStream

                Dim line As String = swReading.ReadLine

                If line <> "" Then

                    Dim separate() As String = Split(line, ":")

                    VarMap.Add(separate(0), separate(1))

                End If

            Loop

            swReading.Close()

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "LoadMaps", ex.Message)

        End Try

    End Sub

    Public Sub ChargeDivers()

        Try

            VarInteraction.Clear()

            Dim swReading As New IO.StreamReader("Data/Divers.txt")

            Do Until swReading.EndOfStream

                Dim line As String = swReading.ReadLine

                If line <> "" Then

                    Dim separate() As String = Split(line, "|")

                    Dim _varInteraction As New sInteraction

                    With _varInteraction

                        .IdSprite = separate(0)
                        .Name = separate(1)
                        .DicoInteraction = New Dictionary(Of String, Integer)

                        Dim separateNameInteraction As String() = Split(separate(2), ":")
                        Dim separateIDInteraction As String() = Split(separate(3), ":")

                        For i = 0 To separateNameInteraction.Count - 1
                            .DicoInteraction.Add(separateNameInteraction(i).ToLower, separateIDInteraction(i).ToLower)
                        Next

                    End With

                    VarInteraction.Add(separate(0), _varInteraction)

                End If
            Loop

            swReading.Close()

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "LoadMetier", ex.Message)

        End Try

    End Sub

    Public Sub ChargeRecolte()

        Try

            VarInteraction.Clear()

            Dim swReading As New IO.StreamReader("Data/Recolte.txt")

            Do Until swReading.EndOfStream

                Dim line As String = swReading.ReadLine

                If line <> "" Then

                    Dim separate() As String = Split(line, "|")

                    Dim _varInteraction As New sInteraction

                    With _varInteraction

                        .IdSprite = separate(0)
                        .Name = separate(1)
                        .DicoInteraction = New Dictionary(Of String, Integer)

                        Dim separateNameInteraction As String() = Split(separate(2), ":")
                        Dim separateIDInteraction As String() = Split(separate(3), ":")

                        For i = 0 To separateNameInteraction.Count - 1
                            .DicoInteraction.Add(separateNameInteraction(i).ToLower, separateIDInteraction(i).ToLower)
                        Next

                    End With

                    VarRecolte.Add(separate(0), _varInteraction)

                End If
            Loop

            swReading.Close()

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "ChargeRecolte", ex.Message)

        End Try

    End Sub

    Public Sub ChargeMobs()

        'J'ouvre et je lis le fichier.
        Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Mobs.txt")

        Do Until swReading.EndOfStream

            Dim line As String = swReading.ReadLine

            If line <> "" Then

                Dim separate As String() = Split(line, "|") '31|Larve Bleue|Level:2:Résistance:1:5:5:-9:-9:5:3|Next level

                Dim idMobs As Integer = separate(0)
                Dim nameMobs As String = separate(1)

                For a = 2 To separate.Count - 1 ' Level:2:Résistance:1:5:5:-9:-9:5:3

                    Dim separateData As String() = Split(separate(a), ":")

                    Dim varMobs As New sMobs

                    With varMobs

                        .ID = separate(0)
                        .Nom = separate(1)
                        .Niveau = separateData(1)

                        .RésistanceNeutre = separateData(3)
                        .RésistanceTerre = separateData(4)
                        .RésistanceFeu = separateData(5)
                        .RésistanceEau = separateData(6)
                        .RésistanceAir = separateData(7)

                        .EsquivePA = separateData(8)
                        .EsquivePM = separateData(9)

                    End With

                    If Variable.VarMobs.ContainsKey(idMobs) Then

                        Variable.VarMobs(idMobs).Add(a - 2, varMobs)

                    Else

                        Variable.VarMobs.Add(idMobs, New Dictionary(Of Integer, sMobs) From
                                 {{
                                 a - 2,
                                 varMobs
                                 }})

                    End If

                Next

            End If

        Loop

        'Puis je ferme le fichier.
        swReading.Close()

    End Sub

    Public Sub LoadPersonage()

        'J'ouvre et je lis le fichier.
        Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Personnage.txt")

        Do Until swReading.EndOfStream

            Dim lige As String = swReading.ReadLine

            If lige <> "" Then

                Dim separate() As String = Split(lige, "|")

                If Not VarPersonnage.ContainsKey(separate(0)) Then

                    Dim varPersonage As New sPersonnage

                    With varPersonage

                        .ID = separate(0)
                        .Nom = separate(1)
                        .Sexe = separate(2)

                    End With

                    VarPersonnage.Add(separate(0), varPersonage)

                End If

            End If

        Loop

        'Puis je ferme le fichier.
        swReading.Close()

    End Sub

    Public Sub ChargePnj()

        VarPnj.Clear()

        'J'ouvre et je lis le fichier.
        Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Pnj.txt")

        Do Until swReading.EndOfStream

            Dim line As String = swReading.ReadLine

            If line <> "" Then

                Dim separate() As String = Split(line, "|")

                If Not VarPnj.ContainsKey(separate(0)) Then

                    VarPnj.Add(separate(0), separate(1)) ' ID + Nom

                End If

            End If

        Loop

        'Puis je ferme le fichier.
        swReading.Close()

    End Sub

    Public Sub ChargePnjRéponse()

        Dim swReading As New IO.StreamReader("Data/PnjRéponse.txt")

        Do Until swReading.EndOfStream

            Dim line As String = swReading.ReadLine

            If line <> "" Then

                Dim separate() As String = Split(line, "=")

                VarPnjRéponse.Add(separate(0), separate(1))

            End If

        Loop

        swReading.Close()

    End Sub

    Public Sub ChargeMaison()

        'J'ouvre et je lis le fichier.
        Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Maison.txt")

        VarMaison.Clear()

        Do Until swReading.EndOfStream

            Dim line As String = swReading.ReadLine

            If line <> "" Then

                Dim separate As String() = Split(line, " | ")

                Dim hP As String = Split(separate(0), " : ")(1)
                Dim cellDoor As String = Split(separate(1), " : ")(1)
                Dim map As String = Split(separate(2), " : ")(1)
                Dim mapId As String = Split(separate(3), " : ")(1)
                Dim name As String = Split(separate(4), " : ")(1)

                If Not VarMaison.ContainsKey(hP) Then

                    Dim varHouse As New sMaison

                    With varHouse

                        .Nom = name
                        .Map = map
                        .CellulePorte = cellDoor
                        .MapId = mapId

                    End With

                    VarMaison.Add(hP, varHouse)

                End If

            End If

        Loop

        'Puis je ferme le fichier.
        swReading.Close()

    End Sub

    Public Sub ChargeMetier()

        Try

            VarMetier.Clear()

            Dim swReading As New IO.StreamReader("Data/Metier.txt")

            Do Until swReading.EndOfStream

                Dim Line As String = swReading.ReadLine

                If Line <> "" Then

                    Dim separate() As String = Split(Line.ToLower, "|")

                    Dim varJob As New sMetier

                    With varJob

                        .ID = separate(0)
                        .Nom = separate(1)
                        .AtelierRessource = New Dictionary(Of Integer, sMetierAtelierRessource)

                        For i = 2 To separate.Count - 1

                            Dim separateJob As String() = Split(separate(i), ":")

                            Dim newsMetierAtelierRessource As New sMetierAtelierRessource

                            With newsMetierAtelierRessource

                                .ID = separateJob(0)
                                .Nom = separateJob(1)
                                .Action = separateJob(2)

                            End With

                            .AtelierRessource.Add(separateJob(0), newsMetierAtelierRessource)

                        Next

                    End With

                    VarMetier.Add(separate(0), varJob)

                End If
            Loop

            swReading.Close()

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "LoadMetier", ex.Message)

        End Try

    End Sub

    Public Sub ChargeFamilier()

        'J'ouvre et je lis le fichier.
        Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Familier.txt")

        Do Until swReading.EndOfStream

            Dim line As String = swReading.ReadLine

            If line <> "" Then

                Dim separate As String() = Split(line, "|")

                Dim varPet As New sFamilier

                If VarFamilier.ContainsKey(separate(0)) Then

                    varPet = VarFamilier(separate(0))

                End If

                With varPet

                    .Nom = separate(1)

                    Dim separateData As String() = Split(separate(3), ",")

                    If VarFamilier.ContainsKey(separate(0)) Then

                        .Caracteristique = VarFamilier(separate(0)).Caracteristique

                    Else

                        .Caracteristique = New Dictionary(Of String, List(Of Integer))

                    End If

                    For a = 0 To separateData.Count - 1

                        If .Caracteristique.ContainsKey(separate(2).ToLower) Then

                            .Caracteristique(separate(2).ToLower).Add(separateData(a))

                        Else

                            .Caracteristique.Add(separate(2).ToLower, New List(Of Integer) From {separateData(a)})

                        End If

                    Next

                    separateData = Split(separate(4), ",")

                    .CapacitéNormal = separateData(0)
                    .CapacitéMax = separateData(1)

                    separateData = Split(separate(5), ",")

                    .IntervalRepasMin = separateData(0)
                    .IntervalRepasMax = separateData(1)

                End With

                If Not VarFamilier.ContainsKey(separate(0)) Then ' IdFamilier

                    VarFamilier.Add(separate(0), varPet)

                Else 'Il contient l'ID

                    VarFamilier(separate(0)) = varPet

                End If

            End If

        Loop

        swReading.Close()

    End Sub

    Public Sub ChargeCaractéristique()

        'J'ouvre et je lis le fichier.
        Dim swReading As New IO.StreamReader(Application.StartupPath + "\Data/Caractéristique.txt")

        Do Until swReading.EndOfStream

            Dim line As String = swReading.ReadLine

            If line <> "" Then

                Dim separate() As String = Split(line.Replace(" ", ""), "|")

                If Not VarCaractéristique.ContainsKey(separate(0)) Then

                    VarCaractéristique.Add(separate(0), New Dictionary(Of String, String()) From {{separate(1), separate}})

                Else

                    VarCaractéristique(separate(0)).Add(separate(1), separate)

                End If

            End If

        Loop

        'Puis je ferme le fichier.
        swReading.Close()

    End Sub


End Module
