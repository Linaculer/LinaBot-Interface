
Module mdlDragodinde

    Sub GiDragodindeEncloEtableEquipe(ByVal index As Integer, ByVal data As String, ByVal choix As String)

        With Comptes(index)

            Try

                ' Re+ 1200859   : 46 : 10,3,10,15,22,10,10,15,46,23,18,3,22,10 :           ,            :     : 0    :  7587      , 7250   , 9250   : 7      : 1        : 185 : 0 : 2249      , 10000         : 2000     , 2000         : 206     , 1205        : -10000       , -10000    , 10000  : 2500  , 10000     : -1          : 0 : 7d#7#0#0 , 7c#1#0#0 : 0       , 240         : 5     , 20
                ' Re+ Id unique : Id : Arbre généalogique                      : Capacité1 , Capacité 2 : Nom : Sexe :  Xp actuel , Xp Min , Xp Max : Niveau : Montable : ?   : ? : Endurance , Endurance Max : Maturité , Méturité max : Energie , Energie max :  Agressivité , Equilibré , Serein : Amour , Amour max : Fécondation : ? : +7 vita  , +1 sag   : Fatigue , Fatigue max : Repro , Repro max

                Dim sexe As String() = {"Mâle", "Femelle"}
                Dim capacité As String() = {"Infatigable", "Porteuse", "Reproductrice", "Sage", "Endurante", "Amoureuse", "Précoce", "Prédisposée Génétique", "Caméléone"}

                Dim separateData As String() = Split(data.Replace("Rd", "").Replace("Re+", "").Replace("Ee+", "").Replace("Ef+", "").Replace("Ee~", ""), ";")

                For i = 0 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ":")

                    Dim newDragodinde As New CDragodindeInformation

                    With newDragodinde

                        ' Id Unique
                        .IdUnique = separate(0)

                        'Nom Dragodinde
                        .NomDragodinde = "Dragodinde " & If(VarDragodindeId.ContainsKey(separate(1)), VarDragodindeId(separate(1)), separate(1))

                        'Arbre Généalogique
                        .ArbreGénéalogique = separate(2)

                        'Nom
                        .Nom = If(separate(4) = Nothing, "SansNom", separate(4))

                        'Sexe
                        .Sexe = sexe(separate(5))

                        'Niveau
                        .Niveau = separate(7)

                        'Expérience
                        .ExpMinimum = Split(separate(6), ",")(1)
                        .ExpActuelle = Split(separate(6), ",")(0)
                        .ExpMaximum = Split(separate(6), ",")(2)

                        'Montable
                        .Montable = If(separate(8) = "0", False, True)

                        'Endurance
                        .Endurance = Split(separate(11), ",")(0)
                        .EnduranceMax = Split(separate(11), ",")(1)

                        'Maturité
                        .Maturité = Split(separate(12), ",")(0)
                        .MaturitéMax = Split(separate(12), ",")(1)

                        'Amour
                        .Amour = Split(separate(15), ",")(0)
                        .AmourMax = Split(separate(15), ",")(1)

                        'Etat
                        .Agressivité = Split(separate(14), ",")(0)
                        .Equilibré = Split(separate(14), ",")(1)
                        .Serein = Split(separate(14), ",")(2)

                        'Energie
                        .Energie = Split(separate(13), ",")(0)
                        .EnergieMax = Split(separate(13), ",")(1)

                        'Fatigue
                        .Fatigue = Split(separate(19), ",")(0)
                        .FatigueMax = Split(separate(19), ",")(1)

                        'Capacité

                        If Split(separate(3), ",")(0) <> Nothing Then
                            .Capacité1 = capacité(Split(separate(3), ",")(0))
                        End If

                        If Split(separate(3), ",")(1) <> Nothing Then
                            .Capacité2 = capacité(Split(separate(3), ",")(1))
                        End If

                        'Caractéristique
                        If separate(18) <> Nothing Then

                            .Caractéristique = ItemCaracteristique(separate(18))

                        End If

                        'Fécondation
                        If CInt(separate(16)) > 0 Then

                            .Fécondation = "Feconde depuis : " & separate(16)

                        ElseIf (CInt(Split(separate(11), ",")(0)) >= 7500) AndAlso (CInt(Split(separate(15), ",")(0)) >= 7500) AndAlso CInt(Split(separate(12), ",")(0) = Split(separate(12), ",")(1)) AndAlso (CInt(separate(7)) >= 5) Then

                            .Fécondation = "Fecondable"

                        Else

                            .Fécondation = "Non Fecondable"

                        End If

                        'Reproduction
                        .Reproduction = Split(separate(20), ",")(0)
                        .ReproductionMax = Split(separate(20), ",")(1)

                    End With

                    Select Case choix

                        Case "information"

                            .Dragodinde.Information = newDragodinde

                        Case "equipe", "equiper"

                            .Dragodinde.Equipé = newDragodinde

                        Case "enclos", "enclo"

                            If .Dragodinde.Enclos.ContainsKey(separate(0)) Then

                                .Dragodinde.Enclos(separate(0)) = newDragodinde

                            Else

                                .Dragodinde.Enclos.Add(separate(0), newDragodinde)

                            End If

                        Case "etable"

                            If .Dragodinde.Etable.ContainsKey(separate(0)) Then

                                .Dragodinde.Etable(separate(0)) = newDragodinde

                            Else

                                .Dragodinde.Etable.Add(separate(0), newDragodinde)

                            End If

                    End Select

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDragodindeEncloEtableEquipé", data & vbCrLf & ex.Message)

            End Try

            .Dragodinde.Bloque.Set()

        End With

    End Sub

    Sub GiDragodindeMonterDesendu(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                Select Case Mid(data, 3)

                    Case "+"

                        .Dragodinde.Monter = True
                        EcritureMessage(index, "[Dofus]", "Vous êtes monté sur votre monture", Color.Green)

                    Case "-"

                        .Dragodinde.Monter = False
                        EcritureMessage(index, "[Dofus]", "Vous êtes Déscendu de votre monture", Color.Green)

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDragodindeMonterDesendu", data & vbCrLf & ex.Message)

            End Try

            .Dragodinde.Bloque.Set()

        End With

    End Sub

    Sub GiDragodindeExperienceDonnee(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                With .Dragodinde

                    .XpDonnee = Mid(data, 3)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDragodindeExperienceDonnee", data & vbCrLf & ex.Message)

            End Try

            .Dragodinde.Bloque.Set()

        End With

    End Sub

    Sub GiDragodindeNom(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                With .Dragodinde.Equipé

                    .Nom = Mid(data, 3)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDragodindeNom", data & vbCrLf & ex.Message)

            End Try

            .Dragodinde.Bloque.Set()

        End With

    End Sub

    Sub GiDragodindePods(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'Ew 0        ; 185
                'Ew Actuelle ; Maximum

                Dim separateData As String() = Split(Mid(data, 3), ";")

                With .Dragodinde

                    .PodsActuelle = separateData(0)
                    .PodsMaximum = separateData(1)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDragodindePods", data & vbCrLf & ex.Message)

            End Try

            .Dragodinde.Bloque.Set()

        End With

    End Sub

End Module

#Region "Dragodinde"

Public Class CDragodinde

    Public PodsActuelle, PodsMaximum As Integer
    Public XpDonnee As Integer
    Public EnInventaire As Boolean
    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public Enclos As New Dictionary(Of Integer, CDragodindeInformation)
    Public Etable As New Dictionary(Of Integer, CDragodindeInformation)
    Public Equipé, Information As New CDragodindeInformation
    Public Monter As Boolean

End Class

Public Class CDragodindeInformation

    Public IdUnique As Integer
    Public ID As Integer
    Public ArbreGénéalogique As String
    Public Capacité1, Capacité2 As String
    Public Nom, NomDragodinde As String
    Public Sexe As String
    Public ExpActuelle As Integer
    Public ExpMinimum As Integer
    Public ExpMaximum As Integer
    Public Niveau As Integer
    Public Montable As Boolean
    Public Endurance, EnduranceMax As Integer
    Public Maturité, MaturitéMax As Integer
    Public Energie, EnergieMax As Integer
    Public Amour, AmourMax As Integer
    Public Agressivité, Equilibré, Serein As Integer
    Public Fécondation As String
    Public Fatigue, FatigueMax As Integer
    Public Reproduction, ReproductionMax As Integer
    Public PodsActuelle, PodsMaximum As Integer
    Public Caractéristique As New CItemCaractéristique

End Class

#End Region
