Module MdlCaractéristique

    Sub GiCaracteristique(index As Integer, data As String)

        With Comptes(index)

            Try

#Region "Information"

                'As 93821075 ,92071000  ,95886000   |165888|10                                 |8                      |0~0,0,0,0,0,0|793       ,793        |10000         ,10000          |439       |100        |6      ,2            ,0      ,0        ,8       |3      ,0            ,0      ,0        ,3       |0         ,-15             ,0         ,0           |0,248,0,0|220,137,0,0|0,0,0,0|1,30,0,0|158,250,0,0 |0,0,0,0|1,0,0,0|0,0,0,0|0,0,0,0         |0,0,0,0 |0,0,0,0 |0,7,0,0|0,0,0,0      |0,0,0,0       |0,0,0,0        |0,5,0,0|0,0,0,0|55,34,0,0|55,34,0,0|0,4,0,0               |0,0,0,0                |0,0,0,0                   |0,0,0,0                    |0,5,0,0              |0,1,0,0               |0,0,0,0                  |0,0,0,0                   |0,10,0,0           |0,2,0,0             |0,0,0,0                |0,0,0,0                 |0,4,0,0            |0,0,0,0             |0,0,0,0                |0,0,0,0                 |0,3,0,0            |0,0,0,0             |0,0,0,0                |0,0,0,0                 |19
                'As XP Actuel,Xp Minimum,XP Maximum |Kamas |Capital Caractéristiques disponible|Capital Sort disponible|Inconnu      |Pdv_Actuel,PDV_Maximum|Energie_Actuel,Energie_Maximum|Initiative|Prospection|PA_Base,PA_Equipement,PA_Dons,PA_Booste,PA_Total|PM_Base,PM_Equipement,PM_Dons,PM_Booste,PM_Total|Force_Base,Force_Equipement,Force_Dons,Force_Booste|Vitalité |Sagesse    |Chance |Agilité |Intelligence|PO     |Invoc  |Dommage|Dommage Physique|Maîtrise|%Dommage|Soin   |Dommage_Piège|%Dommage_Piège|Renvoie dommage|CC     |EC     |Esq PA   | Esq PM  |Résistance_Neutre_Fixe|%Résistance_Neutre_Fixe|pvp_Résistance_Neutre_Fixe|pvp_%Résistance_Neutre_Fixe|Résistance_Terre_Fixe|%Résistance_Terre_Fixe|pvp_Résistance_Terre_Fixe|pvp_%Résistance_Terre_Fixe|Résistance_Eau_Fixe|%Résistance_Eau_Fixe|pvp_Résistance_Eau_Fixe|pvp_%Résistance_Eau_Fixe|Résistance_Air_Fixe|%Résistance_Air_Fixe|pvp_Résistance_Air_Fixe|pvp_%Résistance_Air_Fixe|Résistance_Feu_Fixe|%Résistance_Feu_Fixe|pvp_Résistance_Feu_Fixe|pvp_%Résistance_Feu_Fixe|Inconnu

                'data reçu :
                'AsXp_Actuel,XP_Minimum,Xp_Maximum| = 0
                'Kamas| = 1
                'Capital Caractéristiques disponible| = 2
                'Capital Sort disponible| = 3
                '0~0,0,0,0,0,0| = 4 (Inconnu)
                'Pdv_Actuel,PDV_Maximum| = 5
                'Energie_Actuel,Energie_Maximum| = 6
                'Initiative_Actuel| = 7
                'Prospection_Actuel| = 8

                'PA_Base,PA_Equipement,PA_Dons,PA_Booste,PA_Total| = 9
                'PM_Base,PM_Equipement,PM_Dons,PM_Booste,PM_Total| = 10

                'Force_Base,Force_Equipement,Force_Dons,Force_Booste| = 11
                'Vitalité_Base,Vitalité_Equipement,Vitalité_Dons,Vitalité_Booste| = 12
                'Sagesse_Base,Sagesse_Equipement,Sagesse_Dons,Sagesse_Booste| = 13
                'Chance_Base,Chance_Equipement,Chance_Dons,Chance_Booste| = 14
                'Agilité_Base,Agilité_Equipement,Agilité_Dons,Agilité_Booste| = 15
                'Intelligence_Base,Intelligence_Equipement,Intelligence_Dons,Intelligence_Booste| = 16

                'PO_Base,PO_Equipement,PO_Dons,PO_Booste| = 17
                'Invocation_Base,Invocation_Equipement,Invocation_Dons,Invocation_Booste| = 18
                'Dommage_Base,Dommage_Equipement,Dommage_Dons,Dommage_Booste| = 19
                'Dommage_Physique_Base,Dommage_Physique_Equipement,Dommage_Physique_Dons,Dommage_Physique_Booste| = 20
                'Maîtrise_Base,Maîtrise_Equipement,Maîtrise_Dons,Maîtrise_Booste| = 21
                '%Dommage_Base,%Dommage_Equipement,%Dommage_Dons,%Dommage_Booste| = 22
                'Soin_Base,Soin_Equipement,Soin_Dons,Soin_Booste| = 23
                'Dommage_Piège_Base,Dommage_Piège_Equipement,Dommage_Piège_Dons,Dommage_Piège_Booste| = 24
                '%Dommage_Piège_Base,%Dommage_Piège_Equipement,%Dommage_Piège_Dons,%Dommage_Piège_Booste| = 25
                'Renvoie_Dommage_Base,Renvoie_Dommage_Equipement,Renvoie_Dommage_Dons,Renvoie_Dommage_Booste| = 26

                'Coups_Critiques_Base,Coups_Critiques_Equipement,Coups_Critiques_Dons,Coups_Critiques_Booste| = 27
                'Echec_Critique_Base,Echec_Critique_Equipement,Echec_Critique_Dons,Echec_Critique_Booste| = 28

                'Esquive_PA_Base,Esquive_PA_Equipement,Esquive_PA_Dons,Esquive_PA_Booste| = 29
                'Esquive_PM_Base,Esquive_PM_Equipement,Esquive_PM_Dons,Esquive_PM_Booste| = 30

                'Résistance_Neutre_Fixe_Base,Résistance_Neutre_Fixe_Equipement,Résistance_Neutre_Fixe_Dons,Résistance_Neutre_Fixe_Booste| = 31
                '%Résistance_Neutre_Fixe_Base,%Résistance_Neutre_Fixe_Equipement,%Résistance_Neutre_Fixe_Dons,%Résistance_Neutre_Fixe_Booste| = 32
                'Résistance_Neutre_Fixe_PVP_Base,Résistance_Neutre_Fixe_PVP_Equipement,Résistance_Neutre_Fixe_PVP_Dons,Résistance_Neutre_Fixe_PVP_Booste| = 33
                '%Résistance_Neutre_Fixe_PVP_Base,%Résistance_Neutre_Fixe_PVP_Equipement,%Résistance_Neutre_Fixe_PVP_Dons,%Résistance_Neutre_Fixe_PVP_Booste| = 34

                'Résistance_Terre_Fixe_Base,Résistance_Terre_Fixe_Equipement,Résistance_Terre_Fixe_Dons,Résistance_Terre_Fixe_Booste| = 35
                '%Résistance_Terre_Fixe_Base,%Résistance_Terre_Fixe_Equipement,%Résistance_Terre_Fixe_Dons,%Résistance_Terre_Fixe_Booste| = 36
                'Résistance_Terre_Fixe_PVP_Base,Résistance_Terre_Fixe_PVP_Equipement,Résistance_Terre_Fixe_PVP_Dons,Résistance_Terre_Fixe_PVP_Booste| = 37
                '%Résistance_Terre_Fixe_PVP_Base,%Résistance_Terre_Fixe_PVP_Equipement,%Résistance_Terre_Fixe_PVP_Dons,%Résistance_Terre_Fixe_PVP_Booste| = 38

                'Résistance_Eau_Fixe_Base,Résistance_Eau_Fixe_Equipement,Résistance_Eau_Fixe_Dons,Résistance_Eau_Fixe_Booste| = 39
                '%Résistance_Eau_Fixe_Base,%Résistance_Eau_Fixe_Equipement,%Résistance_Eau_Fixe_Dons,%Résistance_Eau_Fixe_Booste| = 40
                'Résistance_Eau_Fixe_PVP_Base,Résistance_Eau_Fixe_PVP_Equipement,Résistance_Eau_Fixe_PVP_Dons,Résistance_Eau_Fixe_PVP_Booste| = 41
                '%Résistance_Eau_Fixe_PVP_Base,%Résistance_Eau_Fixe_PVP_Equipement,%Résistance_Eau_Fixe_PVP_Dons,%Résistance_Eau_Fixe_PVP_Booste| = 42

                'Résistance_Air_Fixe_Base,Résistance_Air_Fixe_Equipement,Résistance_Air_Fixe_Dons,Résistance_Air_Fixe_Booste| = 43
                '%Résistance_Air_Fixe_Base,%Résistance_Air_Fixe_Equipement,%Résistance_Air_Fixe_Dons,%Résistance_Air_Fixe_Booste| = 44
                'Résistance_Air_Fixe_PVP_Base,Résistance_Air_Fixe_PVP_Equipement,Résistance_Air_Fixe_PVP_Dons,Résistance_Air_Fixe_PVP_Booste| = 45
                '%Résistance_Air_Fixe_PVP_Base,%Résistance_Air_Fixe_PVP_Equipement,%Résistance_Air_Fixe_PVP_Dons,%Résistance_Air_Fixe_PVP_Booste| = 46

                'Résistance_Feu_Fixe_Base,Résistance_Feu_Fixe_Equipement,Résistance_Feu_Fixe_Dons,Résistance_Feu_Fixe_Booste| = 47
                '%Résistance_Feu_Fixe_Base,%Résistance_Feu_Fixe_Equipement,%Résistance_Feu_Fixe_Dons,%Résistance_Feu_Fixe_Booste| = 48
                'Résistance_Feu_Fixe_PVP_Base,Résistance_Feu_Fixe_PVP_Equipement,Résistance_Feu_Fixe_PVP_Dons,Résistance_Feu_Fixe_PVP_Booste| = 49
                '%Résistance_Feu_Fixe_PVP_Base,%Résistance_Feu_Fixe_PVP_Equipement,%Résistance_Feu_Fixe_PVP_Dons,%Résistance_Feu_Fixe_PVP_Booste| = 50

                '73 = 51 (Inconnu)

#End Region

                Dim separateData As String() = Split(Mid(data, 3), "|")
                Dim separate As String()

                With .Personnage

                    'Kamas
                    .Kamas = separateData(1)

                    'Capital Sort
                    .CapitalSort = separateData(3)

                    ' XP 26980    , 25200   , 32600
                    ' XP Actuelle , Minimum , Maximum
                    separate = Split(separateData(0), ",")

                    With .Experience

                        .Minimum = separate(1)
                        .Maximum = separate(2)
                        .Actuelle = separate(0)
                        .Pourcentage = (separate(0) - separate(1)) / (separate(2) - separate(1)) * 100

                    End With

                    'Point de vie Actuel
                    separate = Split(separateData(5), ",")

                    '793      , 793
                    'Actuelle , Maximum
                    With .Vitaliter

                        .Maximum = separate(1)
                        .Actuelle = If(separate(0) < 0, 0, separate(0))
                        .Pourcentage = If(separate(0) < 0, 0, separate(0) / separate(1) * 100)

                    End With

                    'Energie
                    separate = Split(separateData(6), ",")

                    '8956     , 10000
                    'Actuelle , Maximum
                    With .Energie

                        .Maximum = separate(1)
                        .Actuelle = separate(0)
                        .Pourcentage = If(separate(0) < 0, 0, separate(0) / separate(1) * 100)

                    End With

                End With

                'Capital Caractéristique
                .Caracteristique.Capital = separateData(2)

                Dim nomCaracteristique As String() = {"initiative", "prospection", "pa", "pm", "force", "vitaliter",
                            "sagesse", "chance", "agiliter", "intelligence", "po", "creature invocable", "dommage", "dommage physique", "maitrise arme",
                            "dommage pr", "soin", "piege", "piege pr", "renvoi dommage", "coup critique", "echec critique", "esquive pa", "esquive pm",
                            "resistance neutre pvm", "resistance neutre pvm pr", "resistance neutre pvp", "resistance neutre pvp pr",
                            "resistance terre pvm", "resistance terre pvm pr", "resistance terre pvp", "resistance terre pvp pr",
                            "resistance eau pvm", "resistance eau pvm pr", "resistance eau pvp", "resistance eau pvp pr",
                            "resistance air pvm", "resistance air pvm pr", "resistance air pvp", "resistance air pvp pr",
                            "resistance feu pvm", "resistance feu pvm pr", "resistance feu pvp", "resistance feu pvp pr"}

                For i = 0 To 43

                    separate = Split(separateData(7 + i), ",")

                    '55   , 72         , 60   , 60
                    'Base , Equipement , Dons , Booste

                    With .Caracteristique.Caracteristique(nomCaracteristique(i))

                        .Base = separate(0)
                        .Equipement = If(separate.Count > 1, separate(1), "0")
                        .Dons = If(separate.Count > 1, separate(2), "0")
                        .Boost = If(separate.Count > 1, separate(3), "0")

                        Select Case separate.Count

                            Case > 3

                                .Total = CInt(separate(0)) + CInt(separate(1)) + CInt(separate(2)) + CInt(separate(3))

                            Case Else 'Si unique, genre initiative + prospection.

                                .Total = separate(0)

                        End Select

                    End With

                Next

                separate = Split(separateData(5), ",")

                .Personnage.Regeneration = CInt(separate(0)) / CInt(separate(1)) * 100

                If .MITM = False Then

                    .Send("BD") 'Permet d'avoir 'BD649|6|6' = la date.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "[GiCaractéristique]", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiPods(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(Sub() GiPods(index, data)))

                Else

                    ' Ow 750         | 3353
                    ' Ow Pods actuel | Pods Max

                    Dim separateData As String() = Split(Mid(data, 3), "|")



                    With .ProgressBar_Pods

                        .Text = Math.Round(separateData(0) / separateData(1)) * 100 & "%"
                        .Maximum = separateData(1)
                        .Value = separateData(0)

                    End With

                    .ToolTip1.SetToolTip(.ProgressBar_Pods, separateData(0) & " / " & separateData(1))

                End If

            Catch ex As Exception

                ErreurFichier(index, Comptes(index).Personnage.NomDuPersonnage, "GiPods", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiRegenerationSeconde(index As Integer, data As String)

        Try

            With Comptes(index).FrmUser

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(Sub() GiRegenerationSeconde(index, data)))

                Else

                    ' ILS 2000
                    ' ILS Temps à attendre pour 1 pdv

                    EcritureMessage(index, "(Dofus)", "Votre personnage récupére 1 point de vie toutes les " & Mid(data, 4, 1) & " seconde(s).", Color.Green)

                    .TimerRegeneration.Enabled = False
                    .TimerRegeneration.Interval = Mid(data, 4)
                    .TimerRegeneration.Enabled = True

                End If

            End With

        Catch ex As Exception

            ErreurFichier(index, Comptes(index).Personnage.NomDuPersonnage, "GiRégénérationSeconde", data & vbCrLf & ex.Message)

        End Try

    End Sub

    Sub GiNiveauUp(index As Integer, data As String)

        Try

            With Comptes(index)

                ' AN 2
                ' AN Niveau

                .Personnage.Niveau = Mid(data, 3)

                EcritureMessage(index, "[Dofus]", "Tu passes niveau " & Mid(data, 3) & vbCrLf & "Tu gagnes 5 points pour faire évoluer tes caractéristiques et 1 point pour tes sorts.", Color.Green)

                .TabPage_Bot.Text = Split(.TabPage_Bot.Text, " ")(0) & " (" & Mid(data, 3) & ")"

            End With

        Catch ex As Exception

            ErreurFichier(index, Comptes(index).Personnage.NomDuPersonnage, "GiNiveauUp", data & vbCrLf & ex.Message)

        End Try

    End Sub

    Sub GiPdvRestaure(index As Integer, data As String)

        Try

            With Comptes(index)

                ' ILF 1
                ' ILF Nbr de pdv récupèré

                EcritureMessage(index, "(Dofus)", "Tu as retrouvé " & Mid(data, 4) & " points de vie.", Color.Green)

                With .Personnage.Vitaliter

                    Dim Vitalite As Integer = .Actuelle + CInt(Mid(data, 4))

                    If Vitalite > .Maximum Then

                        .Actuelle = .Maximum

                    Else

                        .Actuelle = Vitalite

                    End If

                End With

            End With

            InterfaceGiPdvRestaure(index, data)

        Catch ex As Exception

            ErreurFichier(index, Comptes(index).Personnage.NomDuPersonnage, "GiPdvRestauré", data & vbCrLf & ex.Message)

        End Try

    End Sub

    Private Sub InterfaceGiPdvRestaure(index As Integer, data As String)

        Try

            With Comptes(index).FrmUser

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiPdvRestaure), index, data)

                Else

                    ' ILF 1
                    ' ILF Nbr de pdv récupèré

                    With .ProgressBar_Vitaliter

                        Dim Vitalite As Integer = .Value + CInt(Mid(data, 4))

                        If Vitalite > .Maximum Then

                            .Value = .Maximum

                        Else

                            .Value = Vitalite

                        End If

                        .Text = Math.Round((.Value / .Maximum) * 100) & "%"

                    End With

                End If

            End With

        Catch ex As Exception

        End Try

    End Sub

    Sub GiCaracteristiqueUpEchec(index As Integer, data As String)

        With Comptes(index)

            Try

                ' ABE
                ' Echec

                EcritureMessage(index, "[Dofus]", "Impossible de up la caractéristique.", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiNiveauUp", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module

#Region "Class"

Public Class CCaracteristique

    Public Caracteristique As New Dictionary(Of String, ClassCaracteristique) From
        {
            {"initiative", New ClassCaracteristique},
            {"prospection", New ClassCaracteristique},
            {"pa", New ClassCaracteristique},
            {"pm", New ClassCaracteristique},
            {"force", New ClassCaracteristique},
            {"vitaliter", New ClassCaracteristique},
            {"sagesse", New ClassCaracteristique},
            {"chance", New ClassCaracteristique},
            {"agiliter", New ClassCaracteristique},
            {"intelligence", New ClassCaracteristique},
            {"po", New ClassCaracteristique},
            {"creature invocable", New ClassCaracteristique},
            {"dommage", New ClassCaracteristique},
            {"dommage physique", New ClassCaracteristique},
            {"maitrise arme", New ClassCaracteristique},
            {"dommage pr", New ClassCaracteristique},
            {"soin", New ClassCaracteristique},
            {"piege", New ClassCaracteristique},
            {"piege pr", New ClassCaracteristique},
            {"renvoi dommage", New ClassCaracteristique},
            {"coup critique", New ClassCaracteristique},
            {"echec critique", New ClassCaracteristique},
            {"esquive pa", New ClassCaracteristique},
            {"esquive pm", New ClassCaracteristique},
            {"resistance neutre pvm", New ClassCaracteristique},
            {"resistance neutre pvm pr", New ClassCaracteristique},
            {"resistance neutre pvp", New ClassCaracteristique},
            {"resistance neutre pvp pr", New ClassCaracteristique},
            {"resistance terre pvm", New ClassCaracteristique},
            {"resistance terre pvm pr", New ClassCaracteristique},
            {"resistance terre pvp", New ClassCaracteristique},
            {"resistance terre pvp pr", New ClassCaracteristique},
            {"resistance eau pvm", New ClassCaracteristique},
            {"resistance eau pvm pr", New ClassCaracteristique},
            {"resistance eau pvp", New ClassCaracteristique},
            {"resistance eau pvp pr", New ClassCaracteristique},
            {"resistance air pvm", New ClassCaracteristique},
            {"resistance air pvm pr", New ClassCaracteristique},
            {"resistance air pvp", New ClassCaracteristique},
            {"resistance air pvp pr", New ClassCaracteristique},
            {"resistance feu pvm", New ClassCaracteristique},
            {"resistance feu pvm pr", New ClassCaracteristique},
            {"resistance feu pvp", New ClassCaracteristique},
            {"resistance feu pvp pr", New ClassCaracteristique}
        }
    Public Capital As Integer
    Public [Option] As New CCaracteristiqueOption

End Class

Public Class ClassCaracteristique

    Public Base As String
    Public Equipement As String
    Public Dons As String
    Public Boost As String
    Public Total As String

End Class

Public Class CCaracteristiqueOption

    Public Vitaliter As New CCaracteristiqueOptionValeur
    Public Sagesse As New CCaracteristiqueOptionValeur
    Public Force As New CCaracteristiqueOptionValeur
    Public Agiliter As New CCaracteristiqueOptionValeur
    Public Chance As New CCaracteristiqueOptionValeur
    Public Intelligence As New CCaracteristiqueOptionValeur

End Class

Public Class CCaracteristiqueOptionValeur

    Public Minimum As Integer
    Public Maximum As Integer
    Public Prioriter As Integer

End Class

#End Region
