Module MdlMétier

    Sub GiMetierModePublic(index As Integer, data As String)

        With Comptes(index)

            Try

                'EW + OU -

                For Each Pair As CMetier In .Metier.Values

                    With Pair

                        Select Case Mid(data, 3)

                            Case "+"

                                .ModePublic = True

                            Case "-"

                                .ModePublic = False

                        End Select

                    End With

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMétierModePublic", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMetierInformation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' JS | 17        ; 142                   ~ 2                    ~ 0                    ~ 0 ~ 70                  , next info métier actuel | Nex métier
                ' JS | ID_Métier ; ID_Atelier/ressources ~ Nbr_Case/Récolte min ~ Nbr_Case/Récolte max ~ ? ~ %_Réussite ou temps ,                         |

                Dim separateData As String() = Split(data, "|")

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim idJob As Integer = separate(0)

                    separate = Split(separate(1), ",")

                    Dim newMetier As New CMetier

                    With newMetier

                        .ID = idJob
                        .Nom = VarMetier(idJob).Nom

                        .AtelierRessource = New Dictionary(Of Integer, CMetierAtelierRessource)

                        For a = 0 To separate.Count - 1

                            Dim separateCraft As String() = Split(separate(a), "~")

                            Dim newMétierAtelierRessource As New CMetierAtelierRessource

                            With newMétierAtelierRessource

                                .ID = separateCraft(0)
                                .Nom = VarMetier(idJob).AtelierRessource(separateCraft(0)).Nom
                                .NombreCaseRecolteMinimum = separateCraft(1)
                                .NombreCaseRecolteMaximum = separateCraft(2)
                                .TempsReussite = separateCraft(4)
                                .Action = VarMetier(idJob).AtelierRessource(separateCraft(0)).Action

                            End With

                            If .AtelierRessource.ContainsKey(separateCraft(0)) Then

                                .AtelierRessource(separateCraft(0)) = newMétierAtelierRessource

                            Else

                                .AtelierRessource.Add(separateCraft(0), newMétierAtelierRessource)

                            End If

                        Next

                    End With

                    If .Metier.ContainsKey(idJob) Then

                        .Metier.Remove(idJob)

                    End If

                    .Metier.Add(idJob, newMetier)

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMetierInformation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMetierUp(index As Integer, data As String)

        With Comptes(index)

            Try

                'JN 28        | 73
                'JN ID Métier | Level

                Dim separateData As String() = Split(Mid(data, 3), "|")

                If .Metier.ContainsKey(separateData(0)) Then

                    .Metier(separateData(0)).Niveau = separateData(1)

                End If

                EcritureMessage(index, "(Dofus)", "Ton métier " & VarMetier(separateData(0)).Nom & " passe niveau " & separateData(1) & ".", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMetierUp", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMetierExperience(index As Integer, data As String)

        With Comptes(index)

            Try

                ' JX | 17         ; 42     ; 41044   ; 43205      ; 43378   ; ? |
                ' JX | ID_Métiers ; Niveau ; Exp_Min ; Exp_actuel ; Exp_Max ;   | Métier_Suivant

                Dim separateData As String() = Split(data, "|")

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    If separate(4) < 0 Then separate(4) = separate(3)

                    If .Metier.ContainsKey(separate(0)) Then

                        With .Metier(separate(0))

                            .Niveau = separate(1)
                            .ExperienceMinimum = separate(2)
                            .ExperienceActuelle = separate(3)
                            .ExperienceMaximum = separate(4)

                        End With

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMetierExperience", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMetierOption(index As Integer, data As String)

        With Comptes(index)

            Try

                'JO 0             | 4                      | 5
                'JO Numéro_Métier | Nombre_Pour_Check_Case | Nbr minimum ingrédient 

                Dim separateData As String() = Split(Mid(data, 3), "|")

                For Each pair As CMetier In .Metier.Values

                    If CInt(separateData(0)) = 0 Then

                        With pair

                            .Payant = False
                            .NeFournitAucuneRessource = False
                            .GratuitSurEchec = False
                            .NombreIngredientMinimum = separateData(2)

                            While CInt(separateData(1)) > 0

                                Select Case separateData(1)

                                    Case >= 4 'Ne Fournit aucune ressource

                                        .NeFournitAucuneRessource = True
                                        separateData(1) = CInt(separateData(1)) - 4

                                    Case >= 2 'Gratuit sur échec

                                        .GratuitSurEchec = True
                                        separateData(1) = CInt(separateData(1)) - 2

                                    Case >= 1 'Payant

                                        .Payant = True
                                        separateData(1) = CInt(separateData(1)) - 1

                                End Select

                            End While

                        End With

                        Exit For

                    Else

                        separateData(0) = CInt(separateData(0)) - 1

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMetierOption", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMetierSupprime(index As Integer, data As String)

        With Comptes(index)

            Try

                ' JR 56
                ' JR id métier

                If .Metier.ContainsKey(Mid(data, 3)) Then

                    .Metier.Remove(Mid(data, 3))

                End If

                EcritureMessage(index, "[Dofus]", "Tu as désappris le métier " & VarMetier(Mid(data, 3)).Nom & ".", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMetierSupprime", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module

#Region "Class"

Public Class CMetier

    Public Nom As String
    Public ID As Integer
    Public Niveau As Integer
    Public ItemEquipe As Boolean
    Public ExperienceMinimum As Integer
    Public ExperienceActuelle As Integer
    Public ExperienceMaximum As Integer
    Public Action As String
    Public NeFournitAucuneRessource As Boolean
    Public GratuitSurEchec As Boolean
    Public Payant As Boolean
    Public NombreIngredientMinimum As Integer
    Public ModePublic As Boolean

    Public AtelierRessource As New Dictionary(Of Integer, CMetierAtelierRessource)

End Class

Public Class CMetierAtelierRessource

    Public Nom As String
    Public ID As Integer
    Public Action As String
    Public NombreCaseRecolteMinimum As Integer
    Public NombreCaseRecolteMaximum As Integer
    Public TempsReussite As Integer

End Class

#End Region