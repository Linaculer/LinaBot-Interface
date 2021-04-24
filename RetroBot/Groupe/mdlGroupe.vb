Module mdlGroupe

    Sub GiGroupeRecoitInvitation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PIK Linaculer           | Talark
                ' PIK Personne qui invite | Invité

                .Groupe.EnInvitation = True

                Dim separateData As String() = Split(Mid(data, 4), "|")

                .Groupe.Inviteur = separateData(0)
                .Groupe.Inviter = separateData(1)

                If separateData(0).ToLower = .Personnage.NomDuPersonnage.ToLower Then

                    EcritureMessage(index, "[Dofus]", "Tu invites " & separateData(1) & " à rejoindre ton groupe...", Color.Green)

                Else

                    EcritureMessage(index, "[Dofus]", separateData(0) & " t'invite à rejoindre son groupe." & vbCrLf &
                                "Acceptes-tu ?", Color.Green)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeRecoitInvitation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeRejoint(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PCK Linaculer
                ' PCK nom

                .Groupe.EnInvitation = False
                .Groupe.EnGroupe = True

                EcritureMessage(index, "[Dofus]", "Tu viens de rejoindre le groupe de " & Mid(data, 4) & ".", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeRejoint", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeErreur(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PIE a
                ' PIE n Nom

                .Groupe.EnInvitation = False

                Select Case Mid(data, 4, 1)

                    Case "a"

                        EcritureMessage(index, "[Dofus]", "Impossible, ce joueur fait déjà partie d'un groupe.", Color.Red)

                    Case "n"

                        EcritureMessage(index, "[Dofus]", Mid(data, 5) & " n'est pas connecté ou n'existe pas.", Color.Red)

                End Select


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeErreur", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeChefID(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PL 1234567
                ' PL id chef

                .Groupe.ChefID = Mid(data, 3)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeChefID", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeFinInvitation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PR

                .Groupe.EnInvitation = False

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeFinInvitation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeAjouteMembre(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PM+ 1234567   ; Linaculer ; 91         ; -1       ; -1       ; -1       ;     , 9aa    , 9a9  ,          ,          ; 111        , 111     ; 4      ; 203          ; 104         ; 0 |Next
                ' PM+ Id Unique ; Nom       ; Id Classe  ; Couleur1 ; Couleur2 ; Couleur3 ; Cac , Coiffe , Cape , Familier , Bouclier ; Pdv actuel , Pdv Max ; Niveau ; Initiative   ; Prospection ; ? |

                Dim separateData As String() = Split(Mid(data, 4), "|")

                For i = 0 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim separateInfo As String() = Split(separate(6), ",")

                    If Not .Groupe.Membre.ContainsKey(separate(0)) Then

                        Dim newGroupe As New CGroupeMembre

                        With newGroupe

                            .IdUnique = separate(0) ' IdUnique
                            .Nom = separate(1) ' Nom
                            .ClasseSexe = separate(2) ' IdClasse
                            .Couleur1 = "&H" & separate(3) ' Couleur1
                            .Couleur2 = "&H" & separate(4) ' Couleur2
                            .Couleur3 = "&H" & separate(5) ' Couleur3
                            .Cac = If(separateInfo(0) = "", 0, Convert.ToInt64(separateInfo(0), 16).ToString) ' Cac
                            .Coiffe = If(separateInfo(1) = "", 0, Convert.ToInt64(separateInfo(1), 16).ToString) ' Coiffe
                            .Cape = If(separateInfo(2) = "", 0, Convert.ToInt64(separateInfo(2), 16).ToString) ' Cape
                            .Familier = If(separateInfo(3) = "", 0, Convert.ToInt64(separateInfo(3), 16).ToString) ' Familier
                            .Bouclier = If(separateInfo(4) = "", 0, Convert.ToInt64(separateInfo(4), 16).ToString) ' Bouclier

                            separateInfo = Split(separate(7), ",")

                            .PdvActuel = separateInfo(0) ' Pdv actuel
                            .PdvMaximum = separateInfo(1) ' Pdv Max
                            .Niveau = separate(8) ' Niveau
                            .Initiative = separate(9) ' Initiative
                            .Prospection = separate(10) ' Prospection
                        End With

                        .Groupe.Membre.Add(separate(0), newGroupe)

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeAjouteMembre", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeSupprimeMembre(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PM- 01234567
                ' PM- Id Unique

                data = Mid(data, 4)

                EcritureMessage(index, "[Groupe]", "Le joueur " & .Groupe.Membre(data).Nom & " a quitté le groupe.", Color.Red)

                .Groupe.Membre.Remove(data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeSupprimeMembre", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeModifieMembre(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PM~ 1234567   ; Linaculer ; 91         ; -1       ; -1       ; -1       ;     , 9aa    , 9a9  ,          ,          ; 107        , 111     ; 4      ; 195        ; 104         ; 0 
                ' PM~ Id Unique ; Nom       ; Id Classe  ; Couleur1 ; Couleur2 ; Couleur3 ; Cac , Coiffe , Cape , Familier , Bouclier ; Pdv actuel , Pdv Max ; Niveau ; Initiative ; Prospection ; ? 

                Dim separate As String() = Split(Mid(data, 4), ";")

                Dim separateInfo As String() = Split(separate(6), ",")

                If .Groupe.Membre.ContainsKey(separate(0)) Then

                    With .Groupe.Membre(separate(0))

                        .IdUnique = separate(0) ' IdUnique
                        .Nom = separate(1) ' Nom
                        .ClasseSexe = separate(2) ' IdClasse
                        .Couleur1 = "&H" & separate(3) ' Couleur1
                        .Couleur2 = "&H" & separate(4) ' Couleur2
                        .Couleur3 = "&H" & separate(5) ' Couleur3
                        .Cac = If(separateInfo(0) <> Nothing, Convert.ToInt64(separateInfo(0), 16).ToString, "") ' Cac
                        .Coiffe = If(separateInfo(1) <> Nothing, Convert.ToInt64(separateInfo(1), 16).ToString, "") ' Coiffe
                        .Cape = If(separateInfo(2) <> Nothing, Convert.ToInt64(separateInfo(2), 16).ToString, "") ' Cape
                        .Familier = If(separateInfo(3) <> Nothing, Convert.ToInt64(separateInfo(3), 16).ToString, "") ' Familier
                        .Bouclier = If(separateInfo(4) <> Nothing, Convert.ToInt64(separateInfo(4), 16).ToString, "") ' Bouclier

                        separateInfo = Split(separate(7), ",")

                        .PdvActuel = separateInfo(0) ' Pdv actuel
                        .PdvMaximum = separateInfo(1) ' Pdv Max
                        .Niveau = separate(8) ' Niveau
                        .Initiative = separate(9) ' Initiative
                        .Prospection = separate(10) ' Prospection

                    End With

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeModifieMembre", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeQuitte(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PV 1234567
                ' PV Id joueur

                .Groupe = New CGroupe

                If data = "PV" Then

                        EcritureMessage(index, "[Dofus]", "Tu as quitté ton groupe.", Color.Green)

                    Else

                        EcritureMessage(index, "[Dofus]", .Groupe.Membre(Mid(data, 3)).Nom & " vient de t'exclure du groupe.", Color.Green)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeQuitte", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeSuivezTous(index As Integer, data As String)

        With Comptes(index)

            Try

                ' PFK 1234567
                ' PFK id Unique

                'Donne l'ID de la personne qui est suivi
                'si personne seulement PFK

                If data = "PFK" Then

                    .Groupe.SuivreID = 0

                Else

                    .Groupe.SuivreID = Mid(data, 4)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeSuivezTous", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiGroupeSuivreCoordonnees(index As Integer, data As String)

        With Comptes(index)

            Try

                ' IC
                ' IC  -23 | 34  | 2 | 3039571 
                ' IC  Map | Map | ? | Id Unique Suivi
                ' Map = [-23,34]

                If data <> "IC" Then

                    data = Mid(data, 3)

                    Dim separate As String() = Split(data, "|")

                    If .Groupe.Membre.ContainsKey(separate(3)) Then

                        .Groupe.SuivreCoordonnees = separate(0) & "," & separate(1)

                        EcritureMessage(index, "[Groupe]", .Groupe.Membre(separate(3)).Nom & " se trouve sur la map [" & separate(0) & "," & separate(1) & "].", Color.Cyan)

                    End If

                Else

                    .Groupe.SuivreCoordonnees = ""

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGroupeSuivreCoordonnées", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Delegate Function DlgF()
    Public Function CopyGroupe(ByVal index As Integer, ByVal dico As Dictionary(Of Integer, CGroupeMembre)) As Dictionary(Of Integer, CGroupeMembre)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New DlgF(Function() CopyGroupe(index, dico)))

            Else

                Dim newDico As New Dictionary(Of Integer, CGroupeMembre)

                For Each pair As KeyValuePair(Of Integer, CGroupeMembre) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

        End With

    End Function

End Module


#Region "Class"

Public Class CGroupe

    Public EnGroupe As Boolean = False
    Public EnInvitation As Boolean = False
    Public Inviteur, Inviter As String
    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public Membre As New Dictionary(Of Integer, CGroupeMembre)
    Public SuivreID As Integer = 0
    Public SuivreCoordonnees As String = Nothing
    Public ChefID As Integer = 0

End Class

Public Class CGroupeMembre

    Public IdUnique As Integer
    Public Nom As String
    Public ClasseSexe As Integer
    Public Couleur1, Couleur2, Couleur3 As String
    Public Cac, Coiffe, Cape, Familier, Bouclier As String
    Public PdvActuel, PdvMaximum As Integer
    Public Niveau As Integer
    Public Initiative As Integer
    Public Prospection As Integer

End Class

#End Region