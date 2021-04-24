Public Class FunctionGroupe

    Public Function Invite(index As String, nom As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnInvitation = False Then

                    If .Groupe.Membre.Count < 8 Then

                        Return .Send("PI" & nom,
                                    {"PIK"},   ' La personne à reçu l'invitation
                                    {"PIEa",  ' La personne et déjà dans un groupe.
                                     "PIEn"})  ' Le joueur n'est pas connecté ou existe pas.

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Invite", ex.Message)

            End Try

            Return .Groupe.EnInvitation

        End With

    End Function

    Public Function RefuseArrete(index As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnInvitation Then

                    .Send("PR",
                         {"PR"}) ' Fin invitation.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "RefuseArrete", ex.Message)

            End Try

            Return .Groupe.EnInvitation

        End With

    End Function

    Public Function Accepte(index As Integer) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnInvitation Then

                    .Send("PA",
                         {"PCK", ' PCK = J'ai rejoint le groupe.
                          "PR"}) ' PR = Il arrête l'invitation en groupe, je peux plus rejoindre.


                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Accepte", ex.Message)

            End Try

            Return .Groupe.EnGroupe

        End With

    End Function

    Public Function Quitte(index As String)

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    .Send("PV",
                         {"PV"}) ' J'ai quitté le groupe.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Quitte", ex.Message)

            End Try

            Return Not .Groupe.EnGroupe

        End With

    End Function

    Public Function SuivezMoiTous(index As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    .Send("PG+" & .Personnage.ID,
                         {"PFK", 'La personne à suivre.
                          "Im052"}) ' Nom du joueur qui me suit.

                    If .Groupe.SuivreID > 0 Then

                        Return True

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "SuivezMoiTous", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ArretezTousDeMeSuivre(index As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    .Send("PG-" & .Personnage.ID,
                         {"PFK",' ID de la Personne à suivre.
                          "Im053",' Nom de la personne à ne plus suivre.
                          "IC"}) ' Plus aucune coordonnées à suivre.

                    If .Groupe.SuivreID = 0 Then

                        Return True

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "ArretezTousDeMeSuivre", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function SuivreLeDeplacement(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    For Each pair As CGroupeMembre In CopyGroupe(index, .Groupe.Membre).Values

                        If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdUnique = nomID Then

                            .Send("PF+" & pair.IdUnique,
                                 {"IC", ' Donne la map où se trouve la personne à suivre.
                                  "PFK"}) ' ID de la personne à suivre.

                            Exit For

                        End If

                    Next

                    If .Groupe.SuivreCoordonnees <> "" Then

                        Return True

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "SuivreLeDeplacement", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function NePlusSuivreLeDeplacement(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    For Each pair As CGroupeMembre In CopyGroupe(index, .Groupe.Membre).Values

                        If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdUnique = nomID Then

                            .Send("PF-" & pair.IdUnique,
                                 {"IC", ' Position de la personne à suivre.
                                 "PFK"}) 'ID de la personne à ne plus suivre.

                            If .Groupe.SuivreCoordonnees = "" Then

                                Return True

                            End If

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "NePlusSuivreLeDeplacement", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function SuivezLeTous(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    For Each pair As CGroupeMembre In CopyGroupe(index, .Groupe.Membre).Values

                        If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdUnique = nomID Then

                            .Send("PG+" & pair.IdUnique,
                                 {"IC", ' La personne à suivre.
                                  "PFK"}) ' Id de la personne à suivre.

                            If .Groupe.SuivreCoordonnees <> "" Then

                                Return True

                            End If

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "SuivezLeTous", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ArretezTousDeLeSuivre(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    For Each pair As CGroupeMembre In CopyGroupe(index, .Groupe.Membre).Values

                        If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdUnique = nomID Then

                            .Send("PG-" & pair.IdUnique,
                                 {"IC", ' La personne à suivre.
                                  "PFK"}) ' Plus personne à suivre.

                            If .Groupe.SuivreCoordonnees = "" Then

                                Return True

                            End If

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "ArretezTousDeLeSuivre", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Exclure(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Groupe.EnGroupe Then

                    For Each pair As CGroupeMembre In CopyGroupe(index, .Groupe.Membre).Values

                        If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdUnique = nomID Then

                            .Send("PV" & pair.IdUnique,
                                 {"PM-", ' Supprime le membre du groupe.
                                  "PV"}) ' Joueur exclut, ou quitte le groupe.

                            If .Groupe.Membre.ContainsKey(pair.IdUnique) Then

                                Return True

                            End If

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Exclure", ex.Message)

            End Try

            Return False

        End With

    End Function







End Class
