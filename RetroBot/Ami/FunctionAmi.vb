Public Class FunctionAmi


    ''' <summary>
    ''' Ouvre la liste d'ami/ennemi.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="choix">Choisis entre "Ami" et "Ennemi"</param>
    ''' <returns>
    ''' True = La liste est ouverte. <br/>
    ''' False = La liste n'est pas ouverte.
    ''' </returns>
    Public Function Ouvre(index As String, choix As String) As Boolean

        With Comptes(index)

            Try

                Select Case choix.ToLower

                    Case "ami", "amie"

                        Return .Send("FL",
                                    {"FL"}) ' La liste d'ami est ouverte.

                    Case "ennemi", "ennemie"

                        Return .Send("iL",
                                    {"iL"}) ' La liste d'ennemi est ouverte.

                End Select

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Supprime le joueur de la liste d'ami/ennemi.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="pseudoNom">Le pseudo ou le nom de la personne.</param>
    ''' <param name="choix">Choisis entre "Ami" et "Ennemi"</param>
    ''' <returns>
    ''' True = Suppression réussie. <br/>
    ''' False = La suppression a échoué.
    ''' </returns>
    Public Function Supprime(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try

                If pseudoNom.ToLower <> .Personnage.Pseudo.ToLower AndAlso pseudoNom.ToLower <> .Personnage.NomDuPersonnage.ToLower Then

                    For Each Pair As CAmiInformation In CopyAmi(index, If(choix.ToLower = "ami", .Ami.Ami, .Ami.Ennemi)).Values

                        If Pair.Pseudo.ToLower = pseudoNom.ToLower OrElse Pair.Nom.ToLower = pseudoNom.ToLower Then

                            Select Case choix.ToLower

                                Case "ami", "amie"

                                    .Send("FD*" & Pair.Pseudo,
                                         {"FDK", ' Ami bien supprimé.
                                          "FAEf"}) ' Impossible, ce perso ou compte n'existe pas.

                                    ' Je remet à jour la liste d'ami.
                                    .Send("FL",
                                         {"FL"}) ' Reçoit la liste d'ami

                                    If .Ami.Ami.ContainsKey(Pair.Pseudo) Then Return False

                                Case "ennemi", "ennemie"

                                    .Send("iD*" & Pair.Pseudo,
                                         {"iDK", ' Ennemi bien supprimé.
                                          "iAEf"}) ' Impossible, ce perso ou compte n'existe pas.

                                    ' Je remet à jour la liste d'ennemi.
                                    .Send("iL",
                                         {"iL"}) ' Met à jour la liste d'ennemi.

                                    If .Ami.Ennemi.ContainsKey(Pair.Pseudo) Then Return False

                                Case "ignore"

                                    If .Ami.Ignore.ContainsKey(pseudoNom) Then

                                        .Ami.Ignore.Remove(pseudoNom)

                                        If .Ami.Ignore.ContainsKey(pseudoNom) Then Return False

                                    End If

                            End Select

                            Return True

                        End If

                    Next

                Else

                    Return True

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Ajoute un ami/ennemi.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="pseudoNom">Le pseudo ou le nom de la personne.</param>
    ''' <param name="choix">Choisis entre "Ami" et "Ennemi"</param>
    ''' <returns>
    ''' True = Ajout réussie. <br/>
    ''' False = Ajout échoué.
    ''' </returns>
    Public Function Ajoute(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try

                If pseudoNom <> "" Then


                    If pseudoNom.ToLower <> .Personnage.Pseudo.ToLower AndAlso pseudoNom.ToLower <> .Personnage.NomDuPersonnage.ToLower Then

                        Select Case choix.ToLower

                            Case "ami", "amie"

                                .Send("FA" & pseudoNom,
                                 {"FAEa", ' Déjà dans ta liste d'amis.
                                  "FAEf", ' Impossible, ce perso ou compte n'existe pas ou n'est pas connecté.
                                  "FAK", ' X a été ajouté à ta liste d'amis.
                                  "FL"}) ' Met à jour la liste d'amis.

                            Case "ennemi", "ennemie"

                                .Send("iA%" & pseudoNom,
                                 {"iAEa", ' Déjà dans ta liste d'ennemis.
                                  "iAEf", ' Impossible, ce perso ou compte n'existe pas ou n'est pas connecté.
                                  "iAK", ' X a été ajouté à ta liste d'ennemis.
                                  "iL"}) ' Met à jour la liste d'ennemis.

                            Case "ignore"

                                .Ami.Ignore.Add(pseudoNom, New CAmiInformation)

                        End Select

                        Return Exist(index, pseudoNom, choix)

                    Else

                        Return True

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Ami_Ajoute", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Obtient les informations du joueur.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="pseudoNom">Le pseudo ou le nom de la personne.</param>
    ''' <param name="choix">Choisis entre "Ami" et "Ennemi"</param>
    ''' <returns>
    ''' True = Information obtenue. <br/>
    ''' False = aucune information obtenue.
    ''' </returns>
    Public Function Information(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try


                If pseudoNom.ToLower <> .Personnage.Pseudo.ToLower AndAlso pseudoNom.ToLower <> .Personnage.NomDuPersonnage.ToLower Then

                    For Each Pair As CAmiInformation In CopyAmi(index, If(choix.ToLower = "ami", .Ami.Ami, .Ami.Ennemi)).Values

                        If Pair.Pseudo.ToLower = pseudoNom.ToLower OrElse Pair.Nom.ToLower = pseudoNom.ToLower Then

                            .Send("BW" & Pair.Nom,
                                 {"BWK", ' Information reçu.
                                  "BWE"}) ' n'est pas connecté ou n'existe pas.

                            If .Ami.Information.Pseudo <> "" Then

                                Return True

                            Else

                                Return False

                            End If

                        End If

                    Next

                Else

                    Return True

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "AmiInformation", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Rejoint un joueur directement sur la map (Incarnam)
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nom">Nom du joueur.</param>
    ''' <returns>
    ''' True = Le bot a rejoint le joueur. <br/>
    ''' False = Le bot n'a pas réussie à rejoindre le joueur.
    ''' </returns>
    Public Function Rejoindre(index As String, nom As String) As Boolean

        With Comptes(index)

            Try


                If nom.ToLower <> .Personnage.NomDuPersonnage.ToLower Then

                    Return .Send("FJF" & nom,
                                {"GDM"})

                Else

                    Return True

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "AmiRejoindre", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Averti lors de la connexion d'un ami.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="choix"></param>
    ''' <returns>
    ''' True = Le bot avertie via un message si un ami se connecte.. <br/>
    ''' False = Le bot n'avertie pas si un ami se connecte.
    ''' </returns>
    Public Function Avertie(index As String, choix As String) As Boolean

        With Comptes(index)

            Try

                Select Case choix.ToLower

                    Case "oui", "true"

                        Return .Send("FO+",
                                    {"BN"}) ' Info bien reçu par le serveur.

                    Case "non", "false"

                        Return .Send("FO-",
                                    {"BN"}) ' Info bien reçu par le serveur.

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "AmiAvertie", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Vérifie si le joueur se trouve dans la liste demandé.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="PseudoNom">Le nom ou le pseudo du joueur à vérifier.</param>
    ''' <returns>
    ''' True = Le joueur se trouve dans la liste.<br/>
    ''' False = Le joueur n'existe pas dans la liste..
    ''' </returns>
    Public Function Exist(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try

                If pseudoNom.ToLower <> .Personnage.Pseudo.ToLower AndAlso pseudoNom.ToLower <> .Personnage.NomDuPersonnage.ToLower Then

                    Select Case choix.ToLower

                        Case "ami", "amie"

                            For Each ami As KeyValuePair(Of String, CAmiInformation) In .Ami.Ami

                                If ami.Value.Pseudo.ToLower = pseudoNom.ToLower OrElse ami.Value.Nom.ToLower = pseudoNom.ToLower Then

                                    Return True

                                End If

                            Next

                        Case "ennemi", "ennemie"

                            For Each ennemi As KeyValuePair(Of String, CAmiInformation) In .Ami.Ennemi

                                If ennemi.Value.Pseudo.ToLower = pseudoNom.ToLower OrElse ennemi.Value.Nom.ToLower = pseudoNom.ToLower Then

                                    Return True

                                End If

                            Next

                        Case "ignore"

                            For Each ignore As KeyValuePair(Of String, CAmiInformation) In .Ami.Ignore

                                If ignore.Value.Pseudo.ToLower = pseudoNom.ToLower OrElse ignore.Value.Nom.ToLower = pseudoNom.ToLower Then

                                    Return True

                                End If

                            Next

                    End Select

                Else

                    Return True

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "AmiAvertie", ex.Message)

            End Try

            Return False

        End With

    End Function

End Class
