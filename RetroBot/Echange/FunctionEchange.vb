Public Class FunctionEchange

    ''' <summary>
    ''' Lance un echange avec le joueur.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nom">Le nom du joueur à qui lancer la demande d'échange.</param>
    ''' <returns>
    ''' True = Si le bot a réussie à faire la demande d'échange. <br/>
    ''' False = Si le bot n'a pas réussie à faire la demande d'échange.
    ''' </returns>
    Public Function Invite(index As String, nom As String) As Boolean

        With Comptes(index)

            Try

                If .Echange.EnInvitation = False AndAlso .Echange.EnEchange = False Then

                    For Each pair As KeyValuePair(Of Integer, CEntite) In .Map.Entite

                        If pair.Value.Nom.ToLower = nom.ToLower Then

                            .Send("ER1|" & pair.Key,
                                 {"ERK", ' Demande d'échange en cours...
                                  "EREO"}) ' Ce joueur est déjà en échange.

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionEchange_Invite", ex.Message)

            End Try

            Return .Echange.EnInvitation

        End With

    End Function


    ''' <summary>
    ''' Refuse la demande d'échange en cours.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Si le bot a réussie à refuser la demande d'échange. <br/>
    ''' False = Si le bot n'a pas réussie à refuser la demande d'échange.
    ''' </returns>
    Public Function Refuse(index As String) As Boolean

        With Comptes(index)

            Try

                If .Echange.EnInvitation Then

                    Return .Send("EV",
                                {"EV"}) ' Echange Annulé/Refusé/Arrêté

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "EchangeRefuseArrete", ex.Message)

            End Try

            Return Not .Echange.EnInvitation

        End With

    End Function


    ''' <summary>
    ''' Arrête la demande d'échange en cours.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Si le bot a réussie à arrêter l'échange en cours. <br/>
    ''' False = Si le bot n'a pas réussie à arrêter l'échange en cours.
    ''' </returns>
    Public Function Arrete(index As String) As Boolean

        With Comptes(index)

            Try

                If .Echange.EnEchange Then

                    Return .Send("EV",
                                {"EV"}) ' Echange Annulé/Refusé/Arrêté

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "EchangeRefuseArrete", ex.Message)

            End Try

            Return Not .Echange.EnEchange

        End With

    End Function


    ''' <summary>
    ''' Accepte la demande d'échange.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Si le bot a accepté la demande d'échange. <br/>
    ''' False = Si le bot n'a pas réussie à accepter la demande d'échange.
    ''' </returns>
    Public Function Accepte(index As String) As Boolean

        With Comptes(index)

            Try

                If .Echange.EnInvitation Then

                    .Send("EA", {
                          "ECK1", ' Echange accepté.
                          "EV"}) ' Echange annulé.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "EchangeAccepte", ex.Message)

            End Try

            Return .Echange.EnEchange

        End With

    End Function


    ''' <summary>
    ''' Mais la quantité de kamas dans l'échange.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="quantite">La quantité de kamas à mettre dans l'échange.</param>
    ''' <returns>
    ''' True = Si le bot a réussie à mettre les kamas dans l'échange. <br/>
    ''' False = Si le bot n'a pas réussie à mettre les kamas dans l'échange.
    ''' </returns>
    Public Function Kamas(index As String, quantite As String) As Boolean

        With Comptes(index)

            Try

                If quantite > .Personnage.Kamas Then quantite = .Personnage.Kamas

                Return .Send("EMG" & quantite,
                            {"EMKG", ' Nombre de kamas mis dans l'échange.
                             "EsKG"}) ' Nombre de kamas mis dans la Banque/Coffre.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "EchangeKamas", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Valide l'échange en cours.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Si le bot a validé l'échange. <br/>
    ''' False = Si le bot n'a pas réussie à valider l'échange.
    ''' </returns>
    Public Function Valide(index As String) As Boolean

        With Comptes(index)

            Try

                If .Echange.EnEchange Then

                    .Send("EK",
                         {"EK1", ' J'ai validé l'échange.
                          "EK0"}) ' J'ai invalidé l'échange.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Echange_Valide", ex.Message)

            End Try

            Return .Echange.Moi.Valider

        End With

    End Function


End Class
