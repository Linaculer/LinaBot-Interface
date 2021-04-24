Public Module FunctionConnexion

    Private Delegate Function dlg()

    Public Function Connexion(index As String, nomDeCompte As String, motDePasse As String, serveur As String, nomDuPersonnage As String)

        With Comptes(index)

            Try

                If .Connecté = False AndAlso .EnConnexion = False AndAlso .EnAuthentification = False Then

                    With .Personnage

                        .NomDeCompte = nomDeCompte
                        .MotDePasse = motDePasse
                        .Serveur = serveur
                        .NomDuPersonnage = nomDuPersonnage

                    End With

                    ChargeOptionBot(index, Application.StartupPath + "\Compte\Options\" & .Personnage.NomDeCompte & "_" & .Personnage.NomDuPersonnage & ".txt")

                    .CreateSocketAuthentification(VarServeur("Authentification").IP, VarServeur("Authentification").Port)

                    While .Connecté = False

                        Task.Delay(3000).Wait()

                    End While

                End If

            Catch ex As Exception

                Return False

            End Try

            Task.Delay(10000).Wait()

            Return .Connecté

        End With

    End Function

End Module
