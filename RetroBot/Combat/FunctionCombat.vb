Class FunctionCombat

    Public Function Cadenas(ByVal index As Integer, ByVal active As Boolean) As Boolean

        With Comptes(index)

            Try

                If .Combat.Cadenas <> active Then

                    .Combat.Bloque.Reset()

                    .Send("fN")

                    .Combat.Bloque.WaitOne(15000)

                End If

                Return If(.Combat.Cadenas = active, True, False)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Cadenas", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Aide(ByVal index As Integer, ByVal active As Boolean) As Boolean

        With Comptes(index)

            Try

                If .Combat.Aide <> active Then

                    .Combat.Bloque.Reset()

                    .Send("fH")

                    .Combat.Bloque.WaitOne(15000)

                End If

                Return If(.Combat.Aide = active, True, False)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Aide", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Spectateur(ByVal index As Integer, ByVal active As Boolean) As Boolean

        With Comptes(index)

            Try

                If .Combat.Spectateur <> active Then

                    .Combat.Bloque.Reset()

                    .Send("fS")

                    .Combat.Bloque.WaitOne(15000)

                End If

                Return If(.Combat.Spectateur = active, True, False)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Spectateur", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Groupe(ByVal index As Integer, ByVal active As Boolean) As Boolean

        With Comptes(index)

            Try

                If .Combat.Groupe <> active Then

                    Return .Send("fP",
                                 {"Im093", "Im094"})

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Groupe", ex.Message)

            End Try

            Return False

        End With

    End Function


    Public Function Pret(ByVal index As Integer, ByVal choix As Boolean) As Boolean

        With Comptes(index)

            Try

                Task.Delay(500).Wait()

                Select Case CBool(choix)

                    Case True

                        If .Combat.Entite(.Personnage.ID).Pret = False Then

                            .Combat.Bloque.Reset()

                            .Send("GR1")

                            .Combat.Bloque.WaitOne(15000)

                        End If

                    Case False

                End Select

                Return If(.Combat.Entite(.Personnage.ID).Pret = choix, True, False)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Pret", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Passe(index As Integer) As Boolean

        With Comptes(index)

            Try

                If .Combat.EnCombat AndAlso .Combat.MonTour Then

                    .Send("Gt")
                    .Send("GT")

                End If

            Catch ex As Exception

            End Try

            Return True

        End With

    End Function

    Public Function Rejoindre(index As Integer, id As Integer) As Boolean

        With Comptes(index)

            Try

                Return .Send("GA903" & id & ";" & id,
                             {"GJK2", "GP", "GA;903;"})

            Catch ex As Exception

            End Try

            Return True

        End With

    End Function

    Public Function Placement(index As Integer, cellule As Integer) As Boolean

        With Comptes(index)

            Try

                Return .Send("Gp" & cellule,
                             {"GIC|" & .Personnage.ID})

            Catch ex As Exception

            End Try

            Return True

        End With

    End Function

End Class
