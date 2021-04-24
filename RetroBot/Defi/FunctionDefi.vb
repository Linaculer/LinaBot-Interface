Public Class FunctionDefi

    Public Function Annule(index As String) As Boolean

        With Comptes(index)

            Try

                If .Defi.EnInvitation Then

                    .Send("GQ",
                         {"GA;902", ' Défi refusé.
                          "GA;901"}) ' Defi accepté

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Defi_Annule", ex.Message)

            End Try

            Return Not .Defi.EnInvitation

        End With

    End Function


    Public Function Abandonne(index As String) As Boolean

        With Comptes(index)

            Try

                If .Defi.EnDefi Then

                    .Send("GQ",
                         {"GA;902", ' Défi refusé.
                          "GA;901"}) ' Defi accepté

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Defi_Abandonne", ex.Message)

            End Try

            Return Not .Defi.EnDefi

        End With

    End Function


    Public Function Refuse(index As String) As Boolean

        With Comptes(index)

            Try

                If .Defi.EnInvitation Then

                    .Send("GA902" & .Defi.IdLanceur,
                         {"GA;902", ' Défi refusé.
                          "GA;901"}) ' Defi accepté

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Defi_Refuse", ex.Message)

            End Try

            Return Not .Defi.EnInvitation

        End With

    End Function


    Public Function Accepte(index As String) As Boolean

        With Comptes(index)

            Try

                If .Defi.EnInvitation Then

                    .Send("GA901" & .Defi.IdLanceur,
                         {"GA;900"}) ' Defi reçu

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Defi_Accepte", ex.Message)

            End Try

            Return .Defi.EnDefi

        End With

    End Function


    Public Function Invite(index As String, nom As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As CEntite In .Map.Entite.Values

                    If pair.Nom.ToLower = nom.ToLower Then

                        .Send("GA900" & pair.IDUnique,
                             {"GA;900"}) ' Défi reçu.

                        Exit For

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Defi_Invite", ex.Message)

            End Try

            Return .Defi.EnInvitation

        End With

    End Function

End Class
