Public Class FunctionDragodinde

    Public Function Monte(ByVal index As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("Rr")

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

    Public Function Descend(ByVal index As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("Rr")

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

    Public Function XpDonnee(ByVal index As String, ByVal valeur As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("Rx" & valeur)

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

    Public Function ChangeNom(ByVal index As String, ByVal nom As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("Rn" & nom)

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

    Public Function Libere(ByVal index As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("Rf")

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

    Public Function Castre(ByVal index As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("Rc")

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

    Public Function Inventaire(ByVal index As String) As Boolean

        With Comptes(index)

            .Dragodinde.Bloque.Reset()

            .Send("ER15|")

            Return .Dragodinde.Bloque.WaitOne(15000)

        End With

    End Function

End Class
