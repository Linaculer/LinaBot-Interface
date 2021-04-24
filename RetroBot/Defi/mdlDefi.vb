Module mdlDefi

    Sub GiDefiRecu(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA ; 900 ; 1234567    ; 7654321
                ' GA ; 900 ; Id Lanceur ; Id Receveur

                Dim separateData As String() = Split(data, ";")

                If separateData(2) = .Personnage.ID Then

                    .Defi.IdLanceur = separateData(2)
                    .Defi.EnInvitation = True
                    EcritureMessage(index, "[Dofus]", "Tu défies " & .Map.Entite(separateData(3)).Nom, Color.Green)

                ElseIf separateData(3) = .Personnage.ID Then

                    .Defi.IdLanceur = separateData(2)
                    .Defi.EnInvitation = True
                    EcritureMessage(index, "[Dofus]", .Map.Entite(separateData(2)).Nom & " te défie. acceptes-tu ?", Color.Green)

                End If

                EcritureMessage(index, "[Dofus]", .Map.Entite(separateData(2)).Nom & " défie " & .Map.Entite(separateData(3)).Nom, Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDefiRecu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiDefiRefuser(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA ; 902 ; 1234567    ; 7654321
                ' GA ; 902 ; Id Lanceur ; Id Receveur

                Dim separateData As String() = Split(data, ";")

                .Defi.IdLanceur = Nothing
                .Defi.EnInvitation = False

                EcritureMessage(index, "[Dofus]", "Défi refusé.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDefiRecu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiDefiAccepter(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA ; 901 ; 1234567    ; 7654321
                ' GA ; 901 ; Id Lanceur ; Id Receveur

                Dim separateData As String() = Split(data, ";")

                .Defi.EnDefi = True
                .Defi.EnInvitation = False

                EcritureMessage(index, "[Dofus]", "Défi accepté.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiDefiRecu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module

#Region "Class"

Public Class CDefi

    Public EnInvitation As Boolean
    Public EnDefi As Boolean
    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public IdLanceur As Integer

End Class

#End Region