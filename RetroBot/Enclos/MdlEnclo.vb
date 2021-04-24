Module MdlEnclo

    Public Sub GiEncloMap(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' Rp 12345    ; 0             ; 2               ; 2               ; Lenculer de bot ; i      , 5      , a      , 9srth
                ' Rp id enclo ; Prix de vente ; Nbr DD ou objet ; Nbr DD ou objet ; Guilde          ; Blason , Blason , Blason , Blason

                Dim separateData As String() = Split(Mid(data, 3), ";")

                With .Enclos

                    .ID = separateData(0)
                    .Prix = separateData(1)
                    .Guilde = separateData(4)
                    .NbrMaxDragodinde = separateData(2)
                    .NbrMaxObjet = separateData(3)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEncloMap", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module


#Region "Class"

Public Class CEnclos

    Public ID As Integer
    Public Prix As Integer
    Public Guilde As String
    Public NbrMaxDragodinde As Integer
    Public NbrMaxObjet As Integer

End Class

#End Region
