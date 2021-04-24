Module Module_Zaap

    Public Sub GiZaapInformation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' WC 1242         | 3250  ; 450  | Next
                ' WC Mapid actuel | Mapid ; Prix | Next

                .Personnage.EnInteraction = True

                .ZaapI.Clear()

                Dim separateData As String() = Split(Mid(data, 3), "|")

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";") '3250;450

                    If Not .ZaapI.ContainsKey(separate(0)) Then

                        .ZaapI.Add(separate(0), separate(1))

                    Else

                        .ZaapI(separate(0)) = separate(1)

                    End If

                Next

                EcritureMessage(index, "(Dofus)", "Utilisation du Zaap en cours.", Color.Lime)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiZaapInformation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiZaapFin(index As Integer, data As String)

        With Comptes(index)

            Try

                ' WV

                .Personnage.EnInteraction = False

                .ZaapI.Clear()

                EcritureMessage(index, "(Dofus)", "Fin d'utilisation du Zaap.", Color.Lime)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiZaapFin", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module
