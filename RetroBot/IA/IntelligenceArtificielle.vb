Public Module IntelligenceArtificielle

    Public Sub IALoad(index As Integer, chemin As String)

        With Comptes(index)

            Try

                .IntelligenceArtificielle.Clear()

            Dim swLecture As IO.StreamReader = New IO.StreamReader(chemin, System.Text.Encoding.UTF7)

            Dim Balise As String = ""

            Do Until swLecture.EndOfStream

                Dim ligneActuel As String = AsciiDecoder(swLecture.ReadLine)

                ligneActuel = ligneActuel.Replace(vbTab, "")

                While ligneActuel.Contains("  ")
                    ligneActuel = ligneActuel.Replace("  ", " ")
                End While

                While ligneActuel.StartsWith(" ")
                    ligneActuel = Mid(ligneActuel, 2, ligneActuel.Length)
                End While

                .IntelligenceArtificielle.Add(ligneActuel)

            Loop

            swLecture.Close()

            Catch ex As Exception

            End Try

        End With

    End Sub

    Public Sub IAGestion(index As Integer)

        With Comptes(index)

            Try

                For i = 0 To .IntelligenceArtificielle.Count - 1

                    Dim separateIA As String() = Split(.IntelligenceArtificielle(i), " : ")

                    For a = 0 To separateIA.Count - 1

                        Dim separateLigne As String() = Split(separateIA(a).Replace(vbTab, ""), "(")

                        Dim separate As String() = Split(separateLigne(0), ".")

                        Dim Parametre As Object = ReturnParametre(index, separateIA(a))

                        Select Case separate(0).ToLower

                            Case "avancer"

                                Avancer(index)

                            Case "lancer un sort"

                                LancerUnSort(index, Parametre(1))

                            Case "passe tour"

                                passetour(index)

                        End Select

                    Next

                Next

            Catch ex As Exception

            End Try

        End With

    End Sub

End Module
