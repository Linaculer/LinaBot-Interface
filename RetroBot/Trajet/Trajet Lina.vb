Module Trajet_Lina


    Public Sub TrajetLoad(index As Integer, chemin As String)

        With Comptes(index)

            .Trajet.Clear()
            .FrmGroupe.Pods = 80
            .FrmGroupe.Regeneration = 80
            .FrmGroupe.Recolte.Clear()
            .Familier.Clear()
            .FrmGroupe.Supprime.Clear()

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

                Dim separateLecture As String() = Split(ligneActuel, " ")

                Select Case separateLecture(0).ToLower

                    Case "sub", "function"

                        'Début balise
                        Balise = separateLecture(1)

                    Case "end"

                        Select Case separateLecture(1).ToLower

                            Case "sub", "function"

                                Balise = ""

                            Case Else

                                If Balise <> "" AndAlso ligneActuel <> "" AndAlso Not ligneActuel.StartsWith("'") Then

                                    If Not .Trajet.ContainsKey(Balise.ToLower) Then

                                        .Trajet.Add(Balise.ToLower, New List(Of String) From {ligneActuel.Replace(vbTab, "")})

                                    Else

                                        .Trajet(Balise.ToLower).Add(ligneActuel.Replace(vbTab, ""))

                                    End If

                                End If

                        End Select

                    Case "dim"

                        Select Case separateLecture(1).ToLower

                            Case "recolte"

                                separateLecture = Split(ligneActuel, " = ")
                                separateLecture = Split(separateLecture(1).Replace("""", ""), " , ")

                                For i = 0 To separateLecture.Count - 1

                                    .FrmGroupe.Recolte.Add(separateLecture(i).ToLower)

                                Next

                            Case "pods"

                                .FrmGroupe.Pods = separateLecture(3)

                            Case "regeneration"

                                .FrmGroupe.Regeneration = separateLecture(3)

                            Case "familier"

                                separateLecture = Split(ligneActuel, " = ")
                                separateLecture = Split(separateLecture(1).Replace("""", ""), " , ")

                                For i = 0 To separateLecture.Count - 1

                                    Dim separate As String() = Split(separateLecture(i), " : ")

                                    .Familier.Add(separate(0).ToLower, separate(1).ToLower)

                                Next

                            Case "supprime"

                                separateLecture = Split(separateLecture(1).Replace("""", ""), " , ")

                                For i = 0 To separateLecture.Count - 1

                                    .FrmGroupe.Supprime.Add(separateLecture(i).ToLower)

                                Next

                            Case Else

                        End Select

                    Case Else

                        If Balise <> "" AndAlso ligneActuel <> "" AndAlso Not ligneActuel.StartsWith("'") Then

                            If Not .Trajet.ContainsKey(Balise.ToLower) Then

                                .Trajet.Add(Balise.ToLower, New List(Of String) From {ligneActuel.Replace(vbTab, "")})

                            Else

                                .Trajet(Balise.ToLower).Add(ligneActuel.Replace(vbTab, ""))

                            End If

                        End If

                End Select

            Loop

            swLecture.Close()

            TrajetLecture(index, .Trajet.Keys(0))

        End With

    End Sub




End Module
