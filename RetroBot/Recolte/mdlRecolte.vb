Module mdlRecolte

    Private Delegate Sub dlgRecolte()

    Public Sub GiRecolteEnCours(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA0 ; 501     ; 0123456   ; 35         , 18800
                ' GA0 ; Récolte ; ID Joueur ; Cellule ID , Temps en milliseconde

                Dim separateData As String() = Split(data, ";")

                Dim idPlayer As Integer = separateData(2) ' 0123456

                Dim send As String = Mid(separateData(0), 3) ' GA0

                separateData = Split(separateData(3), ",") ' 35,18800

                If idPlayer = .Personnage.ID Then

                    .Recolte.EnRecolte = True
                    .Personnage.EnInteraction = True

                    .Personnage.InteractionCellule = separateData(0)
                    .Recolte.NumeroRecolte += 1

                    EcritureMessage(index, "[Récolte]", "Temps de récolte : " & If(separateData(1).Length = 4, Mid(separateData(1), 1, 1), Mid(separateData(1), 1, 2)) & " Seconde(s)", Color.Green)
                    EcritureMessage(index, "[Récolte]", "Récolte n° " & .Recolte.NumeroRecolte, Color.Green)

                    Wait(index, separateData(1), "GKK" & send, separateData(0))

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiRecolteEnCours", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Async Sub Wait(index As Integer, pause As Integer, envoie As String, cellule As String)

        With Comptes(index)

            Try

                If .MITM = False Then

                    Await Task.Delay(pause)

                    If .Send(envoie, {"GDF|" & cellule & ";3;0", ' Indisponible (Récolte fait)
                                      "GDF|" & cellule & ";4;0"}) Then ' Indisponible (Récolte fait) 

                        .Personnage.EnInteraction = False
                        .Recolte.EnRecolte = False

                    Else

                        RecolteEchec(index, cellule)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Wait", envoie & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub RecolteEchec(index As Integer, cellule As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New dlgRecolte(Sub() RecolteEchec(index, cellule)))

                Else

                    If .Map.Interaction.ContainsKey(cellule) Then

                        .Map.Interaction(cellule).Information = "indisponible"

                        EcritureMessage(index, "[Récolte]", "Le bot n'a pas réussie à récolter la ressource sur la cellule : " & cellule, Color.Red)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Recolte_Echec", ex.Message)

            End Try

        End With

    End Sub

    Sub GiRecolteDrop(index As Integer, data As String)

        With Comptes(index)

            Try

                'IQ 1234567   | 2
                'IQ ID Joueur | Quantité

                Dim separate As String() = Split(Mid(data, 3), "|")

                If separate(0) = .Personnage.ID Then

                    EcritureMessage(index, "[Dofus]", "Vous avez obtenue " & separate(1) & " récolte(s).", Color.Green)

                End If

                InterfaceGiRecolteDrop(index, data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiRecolteDrop", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceGiRecolteDrop(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf InterfaceGiRecolteDrop), index, data)

                Else

                    'IQ 1234567   | 2
                    'IQ ID Joueur | Quantité

                    If .Map.Interaction.ContainsKey(.Personnage.InteractionCellule) Then

                        Dim separate As String() = Split(Mid(data, 3), "|")

                        If separate(0) = .Personnage.ID Then

                            If RecolteExiste(index, data) = False Then

                                With .FrmUser.DataGridView_Recolte_Drop

                                    .Rows.Add(Comptes(index).Map.Interaction(Comptes(index).Personnage.InteractionCellule).Nom)

                                    .Rows(.Rows.Count - 1).Cells(1).Value = separate(1)

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                    Exit Sub

                                End With

                            End If

                        End If

                    End If

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Function RecolteExiste(index As Integer, data As String) As Boolean

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New DlgFPlayer(AddressOf RecolteExiste), index, data)

            Else

                    Dim separate As String() = Split(Mid(data, 3), "|")

                    For Each pair As DataGridViewRow In .FrmUser.DataGridView_Recolte_Drop.Rows

                        If pair.Cells(0).Value.ToString.ToLower = .Map.Interaction(separate(0)).Nom.ToLower Then

                            pair.Cells(1).Value = CInt(pair.Cells(1).Value) + CInt(separate(1))

                            Return True

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

End Module


#Region "Class"

Public Class CRecolte

    Public EnRecolte As Boolean
    Public NumeroRecolte As Integer

End Class

#End Region