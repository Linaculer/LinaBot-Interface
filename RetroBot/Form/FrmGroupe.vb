Public Class FrmGroupe

    Public Pods As Integer
    Public Regeneration As Integer
    Public Recolte As New List(Of String)
    Public Supprime As New List(Of String)
    Public BotIndex As New List(Of Integer)

    Private Sub FrmGroupe_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Task.Run(AddressOf Groupe_Invitation)
        Task.Run(AddressOf CombatRejoint)

    End Sub

    Private Sub Groupe_Invitation()

        Try

            While True

                Task.Delay(1000).Wait()

                Try

                    Dim newGroupe As New FunctionGroupe

                    If Comptes(BotIndex(0)).Connecté Then

                        For i = 1 To BotIndex.Count - 1

                            If Comptes(BotIndex(i)).Groupe.EnGroupe = False AndAlso Comptes(BotIndex(i)).Groupe.EnInvitation = False AndAlso Comptes(BotIndex(i)).Connecté Then

                                Task.Delay(500).Wait()

                                If newGroupe.Invite(BotIndex(0), Comptes(BotIndex(i)).Personnage.NomDuPersonnage) Then

                                    Task.Delay(500).Wait()

                                    newGroupe.Accepte(BotIndex(i))

                                End If

                            End If

                        Next

                    End If

                Catch ex As Exception

                End Try

            End While

        Catch ex As Exception

        End Try

    End Sub

    Private Sub CombatRejoint()

        Try

            While True

                Task.Delay(1000).Wait()

                Try

                    For i = 0 To BotIndex.Count - 1

                        With Comptes(BotIndex(i))

                            For Each pairCombat As KeyValuePair(Of Integer, List(Of CCombatLancer)) In .Combat.Lancer

                                For Each pairCombatLancer As CCombatLancer In pairCombat.Value

                                    For a = 0 To BotIndex.Count - 1

                                        If Comptes(BotIndex(a)).Personnage.ID <> .Personnage.ID Then

                                            If Comptes(BotIndex(a)).Personnage.ID = pairCombatLancer.idUnique AndAlso
                                                                                   .Combat.EnCombat = False AndAlso
                                                                                   .Combat.EnPlacement = False AndAlso
                                                                                   .Combat.EnPreparation = False Then

                                                Dim newCombat As New FunctionCombat

                                                newCombat.Rejoindre(.Index, pairCombat.Key)

                                                Task.Delay(750).Wait()

                                            End If

                                        End If

                                    Next

                                Next

                            Next

                        End With

                    Next

                Catch ex As Exception

                End Try

            End While

        Catch ex As Exception

        End Try

    End Sub


End Class