Partial Module TrajetExecution

#Region "Map"

    Private Function Map_ID(index As Integer, id As Integer) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim NewDeplacement As New FunctionMap

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() NewDeplacement.ID(bot.Index, id)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Map_Coordonnees(index As Integer, coordonnees As String) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim NewDeplacement As New FunctionMap

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() NewDeplacement.Coordonnees(bot.Index, coordonnees)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Map_Deplacement(index As Integer, direction As String) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim NewDeplacement As New FunctionMap
                Dim rand As New Random

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() NewDeplacement.Deplacement(bot.Index, direction)))

                        Task.Delay(rand.Next(500, 1500)).Wait()

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Task.Delay(1000).Wait()

                        Return False

                    End If

                Next

            Catch ex As Exception

                Task.Delay(1000).Wait()

                Return False

            End Try

            Task.Delay(1000).Wait()

            Return True

        End With

    End Function

#End Region

#Region "AMI"

    Private Function Ami_Ouvre(index As Integer, choix As String) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Ouvre(bot.Index, choix)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Ami_Supprime(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try


                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Supprime(bot.Index, pseudoNom, choix)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Ami_Ajoute(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try


                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Ajoute(bot.Index, pseudoNom, choix)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Ami_Information(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try


                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Information(bot.Index, pseudoNom, choix)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Ami_Rejoindre(index As String, Nom As String) As Boolean

        With Comptes(index)

            Try


                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi
                Dim rand As New Random

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Rejoindre(bot.Index, Nom)))

                        Task.Delay(rand.Next(750, 1750)).Wait()

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

                Task.Delay(1000).Wait()

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Ami_Avertie(index As String, choix As String) As Boolean

        With Comptes(index)

            Try


                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Avertie(bot.Index, choix)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Ami_Exist(index As String, pseudoNom As String, choix As String) As Boolean

        With Comptes(index)

            Try


                Dim newTask As New List(Of Task(Of Boolean))
                Dim newAmi As New FunctionAmi

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newAmi.Exist(bot.Index, pseudoNom, choix)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

#End Region

#Region "Pnj"

    Private Function Pnj_Parler(index As Integer, nom As String) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newPnj As New FunctionPnj

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newPnj.Parler(bot.Index, nom)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Pnj_Reponse(index As Integer, reponse As String) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newPnj As New FunctionPnj

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newPnj.Reponse(bot.Index, reponse)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

#End Region

#Region "Item"

    Private Function Item_Depose(index As Integer, idnom As String, Optional quantite As String = "99999", Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Depose(bot.Index, idnom, quantite, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Retire(index As Integer, idnom As String, Optional quantite As String = "99999", Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Retire(bot.Index, idnom, quantite, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Supprime(index As Integer, idnom As String, Optional quantite As String = "99999", Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Supprime(bot.Index, idnom, quantite, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Existe(index As Integer, idnom As String, Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Existe(bot.Index, idnom, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Equipe(index As Integer, idnom As String, Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Equipe(bot.Index, idnom, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Desequipe(index As Integer, idnom As String, Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Desequipe(bot.Index, idnom, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Jette(index As Integer, idnom As String, Optional quantiter As Integer = 99999, Optional caracteristique As String = "") As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Jette(bot.Index, idnom, quantiter, caracteristique)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function Item_Utilise(index As Integer, idnom As String) As Boolean

        With Comptes(index)

            Try

                Dim newTask As New List(Of Task(Of Boolean))
                Dim newItem As New FunctionItem

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        newTask.Add(Task.Run(Function() newItem.Utilise(bot.Index, idnom)))

                    End If

                Next

                Task.WaitAll(newTask.ToArray)

                For i = 0 To newTask.Count - 1

                    If newTask(i).Result = False Then

                        Return False

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

#End Region
End Module
