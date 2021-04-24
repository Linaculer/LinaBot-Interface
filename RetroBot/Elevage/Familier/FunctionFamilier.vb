Public Class FunctionFamilier

    Public Sub FamilierRetire(index As Integer, idNomFamilier As String, Optional caracteristique As String = Nothing)

        With Comptes(index)

            Dim item As New FunctionItem

            For Each banque As CItem In CopyItem(index, .Echange.Moi.Inventaire).Values

                If VarFamilier.ContainsKey(banque.IdObjet) AndAlso .Familier.ContainsKey(banque.Nom.ToLower) AndAlso idNomFamilier.ToLower = banque.Nom.ToLower Then

                    If banque.Caracteristique.FamilierDateProchainRepas <= Date.Now Then

                        If item.Retire(index, banque.IdUnique, 1, caracteristique) Then

                            Task.Delay(500).Wait()

                            'Je retire la bouffe
                            RetireNourriture(index, banque.IdObjet, banque.Nom, banque.Caracteristique.FamilierRepas)

                            Task.Delay(500).Wait()

                        End If

                    End If

                End If

            Next

        End With

    End Sub

    Private Sub RetireNourriture(index As Integer, idFamilier As Integer, nomFamilier As String, QuantiterTotal As Integer)

        With Comptes(index)

            Dim item As New FunctionItem

            For Each Pair As CItem In CopyItem(index, .Echange.Moi.Inventaire).Values

                If VarFamilier(idFamilier).Caracteristique(.Familier(nomFamilier.ToLower)).Contains(Pair.IdObjet.ToString) Then

                    If Pair.Caracteristique.FamilierDateProchainRepas <= Date.Now Then

                        Dim quantiter As Integer = Math.Abs(QuantiterTotal)

                        If quantiter = 0 Then quantiter = 2

                        If Pair.Quantiter < quantiter Then

                            quantiter = Pair.Quantiter

                        End If

                        QuantiterTotal -= quantiter

                        item.Retire(index, Pair.IdObjet, quantiter)

                        If QuantiterTotal <= 0 Then Exit Sub

                    End If

                End If

            Next

        End With

    End Sub

    Public Function Nourrit(index As Integer) As Boolean

        With Comptes(index)

            Try

                Dim item As New FunctionItem

                DesequipeItem(index)

                Task.Delay(500).Wait()

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Categorie = 18 Then

                        If Pair.Caracteristique.FamilierDateProchainRepas <= Date.Now Then

                            Dim StatsActuelle As Integer = RecupStats(index, Pair.IdUnique, Pair.Nom)

                            item.Equipe(index, Pair.IdUnique)

                            Task.Delay(500).Wait()

                            NourritBouffe(index, StatsActuelle, Pair.IdUnique, Pair.IdObjet, Pair.Nom)

                            Task.Delay(500).Wait()

                        End If

                    End If

                Next

            Catch ex As Exception

                Return False

            End Try

            Return True

        End With

    End Function

    Private Function RecupStats(index As Integer, idunique As Integer, nom As String) As Integer

        With Comptes(index)

            Try

                Dim MonFamilier As Dictionary(Of Integer, CItem) = CopyItem(index, .Inventaire)

                Select Case .Familier(nom.ToLower)

                    Case "pods"

                        Return MonFamilier(idunique).Caracteristique.Pods

                    Case "intelligence"

                        Return MonFamilier(idunique).Caracteristique.Intelligence

                    Case "sagesse"

                        Return MonFamilier(idunique).Caracteristique.Sagesse

                    Case "chance"

                        Return MonFamilier(idunique).Caracteristique.Chance

                    Case "force"

                        Return MonFamilier(idunique).Caracteristique.Force

                    Case "agiliter"

                        Return MonFamilier(idunique).Caracteristique.Agilité

                    Case "vitaliter"

                        Return MonFamilier(idunique).Caracteristique.Vitalité

                    Case "prospection"

                        Return MonFamilier(idunique).Caracteristique.Prospection

                    Case "pc resistance neutre"

                        Return MonFamilier(idunique).Caracteristique.PcResistanceNeutre

                    Case "pc resistance feu"

                        Return MonFamilier(idunique).Caracteristique.PcResistanceFeu

                    Case "pc resistance eau"

                        Return MonFamilier(idunique).Caracteristique.PcResistanceEau

                    Case "pc resistance terre"

                        Return MonFamilier(idunique).Caracteristique.PcResistanceTerre

                    Case "pc resistance air"

                        Return MonFamilier(idunique).Caracteristique.PcResistanceAir

                    Case "dommage"

                        Return MonFamilier(idunique).Caracteristique.Dommage

                    Case "pc dommage"

                        Return MonFamilier(idunique).Caracteristique.PcDommage

                    Case "soin"

                        Return MonFamilier(idunique).Caracteristique.Soin

                    Case "initiative"

                        Return MonFamilier(idunique).Caracteristique.Initiative

                End Select

            Catch ex As Exception

            End Try

            Return 0

        End With

    End Function

    Private Sub DesequipeItem(index As Integer)

        With Comptes(index)

            Dim item As New FunctionItem

            For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                If Pair.Equipement <> "" AndAlso Pair.Equipement = "8" Then

                    item.Desequipe(index, Pair.IdObjet)

                    Exit Sub

                End If

            Next

        End With

    End Sub

    Private Sub NourritBouffe(index As Integer, stats As Integer, idunique As Integer, IdFamilier As Integer, nomFamilier As String)

        With Comptes(index)

            Dim item As New FunctionItem

            For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                If VarFamilier(IdFamilier).Caracteristique(.Familier(nomFamilier.ToLower)).Contains(Pair.IdObjet) Then

                    item.Equipe(index, Pair.IdObjet)

                    Task.Delay(500).Wait()

                    If stats < RecupStats(index, idunique, nomFamilier) Then

                        item.Equipe(index, Pair.IdObjet)

                        Task.Delay(500).Wait()

                    End If

                    DesequipeItem(index)

                    Task.Delay(500).Wait()

                End If

            Next

        End With

    End Sub

    Public Function reconnexion(index As Integer) As Integer

        With Comptes(index)

            Dim reco As Integer = 262800000

            For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                If Pair.Categorie = 18 Then

                    If (Pair.Caracteristique.FamilierCapaciteAccrue = True AndAlso RecupStats(index, Pair.IdUnique, Pair.Nom) < VarFamilier(Pair.IdObjet).CapacitéMax) OrElse
                       (Pair.Caracteristique.FamilierCapaciteAccrue = False AndAlso RecupStats(index, Pair.IdUnique, Pair.Nom) < VarFamilier(Pair.IdObjet).CapacitéNormal) Then

                        Dim resultat As Integer = Math.Abs(DateDiff(DateInterval.Second, Date.Now, Pair.Caracteristique.FamilierDateProchainRepas) * 1000)

                        If resultat < reco Then

                            reco = resultat

                        End If

                    End If

                End If

            Next

            For Each Pair As CItem In CopyItem(index, .Echange.Moi.Inventaire).Values

                If Pair.Categorie = 18 Then

                    If (Pair.Caracteristique.FamilierCapaciteAccrue = True AndAlso RecupStats(index, Pair.IdUnique, Pair.Nom) < VarFamilier(Pair.IdObjet).CapacitéMax) OrElse
                       (Pair.Caracteristique.FamilierCapaciteAccrue = False AndAlso RecupStats(index, Pair.IdUnique, Pair.Nom) < VarFamilier(Pair.IdObjet).CapacitéNormal) Then

                        Dim resultat As Integer = Math.Abs(DateDiff(DateInterval.Second, Date.Now, Pair.Caracteristique.FamilierDateProchainRepas) * 1000)

                        If resultat < reco Then

                            reco = resultat

                        End If

                    End If

                End If

            Next

            .Socket.Connexion_Game(False)

            EcritureMessage(index, "(Bot)", "Reconnexion à : " & DateAdd("s", reco / 1000, Date.Now), Color.Red)

            Return reco

        End With

    End Function

End Class
