Public Class LinaBot

    Public Compteur As Integer
    Public PathMITM As String

    Private Sub LinaBot_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Try

            Try

                Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Data/PathMITM.txt")

                Do Until swLecture.EndOfStream

                    Dim ligne As String = swLecture.ReadLine

                    If ligne <> "" Then

                        PathMITM = ligne

                    End If

                Loop

                swLecture.Close()

            Catch ex As Exception

            End Try

            ChargeServeur()

            ChargeItems()

            ChargeSort()

            ChargeQuête()

            ChargeMap()

            ChargeRecolte()

            ChargeDivers()

            ChargeMobs()

            ChargePnj()

            ChargePnjRéponse()

            ChargeMaison()

            ChargeMetier()

            ChargeFamilier()

            ChargeCaractéristique()

            LoadPersonage()

        Catch ex As Exception

        End Try

    End Sub

    Private Sub Charger_Un_Compte_Click(sender As Object, e As EventArgs) Handles Charger_Un_Compte.Click

        GestionComptes.Show()

    End Sub

    Private Sub MITMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles MITMToolStripMenuItem.Click

        PathMITM = InputBox("Veuillez indiquer le chemin vers l'executable de dofus, exemple : ""D:\DOFUS RETRO\""", "Path", "")

        'J'ouvre le fichier pour y écrire se que je souhaite
        Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\Data/PathMITM.txt")

        swEcriture.Write(PathMITM)

        'Puis je le ferme.
        swEcriture.Close()

    End Sub

End Class
