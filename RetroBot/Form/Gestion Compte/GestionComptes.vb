Public Class GestionComptes

    Private Sub GestionComptes_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ComboBoxChoixServeur.Items.Clear()

        For Each pair As KeyValuePair(Of String, ClassServeur) In VarServeur

            ComboBoxChoixServeur.Items.Add(pair.Key)

        Next

        ComboBoxChoixServeur.SelectedIndex = 0

    End Sub

#Region "Ajouter"

    Private Sub TextBox_Nom_De_Compte_TextChanged(sender As Object, e As EventArgs) Handles TextBoxNomDeCompte.Click, TextBoxMotDePasse.Click, TextBoxNomPersonnage.Click

        sender.Text = ""

    End Sub

    Private Sub ButtonAjouter_Click(sender As Object, e As EventArgs) Handles ButtonAjouter.Click

        Try

            'Je lis le fichier.
            Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Compte/Comptes.txt")

            Dim ligneFinal As String = ""

            Do Until swLecture.EndOfStream

                Dim Ligne As String = swLecture.ReadLine

                If Ligne <> "" Then

                    ligneFinal &= Ligne & vbCrLf

                End If

            Loop

            'Puis je ferme le fichier.
            swLecture.Close()

            Dim monCompte As String

            monCompte = "Nom de compte : " & TextBoxNomDeCompte.Text & " | "
            monCompte &= "Mot de passe : " & TextBoxMotDePasse.Text & " | "
            monCompte &= "Nom du personnage : " & TextBoxNomPersonnage.Text & " | "
            monCompte &= "Serveur : " & ComboBoxChoixServeur.Text

            ligneFinal &= monCompte

            'J'ouvre le fichier pour y écrire se que je souhaite
            Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\Compte/Comptes.txt")

            swEcriture.Write(ligneFinal)

            'Puis je le ferme.
            swEcriture.Close()

            'Je mets les informations de base pour que se soit visible directement par l'utilisateur.
            TextBoxNomDeCompte.Text = "Nom de compte"
            TextBoxMotDePasse.Text = "Mot de passe"
            TextBoxNomPersonnage.Text = "Nom du personnage"
            ComboBoxChoixServeur.SelectedIndex = 0

        Catch ex As Exception

            MsgBox(ex.Message)

        End Try

    End Sub

#End Region

#Region "Charger"

    Private Sub ChargeComptes()

        Try

            'Je supprime tout se qui se trouve dans la listbox
            ListBoxCompte.Items.Clear()

            'J'ouvre et je lis le fichier.
            Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Compte/Comptes.txt")

            Do Until swLecture.EndOfStream

                Dim Ligne As String = swLecture.ReadLine

                If Ligne <> "" Then

                    'Nom de compte : Linaculer | etc....
                    Dim separate() As String = Split(Ligne, " | ")

                    Dim nomDeCompte As String = Split(separate(0), " : ")(1)
                    Dim nomDuPersonnage As String = Split(separate(2), " : ")(1)

                    ListBoxCompte.Items.Add(nomDeCompte & " (" & nomDuPersonnage & ")") 'Nom de compte + Nom du personnage

                End If

            Loop

            'Puis je ferme le fichier.
            swLecture.Close()

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "LoadCompte", ex.Message)

        End Try

    End Sub

    Private Sub TextBoxGroupeNom_TextChanged(sender As Object, e As EventArgs) Handles TextBoxGroupeNom.Click

        sender.Text = ""

    End Sub

    Private Sub ButtonLoadCompte_Click() Handles ButtonLoadCompte.Click

        Dim frmGroupe As New FrmGroupe
        frmGroupe.Text = TextBoxGroupeNom.Text
        frmGroupe.MdiParent = LinaBot
        frmGroupe.Show()

        'Je lis le fichier pour obtenir les comptes.
        Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Compte/Comptes.txt")

        Do Until swLecture.EndOfStream

            Dim Ligne As String = swLecture.ReadLine

            If Ligne <> "" Then

                Dim separation() As String = Split(Ligne, " | ")

                'Je regarde si l'une des sélections correspond à la ligne actuel.
                If ListBoxCompte.SelectedItems.Contains(Split(separation(0), " : ")(1) & " (" & Split(separation(2), " : ")(1) & ")") Then 'Nom de compte + Nom du personnage

                    'J'ajoute alors aux comptes la class Player.
                    Comptes.Add(New Player)

                    'Puis pour le comptes actuel je met les informations nécessaire.
                    With Comptes(Comptes.Count - 1)

                        For a = 0 To separation.Count - 1

                            Dim separateInfo As String() = Split(separation(a), " : ")

                            Select Case separateInfo(0)

                                Case "Nom de compte"

                                    .Personnage.NomDeCompte = separateInfo(1)

                                Case "Mot de passe"

                                    .Personnage.MotDePasse = separateInfo(1)

                                Case "Nom du personnage"

                                    .Personnage.NomDuPersonnage = separateInfo(1)

                                Case "Serveur"

                                    .Personnage.Serveur = separateInfo(1)

                            End Select

                        Next

                        .Index = LinaBot.Compteur

                        .MITM = CheckBox_MITM.Checked

                        .FrmGroupe = frmGroupe

                        .FrmGroupe.BotIndex.Add(.Index)

                        Initialiser(LinaBot.Compteur, .FrmGroupe)

                        LinaBot.Compteur += 1

                    End With

                End If

            End If

        Loop

        swLecture.Close()

        Close()

    End Sub

#End Region
    Private Sub TabControl_Click(sender As Object, e As EventArgs) Handles TabControl1.Click

        Select Case TabControl1.SelectedTab.Text

            Case "Charger"

                ChargeComptes()
                TabPage2.Controls.Add(ListBoxCompte)

            Case "Supprimer"

                ChargeComptes()
                TabPage3.Controls.Add(ListBoxCompte)

        End Select

    End Sub

    Private Sub Button_Supprimer_Click(sender As Object, e As EventArgs) Handles Button_Supprimer.Click

        Try

            'J'ouvre et je lis le fichier.
            Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Compte/Comptes.txt")

            Dim ligneFinal As String = ""

            Do Until swLecture.EndOfStream

                Dim Ligne As String = swLecture.ReadLine

                If Ligne <> "" Then

                    Dim separate() As String = Split(Ligne, " | ")

                    Dim nomDeCompte As String = Split(separate(0), " : ")(1)
                    Dim nomDuPersonnage As String = Split(separate(2), " : ")(1)

                    'Si le compte n'est pas sélectionné, ça indique qu'il ne doit pas être supprimé, donc je le re écrit dans le fichier.
                    If Not ListBoxCompte.SelectedItems.Contains(nomDeCompte & " (" & nomDuPersonnage & ")") Then

                        ligneFinal &= Ligne & vbCrLf

                    Else

                        'Vue qu'il est sélectionné, donc il doit être supprimé, je ne le re écrit pas dans le fichier et je supprime aussi le fichier "option" du compte.
                        If IO.File.Exists(Application.StartupPath + "\Compte\Options\" & nomDeCompte & "_" & nomDuPersonnage & ".txt") Then

                            My.Computer.FileSystem.DeleteFile(Application.StartupPath + "\Compte\Options\" & nomDeCompte & "_" & nomDuPersonnage & ".txt")

                        End If

                    End If

                End If

            Loop

            'Puis je ferme le fichier.
            swLecture.Close()

            'Puis je réecrit le fichier sans le(s) compte(s) sélectionné(s)
            Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\Compte/Comptes.txt")

            swEcriture.Write(ligneFinal)

            swEcriture.Close()

            ChargeComptes()

        Catch ex As Exception

            ErreurFichier(0, "Unknow", "ButtonDeleteCompte", ex.Message)

        End Try

    End Sub

End Class