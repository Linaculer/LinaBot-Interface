Public Class UcBot

    Private Delegate Function dlgF()
    Dim ThreadPlanning As Threading.Thread
    Public Index As Integer

    Private Sub Button_Connexion_Click(sender As Object, e As EventArgs) Handles Button_Connexion.Click

        With Comptes(Index)

            Select Case .MITM

                Case True

                    Select Case .Connecté

                        Case True

                            .Socket.Connexion_Game(False)

                        Case False

                            Select Case .EnConnexion

                                Case True

                                    .Socket_Authentification.Connexion_Game(False)

                                Case False

                                    .Main()
                                    .AppMITM = Shell(LinaBot.PathMITM & "/Dofus Retro.exe", AppWinStyle.NormalNoFocus)

                            End Select

                    End Select

                Case False

                    Select Case .Connecté

                        Case True

                            .Socket.Connexion_Game(False)

                        Case False

                            Select Case .EnConnexion

                                Case True

                                    .Socket_Authentification.Connexion_Game(False)

                                Case False

                                    .CreateSocketAuthentification(VarServeur("Authentification").IP, VarServeur("Authentification").Port)

                            End Select

                    End Select

            End Select

        End With

    End Sub

#Region "Tchat"

#Region "CheckBox"

    Private Sub CheckBox_Information_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox_Tchat_Information.Click,
        CheckBox_Tchat_Communs.Click, CheckBox_Tchat_Groupe.Click, CheckBox_Tchat_Guilde.Click,
        CheckBox_Tchat_Alignement.Click, CheckBox_Tchat_Recrutement.Click, CheckBox_Tchat_Commerce.Click

        Task.Run(Function() CanalActiveDesactive(Index, sender.Text, sender.Checked))


    End Sub

#End Region

#Region "Richtextbox + TextBox"

    Dim CompteurTchat, CompteurSocket As Integer

    Private Sub RichTextBox2_TextChanged(sender As Object, e As EventArgs) Handles RichTextBoxSocket.TextChanged

        If CompteurSocket > 600 Then

            RichTextBoxSocket.Clear()

            CompteurSocket = 0

        End If

        CompteurSocket += 1

    End Sub

    Private Sub RichTextBox1_TextChanged(sender As Object, e As EventArgs) Handles RichTextBoxTchat.TextChanged

        If CompteurTchat > 600 Then

            RichTextBoxTchat.Clear()

            CompteurTchat = 0

        End If

        CompteurTchat += 1

    End Sub

    Private Sub TextBox_Tchat_TextChanged(sender As Object, e As KeyEventArgs) Handles TextBox_Tchat.KeyDown

        If e.KeyCode = 13 Then

            ButtonTchat_Click(Nothing, Nothing)

        End If

    End Sub

#End Region

#Region "Button"

    Private Sub ButtonTchat_Click(sender As Object, e As EventArgs) Handles Button_Tchat.Click

        Dim message As String = TextBox_Tchat.Text

        Task.Run(Function() CanalEnvoieMessage(Index, message))

        TextBox_Tchat.Text = ""

    End Sub

#End Region

#End Region

#Region "Option"

    Private Sub Button_Option_Sauvegarder_Click(sender As Object, e As EventArgs) Handles Button_Option_Sauvegarder.Click

        Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\Compte\Options/" & Comptes(Index).Personnage.NomDeCompte & "_" & Comptes(Index).Personnage.NomDuPersonnage & ".txt")


        swEcriture.Write("Caracteristique|" & OptionCaracteristique() & vbCrLf &
                         "Sort|" & OptionSort() & vbCrLf &
                         "Proxy|" & OptionProxy() & vbCrLf &
                         "Planning|" & OptionPlanning() & vbCrLf &
                         "IA|" & Label_IA.Name & vbCrLf)

        'Puis je le ferme.
        swEcriture.Close()

    End Sub

    Private Function OptionCaracteristique() As String

        Dim resultat As String = ""

        Try

            resultat &= "Up Automatique = " & CheckBox_Caracteristique_UpAutomatique.Checked

            For Each ctl As Control In GroupBox2.Controls

                If TypeOf (ctl) Is RadioButton Then

                    Dim rdb As RadioButton = TryCast(ctl, RadioButton)

                    If rdb.Checked Then

                        resultat &= "|Caracteristique a up = " & Split(rdb.Name, "_")(2)

                        Exit For

                    End If

                End If

            Next

        Catch ex As Exception

        End Try

        Return resultat

    End Function

    Private Function OptionSort() As String

        Dim resultat As String = ""

        Try

            resultat &= "Up Automatique = " & CheckBox_Sort_UpSort.Checked & "|" & "IA Automatique = " & CheckBox_Sort_IAAutomatique.Checked

            For Each pair As DataGridViewRow In DataGridView_Sort.Rows

                If pair.Cells(0).Value = True Then

                    resultat &= "|ID Sort = " & pair.Cells(1).Value

                End If

            Next

        Catch ex As Exception

        End Try

        Return resultat

    End Function

    Private Function OptionPlanning() As String

        Dim resultat As String = ""

        Try

            resultat &= "Active planning = " & CheckBox_Planning.Checked

            For Each pair As KeyValuePair(Of String, Boolean()) In Comptes(Index).Option.Planning

                Select Case pair.Key.ToLower

                    Case "lundi"

                        resultat &= "|Lundi = "

                    Case "mardi"

                        resultat &= "|Mardi = "

                    Case "mercredi"

                        resultat &= "|Mercredi = "

                    Case "jeudi"

                        resultat &= "|Jeudi = "

                    Case "vendredi"

                        resultat &= "|Vendredi = "

                    Case "samedi"

                        resultat &= "|Samedi = "

                    Case "dimanche"

                        resultat &= "|Dimanche = "

                End Select

                For i = 0 To pair.Value.Count - 1

                    resultat &= ":" & pair.Value(i).ToString

                Next

            Next

        Catch ex As Exception

        End Try

        Return resultat

    End Function


    Private Sub PictureBox_Planning_00_Click(sender As Object, e As EventArgs) Handles PictureBox_Planning_00.Click,
            PictureBox_Planning_01.Click, PictureBox_Planning_02.Click, PictureBox_Planning_03.Click, PictureBox_Planning_04.Click,
            PictureBox_Planning_05.Click, PictureBox_Planning_06.Click, PictureBox_Planning_07.Click, PictureBox_Planning_08.Click,
            PictureBox_Planning_09.Click, PictureBox_Planning_10.Click, PictureBox_Planning_11.Click, PictureBox_Planning_12.Click,
            PictureBox_Planning_13.Click, PictureBox_Planning_14.Click, PictureBox_Planning_15.Click, PictureBox_Planning_16.Click,
            PictureBox_Planning_17.Click, PictureBox_Planning_18.Click, PictureBox_Planning_19.Click, PictureBox_Planning_20.Click,
            PictureBox_Planning_21.Click, PictureBox_Planning_22.Click, PictureBox_Planning_23.Click

        Select Case sender.BackColor

            Case Color.Lime

                sender.BackColor = Color.Red

            Case Color.Red

                sender.BackColor = Color.Lime

        End Select

        OptionPlanningSauvegarde()

    End Sub

    Private Sub OptionPlanningSauvegarde()

        Try

            Dim pb() As PictureBox = {PictureBox_Planning_00,
            PictureBox_Planning_01, PictureBox_Planning_02, PictureBox_Planning_03, PictureBox_Planning_04,
            PictureBox_Planning_05, PictureBox_Planning_06, PictureBox_Planning_07, PictureBox_Planning_08,
            PictureBox_Planning_09, PictureBox_Planning_10, PictureBox_Planning_11, PictureBox_Planning_12,
            PictureBox_Planning_13, PictureBox_Planning_14, PictureBox_Planning_15, PictureBox_Planning_16,
            PictureBox_Planning_17, PictureBox_Planning_18, PictureBox_Planning_19, PictureBox_Planning_20,
            PictureBox_Planning_21, PictureBox_Planning_22, PictureBox_Planning_23}

            For Each pair As KeyValuePair(Of String, Boolean()) In Comptes(Index).Option.Planning

                If pair.Key = (ComboBox_Planning.Text) Then

                    For i = 0 To pair.Value.Count - 1

                        pair.Value(i) = pb(i).BackColor = Color.Lime

                    Next

                End If

            Next

        Catch ex As Exception

        End Try

    End Sub

    Private Sub UcBot_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ' Dim jourActuel As Date = Date.Now

        '  ComboBox_Planning.Text = jourActuel.ToString("dddd")

    End Sub

    Private Sub ComboBox_Planning_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox_Planning.SelectedIndexChanged, ComboBox_Planning.TextChanged

        Try

            Dim pb() As PictureBox = {PictureBox_Planning_00,
            PictureBox_Planning_01, PictureBox_Planning_02, PictureBox_Planning_03, PictureBox_Planning_04,
            PictureBox_Planning_05, PictureBox_Planning_06, PictureBox_Planning_07, PictureBox_Planning_08,
            PictureBox_Planning_09, PictureBox_Planning_10, PictureBox_Planning_11, PictureBox_Planning_12,
            PictureBox_Planning_13, PictureBox_Planning_14, PictureBox_Planning_15, PictureBox_Planning_16,
            PictureBox_Planning_17, PictureBox_Planning_18, PictureBox_Planning_19, PictureBox_Planning_20,
            PictureBox_Planning_21, PictureBox_Planning_22, PictureBox_Planning_23}

            For Each pair As KeyValuePair(Of String, Boolean()) In Comptes(Index).Option.Planning

                If pair.Key = (ComboBox_Planning.Text) Then

                    For i = 0 To pair.Value.Count - 1

                        pb(i).BackColor = If(pair.Value(i), Color.Lime, Color.Red)

                    Next

                End If

            Next

        Catch ex As Exception

        End Try

    End Sub

#Region "Proxy"

    Private Function OptionProxy() As String

        Dim resultat As String = ""

        Try

            resultat &= "Active = " & CheckBox_Proxy.Checked
            resultat &= "|IP = " & TextBox_Proxy_IP.Text
            resultat &= "|PORT = " & TextBox_Proxy_Port.Text
            resultat &= "|Identifiant = " & TextBox_Proxy_Identifiant.Text
            resultat &= "|Mot de passe = " & TextBox_Proxy_MotDePasse.Text


        Catch ex As Exception

        End Try

        Return resultat

    End Function

    Private Sub CheckBox_Proxy_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox_Proxy.CheckedChanged

        Comptes(Index).Proxy.Active = CheckBox_Proxy.Checked

    End Sub

    Private Sub TextBox_Proxy_IP_TextChanged(sender As Object, e As EventArgs) Handles TextBox_Proxy_IP.TextChanged

        Comptes(Index).Proxy.IP = TextBox_Proxy_IP.Text

    End Sub

    Private Sub TextBox_Proxy_Port_TextChanged(sender As Object, e As EventArgs) Handles TextBox_Proxy_Port.TextChanged

        Comptes(Index).Proxy.Port = TextBox_Proxy_Port.Text

    End Sub

    Private Sub TextBox_Proxy_Identifiant_TextChanged(sender As Object, e As EventArgs) Handles TextBox_Proxy_Identifiant.TextChanged

        Comptes(Index).Proxy.Identifiant = TextBox_Proxy_Identifiant.Text

    End Sub

    Private Sub TextBox_Proxy_MotDePasse_TextChanged(sender As Object, e As EventArgs) Handles TextBox_Proxy_MotDePasse.TextChanged

        Comptes(Index).Proxy.MotDePasse = TextBox_Proxy_MotDePasse.Text

    End Sub

#End Region

#End Region

#Region "IA"

    Private Sub Button_IA_Click(sender As Object, e As EventArgs) Handles Button_IA.Click

        With Comptes(Index)

            Try

                If .ThreadIA IsNot Nothing AndAlso .ThreadIA.IsAlive Then

                    .ThreadIA.Abort()

                    Button_IA.BackgroundImage = My.Resources.Ampoule_Off
                    Button_IA.FlatAppearance.BorderColor = Color.Red

                    Label_IA.Text = "Aucun trajet."
                    Label_IA.Name = "IA_Nothing"

                Else

                    Dim Ouverture_Fichier As New OpenFileDialog

                    If Ouverture_Fichier.ShowDialog = 1 Then

                        .ThreadIA = New Threading.Thread(Sub() IALoad(Index, Ouverture_Fichier.FileName)) With {.IsBackground = True}
                        .ThreadIA.Start()

                        Button_IA.BackgroundImage = My.Resources.Ampoule_On
                        Button_IA.FlatAppearance.BorderColor = Color.Lime

                        Dim nom As String() = Split(Ouverture_Fichier.FileName, "\")
                        Label_IA.Text = nom(nom.Count - 1)
                        Label_IA.Name = Ouverture_Fichier.FileName

                    End If

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Sub CheckBox_Sort_IAAutomatique_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox_Sort_IAAutomatique.CheckedChanged

        Select Case sender.Checked

            Case True

                Button_IA.Visible = False
                Label_IA.Visible = False

            Case False

                Button_IA.Visible = True
                Label_IA.Visible = True

        End Select

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        Try
            Comptes(Index).ClientSend("cs<font color='#0011FF'><b>Bienvenue</b> sur <b>LinaBot</b> ! Tapes <b>.Help</b> pour acceder aux commandes.</font>" & vbNullChar)

        Catch ex As Exception

        End Try
    End Sub

#End Region

#Region "Planning"

    Private Sub CheckBox_Planning_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox_Planning.CheckedChanged

        If Comptes(Index).MITM = False Then

            If CheckBox_Planning.Checked Then

            ComboBox_Planning.Text = Date.Now.ToString("dddd")

            ThreadPlanning = New Threading.Thread(AddressOf Planning) With {.IsBackground = True}
            ThreadPlanning.Start()

        Else

            If ThreadPlanning IsNot Nothing AndAlso ThreadPlanning.IsAlive Then

                ThreadPlanning.Abort()

            End If

        End If

        End If

    End Sub

    Private Sub Planning()

        With Comptes(Index)

            Try

                While True

                    Select Case PictureBox_Planning_Co_Deco(Date.Now.Hour)

                        Case True

                            Try

                                If .Connecté = False AndAlso .EnConnexion = False AndAlso .EnAuthentification = False Then

                                    .CreateSocketAuthentification(VarServeur("Authentification").IP, VarServeur("Authentification").Port)

                                End If

                            Catch ex As Exception

                            End Try

                        Case False

                            Try

                                If .Connecté OrElse .EnConnexion Then

                                    .Socket.Connexion_Game(False)

                                ElseIf .EnAuthentification Then

                                    .Socket_Authentification.Connexion_Game(False)

                                End If

                            Catch ex As Exception

                            End Try

                    End Select

                    Task.Delay(10000).Wait()

                End While

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Function PictureBox_Planning_Co_Deco(numero As Integer) As Boolean

        With Comptes(Index)

            If InvokeRequired Then

                Return Invoke(New dlgF(Function() PictureBox_Planning_Co_Deco(numero)))

            Else

                ComboBox_Planning.Text = Date.Now.ToString("dddd")

                Dim pb() As PictureBox = {PictureBox_Planning_00,
           PictureBox_Planning_01, PictureBox_Planning_02, PictureBox_Planning_03, PictureBox_Planning_04,
           PictureBox_Planning_05, PictureBox_Planning_06, PictureBox_Planning_07, PictureBox_Planning_08,
           PictureBox_Planning_09, PictureBox_Planning_10, PictureBox_Planning_11, PictureBox_Planning_12,
           PictureBox_Planning_13, PictureBox_Planning_14, PictureBox_Planning_15, PictureBox_Planning_16,
           PictureBox_Planning_17, PictureBox_Planning_18, PictureBox_Planning_19, PictureBox_Planning_20,
           PictureBox_Planning_21, PictureBox_Planning_22, PictureBox_Planning_23}

                If pb(numero).BackColor = Color.Lime Then

                    Return True

                Else

                    Return False

                End If

            End If

        End With

    End Function

    Private Sub Label_TrajetNom_Click(sender As Object, e As EventArgs) Handles Label_TrajetNom.Click

    End Sub

#End Region

    Private Sub Button_Trajet_Click(sender As Object, e As EventArgs) Handles Button_Trajet.Click

        With Comptes(Index)

            Try

                If .ThreadTrajet IsNot Nothing AndAlso .ThreadTrajet.IsAlive Then

                    .ThreadTrajet.Abort()

                    Button_Trajet.BackgroundImage = My.Resources.Parchemin_Off
                    Button_Trajet.FlatAppearance.BorderColor = Color.Red

                    Label_TrajetNom.Text = "Aucun trajet."

                Else

                    Dim Ouverture_Fichier As New OpenFileDialog

                    If Ouverture_Fichier.ShowDialog = 1 Then

                        .ThreadTrajet = New Threading.Thread(Sub() TrajetLoad(Index, Ouverture_Fichier.FileName)) With {.IsBackground = True}
                        .ThreadTrajet.Start()

                        Button_Trajet.BackgroundImage = My.Resources.Parchemin_On
                        Button_Trajet.FlatAppearance.BorderColor = Color.Lime

                        Dim nom As String() = Split(Ouverture_Fichier.FileName, "\")
                        Label_TrajetNom.Text = nom(nom.Count - 1)

                    End If

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub



End Class
