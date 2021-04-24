<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class GestionComptes
    Inherits System.Windows.Forms.Form

    'Form remplace la méthode Dispose pour nettoyer la liste des composants.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requise par le Concepteur Windows Form
    Private components As System.ComponentModel.IContainer

    'REMARQUE : la procédure suivante est requise par le Concepteur Windows Form
    'Elle peut être modifiée à l'aide du Concepteur Windows Form.  
    'Ne la modifiez pas à l'aide de l'éditeur de code.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.ButtonAjouter = New System.Windows.Forms.Button()
        Me.ComboBoxChoixServeur = New System.Windows.Forms.ComboBox()
        Me.TextBoxNomPersonnage = New System.Windows.Forms.TextBox()
        Me.TextBoxMotDePasse = New System.Windows.Forms.TextBox()
        Me.TextBoxNomDeCompte = New System.Windows.Forms.TextBox()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.LabelNomGroupe = New System.Windows.Forms.Label()
        Me.ListBoxCompte = New System.Windows.Forms.ListBox()
        Me.ButtonLoadCompte = New System.Windows.Forms.Button()
        Me.TextBoxGroupeNom = New System.Windows.Forms.TextBox()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.Button_Supprimer = New System.Windows.Forms.Button()
        Me.CheckBox_MITM = New System.Windows.Forms.CheckBox()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TabControl1.Location = New System.Drawing.Point(12, 12)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(297, 377)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.BackColor = System.Drawing.Color.FromArgb(CType(CType(43, Byte), Integer), CType(CType(44, Byte), Integer), CType(CType(48, Byte), Integer))
        Me.TabPage1.Controls.Add(Me.ButtonAjouter)
        Me.TabPage1.Controls.Add(Me.ComboBoxChoixServeur)
        Me.TabPage1.Controls.Add(Me.TextBoxNomPersonnage)
        Me.TabPage1.Controls.Add(Me.TextBoxMotDePasse)
        Me.TabPage1.Controls.Add(Me.TextBoxNomDeCompte)
        Me.TabPage1.Location = New System.Drawing.Point(4, 25)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(289, 348)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Ajouter"
        '
        'ButtonAjouter
        '
        Me.ButtonAjouter.FlatAppearance.BorderColor = System.Drawing.Color.Lime
        Me.ButtonAjouter.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ButtonAjouter.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ButtonAjouter.ForeColor = System.Drawing.Color.White
        Me.ButtonAjouter.Location = New System.Drawing.Point(6, 307)
        Me.ButtonAjouter.Name = "ButtonAjouter"
        Me.ButtonAjouter.Size = New System.Drawing.Size(277, 35)
        Me.ButtonAjouter.TabIndex = 46
        Me.ButtonAjouter.Text = "Ajouter le(s) compte(s)"
        Me.ButtonAjouter.UseVisualStyleBackColor = True
        '
        'ComboBoxChoixServeur
        '
        Me.ComboBoxChoixServeur.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxChoixServeur.FormattingEnabled = True
        Me.ComboBoxChoixServeur.Location = New System.Drawing.Point(6, 114)
        Me.ComboBoxChoixServeur.Name = "ComboBoxChoixServeur"
        Me.ComboBoxChoixServeur.Size = New System.Drawing.Size(277, 24)
        Me.ComboBoxChoixServeur.TabIndex = 45
        '
        'TextBoxNomPersonnage
        '
        Me.TextBoxNomPersonnage.BackColor = System.Drawing.Color.White
        Me.TextBoxNomPersonnage.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBoxNomPersonnage.ForeColor = System.Drawing.Color.Black
        Me.TextBoxNomPersonnage.Location = New System.Drawing.Point(6, 78)
        Me.TextBoxNomPersonnage.Multiline = True
        Me.TextBoxNomPersonnage.Name = "TextBoxNomPersonnage"
        Me.TextBoxNomPersonnage.Size = New System.Drawing.Size(277, 30)
        Me.TextBoxNomPersonnage.TabIndex = 44
        Me.TextBoxNomPersonnage.Text = "Nom du personnage"
        Me.TextBoxNomPersonnage.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'TextBoxMotDePasse
        '
        Me.TextBoxMotDePasse.BackColor = System.Drawing.Color.White
        Me.TextBoxMotDePasse.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBoxMotDePasse.ForeColor = System.Drawing.Color.Black
        Me.TextBoxMotDePasse.Location = New System.Drawing.Point(6, 42)
        Me.TextBoxMotDePasse.Multiline = True
        Me.TextBoxMotDePasse.Name = "TextBoxMotDePasse"
        Me.TextBoxMotDePasse.Size = New System.Drawing.Size(277, 30)
        Me.TextBoxMotDePasse.TabIndex = 43
        Me.TextBoxMotDePasse.Text = "Mot de passe"
        Me.TextBoxMotDePasse.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'TextBoxNomDeCompte
        '
        Me.TextBoxNomDeCompte.BackColor = System.Drawing.Color.White
        Me.TextBoxNomDeCompte.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextBoxNomDeCompte.ForeColor = System.Drawing.Color.Black
        Me.TextBoxNomDeCompte.Location = New System.Drawing.Point(6, 6)
        Me.TextBoxNomDeCompte.Multiline = True
        Me.TextBoxNomDeCompte.Name = "TextBoxNomDeCompte"
        Me.TextBoxNomDeCompte.Size = New System.Drawing.Size(277, 30)
        Me.TextBoxNomDeCompte.TabIndex = 42
        Me.TextBoxNomDeCompte.Text = "Nom de compte"
        Me.TextBoxNomDeCompte.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'TabPage2
        '
        Me.TabPage2.BackColor = System.Drawing.Color.FromArgb(CType(CType(43, Byte), Integer), CType(CType(44, Byte), Integer), CType(CType(48, Byte), Integer))
        Me.TabPage2.Controls.Add(Me.CheckBox_MITM)
        Me.TabPage2.Controls.Add(Me.LabelNomGroupe)
        Me.TabPage2.Controls.Add(Me.ListBoxCompte)
        Me.TabPage2.Controls.Add(Me.ButtonLoadCompte)
        Me.TabPage2.Controls.Add(Me.TextBoxGroupeNom)
        Me.TabPage2.Location = New System.Drawing.Point(4, 25)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(289, 348)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Charger"
        '
        'LabelNomGroupe
        '
        Me.LabelNomGroupe.AutoSize = True
        Me.LabelNomGroupe.ForeColor = System.Drawing.Color.White
        Me.LabelNomGroupe.Location = New System.Drawing.Point(6, 260)
        Me.LabelNomGroupe.Name = "LabelNomGroupe"
        Me.LabelNomGroupe.Size = New System.Drawing.Size(101, 16)
        Me.LabelNomGroupe.TabIndex = 23
        Me.LabelNomGroupe.Text = "Nom du groupe"
        '
        'ListBoxCompte
        '
        Me.ListBoxCompte.BackColor = System.Drawing.Color.White
        Me.ListBoxCompte.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.ListBoxCompte.ForeColor = System.Drawing.Color.Black
        Me.ListBoxCompte.FormattingEnabled = True
        Me.ListBoxCompte.HorizontalScrollbar = True
        Me.ListBoxCompte.ItemHeight = 16
        Me.ListBoxCompte.Location = New System.Drawing.Point(6, 6)
        Me.ListBoxCompte.Name = "ListBoxCompte"
        Me.ListBoxCompte.SelectionMode = System.Windows.Forms.SelectionMode.MultiSimple
        Me.ListBoxCompte.Size = New System.Drawing.Size(276, 210)
        Me.ListBoxCompte.TabIndex = 20
        '
        'ButtonLoadCompte
        '
        Me.ButtonLoadCompte.FlatAppearance.BorderColor = System.Drawing.Color.Blue
        Me.ButtonLoadCompte.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ButtonLoadCompte.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ButtonLoadCompte.ForeColor = System.Drawing.Color.White
        Me.ButtonLoadCompte.Location = New System.Drawing.Point(6, 292)
        Me.ButtonLoadCompte.Name = "ButtonLoadCompte"
        Me.ButtonLoadCompte.Size = New System.Drawing.Size(276, 49)
        Me.ButtonLoadCompte.TabIndex = 21
        Me.ButtonLoadCompte.Text = "Charger le(s) compte(s)"
        Me.ButtonLoadCompte.UseVisualStyleBackColor = True
        '
        'TextBoxGroupeNom
        '
        Me.TextBoxGroupeNom.BackColor = System.Drawing.Color.White
        Me.TextBoxGroupeNom.ForeColor = System.Drawing.Color.Black
        Me.TextBoxGroupeNom.Location = New System.Drawing.Point(113, 257)
        Me.TextBoxGroupeNom.Name = "TextBoxGroupeNom"
        Me.TextBoxGroupeNom.Size = New System.Drawing.Size(169, 22)
        Me.TextBoxGroupeNom.TabIndex = 22
        Me.TextBoxGroupeNom.Text = "Groupe"
        Me.TextBoxGroupeNom.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'TabPage3
        '
        Me.TabPage3.BackColor = System.Drawing.Color.FromArgb(CType(CType(43, Byte), Integer), CType(CType(44, Byte), Integer), CType(CType(48, Byte), Integer))
        Me.TabPage3.Controls.Add(Me.Button_Supprimer)
        Me.TabPage3.Location = New System.Drawing.Point(4, 25)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage3.Size = New System.Drawing.Size(289, 348)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Supprimer"
        '
        'Button_Supprimer
        '
        Me.Button_Supprimer.FlatAppearance.BorderColor = System.Drawing.Color.Red
        Me.Button_Supprimer.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.Button_Supprimer.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button_Supprimer.ForeColor = System.Drawing.Color.White
        Me.Button_Supprimer.Location = New System.Drawing.Point(6, 293)
        Me.Button_Supprimer.Name = "Button_Supprimer"
        Me.Button_Supprimer.Size = New System.Drawing.Size(276, 49)
        Me.Button_Supprimer.TabIndex = 22
        Me.Button_Supprimer.Text = "Supprimer le(s) compte(s)"
        Me.Button_Supprimer.UseVisualStyleBackColor = True
        '
        'CheckBox_MITM
        '
        Me.CheckBox_MITM.AutoSize = True
        Me.CheckBox_MITM.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CheckBox_MITM.ForeColor = System.Drawing.Color.Lime
        Me.CheckBox_MITM.Location = New System.Drawing.Point(31, 222)
        Me.CheckBox_MITM.Name = "CheckBox_MITM"
        Me.CheckBox_MITM.Size = New System.Drawing.Size(222, 24)
        Me.CheckBox_MITM.TabIndex = 24
        Me.CheckBox_MITM.Text = "Lancer le(s) bot(s) en MITM"
        Me.CheckBox_MITM.UseVisualStyleBackColor = True
        '
        'GestionComptes
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.FromArgb(CType(CType(43, Byte), Integer), CType(CType(44, Byte), Integer), CType(CType(48, Byte), Integer))
        Me.ClientSize = New System.Drawing.Size(320, 400)
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "GestionComptes"
        Me.Text = "Gestionnaire de comptes"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents ButtonAjouter As Button
    Friend WithEvents ComboBoxChoixServeur As ComboBox
    Friend WithEvents TextBoxNomPersonnage As TextBox
    Friend WithEvents TextBoxMotDePasse As TextBox
    Friend WithEvents TextBoxNomDeCompte As TextBox
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents LabelNomGroupe As Label
    Friend WithEvents ListBoxCompte As ListBox
    Friend WithEvents ButtonLoadCompte As Button
    Friend WithEvents TextBoxGroupeNom As TextBox
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents Button_Supprimer As Button
    Friend WithEvents CheckBox_MITM As CheckBox
End Class
