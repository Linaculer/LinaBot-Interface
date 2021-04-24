<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class LinaBot
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(LinaBot))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.Charger_Un_Compte = New System.Windows.Forms.ToolStripTextBox()
        Me.MITMToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.BackColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(56, Byte), Integer), CType(CType(60, Byte), Integer))
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.Charger_Un_Compte, Me.MITMToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(794, 27)
        Me.MenuStrip1.TabIndex = 6
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'Charger_Un_Compte
        '
        Me.Charger_Un_Compte.BackColor = System.Drawing.Color.FromArgb(CType(CType(51, Byte), Integer), CType(CType(56, Byte), Integer), CType(CType(60, Byte), Integer))
        Me.Charger_Un_Compte.Font = New System.Drawing.Font("Segoe UI", 9.0!)
        Me.Charger_Un_Compte.ForeColor = System.Drawing.Color.Cyan
        Me.Charger_Un_Compte.Name = "Charger_Un_Compte"
        Me.Charger_Un_Compte.Size = New System.Drawing.Size(120, 23)
        Me.Charger_Un_Compte.Text = "Gestion des comptes"
        '
        'MITMToolStripMenuItem
        '
        Me.MITMToolStripMenuItem.ForeColor = System.Drawing.Color.MediumSlateBlue
        Me.MITMToolStripMenuItem.Name = "MITMToolStripMenuItem"
        Me.MITMToolStripMenuItem.Size = New System.Drawing.Size(50, 23)
        Me.MITMToolStripMenuItem.Text = "MITM"
        '
        'LinaBot
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(794, 745)
        Me.Controls.Add(Me.MenuStrip1)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.IsMdiContainer = True
        Me.Name = "LinaBot"
        Me.Text = "LinaBot"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents Charger_Un_Compte As ToolStripTextBox
    Friend WithEvents MITMToolStripMenuItem As ToolStripMenuItem
End Class
