Imports System.Drawing.Drawing2D, System.Drawing.Text, System.Drawing
Imports System.ComponentModel
Module Draw
    Public Function RoundRect(ByVal Rectangle As Rectangle, ByVal Curve As Integer) As GraphicsPath
        Dim P As GraphicsPath = New GraphicsPath()
        Dim ArcRectangleWidth As Integer = Curve * 2
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -180, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -90, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 0, 90)
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 90, 90)
        P.AddLine(New Point(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y), New Point(Rectangle.X, Curve + Rectangle.Y))
        Return P
    End Function
    Public Function RoundRect(ByVal X As Integer, ByVal Y As Integer, ByVal Width As Integer, ByVal Height As Integer, ByVal Curve As Integer) As GraphicsPath
        Dim Rectangle As Rectangle = New Rectangle(X, Y, Width, Height)
        Dim P As GraphicsPath = New GraphicsPath()
        Dim ArcRectangleWidth As Integer = Curve * 2
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -180, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -90, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 0, 90)
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 90, 90)
        P.AddLine(New Point(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y), New Point(Rectangle.X, Curve + Rectangle.Y))
        Return P
    End Function
    Private Function ImageFromCode(ByRef str$) As Image
        Dim imageBytes As Byte() = Convert.FromBase64String(str)
        Dim ms As New IO.MemoryStream(imageBytes, 0, imageBytes.Length) : ms.Write(imageBytes, 0, imageBytes.Length)
        Dim i As Image = Image.FromStream(ms, True) : Return i
    End Function
    Public Function TiledTextureFromCode(ByVal str As String) As TextureBrush
        Return New TextureBrush(Draw.ImageFromCode(str), WrapMode.Tile)
    End Function
End Module

Public Class ProgressBarLina
    Inherits Control
    Dim MonTexte As String
    Dim Couleur As Color
    Protected Overrides Sub OnResize(e As EventArgs)
        MyBase.OnResize(e)
    End Sub
#Region "Properties"
    Public Overrides Property Text() As String
        Get
            Return MonTexte
        End Get
        Set(ByVal value As String)
            MonTexte = value
            Invalidate()
        End Set
    End Property
    Public Overrides Property ForeColor() As Color
        Get
            Return Couleur
        End Get
        Set(ByVal value As Color)

            Couleur = value
            Invalidate()

        End Set

    End Property

    Private val As Integer
    Public Property Value() As Integer
        Get
            Return val
        End Get
        Set(ByVal _value As Integer)
            If _value > max Then
                val = max
            ElseIf _value < 0 Then
                val = 0
            Else
                val = _value
            End If
            Invalidate()
        End Set
    End Property
    Private max As Integer
    Public Property Maximum() As Integer
        Get
            Return max
        End Get
        Set(ByVal _value As Integer)
            If _value < 1 Then
                max = 1
            Else
                max = _value
            End If

            If _value < val Then
                val = max
            End If

            Invalidate()
        End Set
    End Property
#End Region
    Sub New()
        max = 100
        SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
        SetStyle(ControlStyles.AllPaintingInWmPaint, True)
        SetStyle(ControlStyles.SupportsTransparentBackColor, True)
        BackColor = Color.Transparent
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        Dim curve As Integer = 6
        Dim b As New Bitmap(Width, Height)
        Dim g As Graphics = Graphics.FromImage(b)
        g.SmoothingMode = SmoothingMode.HighQuality
        g.TextRenderingHint = TextRenderingHint.ClearTypeGridFit
        Dim Fill As Integer = CInt((Width - 1) * (val / max))

        If Fill > 4 Then
            g.FillPath(New SolidBrush(Color.FromArgb(80, 164, 234)), Draw.RoundRect(New Rectangle(0, 0, Fill, Height - 2), curve))
            'Dim FillTexture As New HatchBrush(HatchStyle.DarkUpwardDiagonal, Color.FromArgb(100, 26, 127, 217), Color.Transparent)
            Dim Gloss As New LinearGradientBrush(New Rectangle(0, 0, Fill, Height - 2), Color.Green, Color.Green, 90S)
            g.FillPath(Gloss, Draw.RoundRect(New Rectangle(0, 0, Fill, Height - 2), curve))
            ' g.FillPath(Brushes.Lime, Draw.RoundRect(New Rectangle(0, 0, Fill, Height - 2), curve))
            Dim FillGradientBorder As New LinearGradientBrush(New Rectangle(0, 0, Fill, Height - 2), Color.FromArgb(183, 223, 249), Color.FromArgb(41, 141, 226), 90S)
            g.DrawPath(New Pen(FillGradientBorder), Draw.RoundRect(New Rectangle(1, 1, Fill - 2, Height - 4), curve))
            g.DrawPath(New Pen(Color.FromArgb(1, 44, 76)), Draw.RoundRect(New Rectangle(0, 0, Fill, Height - 2), curve))

        End If
        Dim BorderPen As New LinearGradientBrush(New Rectangle(0, 0, Width - 1, Height - 1), Color.Transparent, Color.FromArgb(87, 88, 92), 90S)
        g.DrawPath(New Pen(BorderPen), Draw.RoundRect(New Rectangle(0, 0, Width - 1, Height - 1), curve))
        g.DrawPath(New Pen(Color.FromArgb(32, 33, 37)), Draw.RoundRect(New Rectangle(0, 0, Width - 1, Height - 2), curve))

        Dim TextSize = g.MeasureString(MonTexte, Font)
        g.DrawString(MonTexte, Font, New SolidBrush(Couleur), Width / 2 - TextSize.Width / 2, Height / 2 - TextSize.Height / 2)

        e.Graphics.DrawImage(b.Clone, 0, 0)
        g.Dispose() : b.Dispose()
    End Sub
End Class
