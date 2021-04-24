Public Module Fonctions

    'Tout refaire avec explication + simplification du code.

    Public Function DistanceLa(ByVal pos1 As Integer, ByVal pos2 As Integer, ByVal MapLargeur As Integer) As Double

        Dim num4 As Decimal = Decimal.op_Decrement(Math.Ceiling(CDec(pos1 / ((MapLargeur * 2) - 1))))
        Dim num12 As Decimal = Decimal.op_Decrement(Math.Ceiling(CDec(pos2 / ((15 * 2) - 1))))
        Dim num15 As Decimal = num12 - Decimal.op_Modulus(pos2 - (num12 * ((15 * 2) - 1)), 15)
        Return Math.Sqrt(Math.Pow(Convert.ToDouble(pos2 - ((15 - 1) * num15 / 15) - (pos1 - ((MapLargeur - 1) * (num4 - Decimal.op_Modulus(pos1 - (num4 * ((MapLargeur * 2) - 1)), MapLargeur)))) / MapLargeur), 2) + Math.Pow(Convert.ToDouble(num15 - (num4 - Decimal.op_Modulus(pos1 - (num4 * ((MapLargeur * 2) - 1)), MapLargeur))), 2))

    End Function

    Public Function Distance2(ByVal pos1 As Integer, ByVal pos2 As Integer, ByVal MapLargeur As Integer) As Double
        Dim num18 As Double
        Dim num As Integer = pos1
        Dim num2 As Integer = MapLargeur
        Dim d As Decimal = num / ((num2 * 2) - 1)
        Dim num4 As Decimal = Decimal.op_Decrement(Math.Ceiling(d))
        Dim num5 As Decimal = num - (num4 * ((num2 * 2) - 1))
        Dim num6 As Decimal = Decimal.op_Modulus(num5, num2)
        Dim num7 As Decimal = num4 - num6
        Dim num8 As Decimal = (num - ((num2 - 1) * num7)) / num2
        Dim num9 As Integer = pos2
        Dim num10 As Integer = 15
        Dim num11 As Decimal = num9 / ((num10 * 2) - 1)
        Dim num12 As Decimal = Decimal.op_Decrement(Math.Ceiling(num11))
        Dim num13 As Decimal = num9 - (num12 * ((num10 * 2) - 1))
        Dim num14 As Decimal = Decimal.op_Modulus(num13, num10)
        Dim num15 As Decimal = num12 - num14
        Dim num16 As Decimal = (num9 - ((num10 - 1) * num15)) / num10
        num18 = Math.Sqrt(Math.Pow(Convert.ToDouble(num16 - num8), 2) + Math.Pow(Convert.ToDouble(num15 - num7), 2))
        Return num18
    End Function

    Private Class loc8
        Public y As Integer = 0
        Public x As Integer = 0
    End Class

    Public Function getX(ByVal laCase As Integer, ByVal MapLargeur As Integer)
        Try
            Dim _loc4 = MapLargeur
            Dim _loc5 = Math.Floor(laCase / (_loc4 * 2 - 1))
            Dim _loc6 = laCase - _loc5 * (_loc4 * 2 - 1)
            Dim _loc7 = _loc6 Mod _loc4
            Dim _loc8 As New loc8

            Dim y As Integer = _loc5 - _loc7
            Dim x As Integer = (laCase - (_loc4 - 1) * y) / _loc4
            Return x
        Catch ex As Exception

        End Try
        Return 0
    End Function

    Public Function getY(ByVal laCase As Integer, ByVal MapLargeur As Integer)
        Try
            Dim _loc4 = MapLargeur
            Dim _loc5 = Math.Floor(laCase / (_loc4 * 2 - 1))
            Dim _loc6 = laCase - _loc5 * (_loc4 * 2 - 1)
            Dim _loc7 = _loc6 Mod _loc4
            Dim _loc8 As New loc8
            Dim y As Integer = _loc5 - _loc7
            Dim x As Integer = (laCase - (_loc4 - 1) * y) / _loc4
            Return y
        Catch ex As Exception

        End Try
        Return 0
    End Function

    Public Function goalDistance(ByVal pos1 As Integer, ByVal pos2 As Integer, ByVal MapLargeur As Integer) As Integer
        Dim _loc7 = Math.Abs(getX(pos1, MapLargeur) - getX(pos2, MapLargeur))
        Dim _loc8 = Math.Abs(getY(pos1, MapLargeur) - getY(pos2, MapLargeur))
        Return _loc7 + _loc8
    End Function

    Public Function ReturnLastCell(ByVal code As String) As Integer
        Try
            Dim Number As Integer = 0
            Dim hash() As String = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", "_"}
            Dim hash2() As String = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
            Dim i As Integer = 0
            For i = 0 To hash2.Length - 1
                Dim j As Integer = 0
                For j = 0 To hash.Length - 1
                    If hash2(i) & hash(j) = code Then Return Number
                    Number = Number + 1
                Next
            Next i
        Catch ex As Exception
        End Try
        Return 0
    End Function

    Public Function Liste_Cellule_Porté(ByVal Index As Integer, ByVal Cellule_Départ As Integer, ByVal PO_Min As Integer, ByVal PO_Max As Integer, Optional ByVal En_Ligne As Integer = 0,
                                        Optional ByVal HG As Integer = 0, Optional ByVal HD As Integer = 0, Optional ByVal BG As Integer = 0, Optional ByVal BD As Integer = 0,
                                        Optional ByVal Line_Of_Sight As Integer = 0, Optional ByVal Cellule_Move As Integer = 0) As ArrayList

        With Comptes(Index) 'Zone = Z L N 
            Dim Liste_PO_Cellule, New_Liste As New ArrayList

            Try
                For i As Integer = PO_Min To PO_Max

                    Dim Haut_Gauche As Integer = (Cellule_Départ - (.Map.Largeur * i)) '-14
                    Dim Haut_Droite As Integer = (Cellule_Départ - ((.Map.Largeur - 1) * i)) '-15
                    Dim Bas_Gauche As Integer = (Cellule_Départ + ((.Map.Largeur - 1) * i)) '+14-----
                    Dim Bas_Droite As Integer = (Cellule_Départ + (.Map.Largeur * i)) '+15----

                    If Not Liste_PO_Cellule.Contains(Haut_Gauche) AndAlso Haut_Gauche < 1024 AndAlso Haut_Gauche > 0 AndAlso HG = 0 Then Liste_PO_Cellule.Add(Haut_Gauche)
                    If Not Liste_PO_Cellule.Contains(Haut_Droite) AndAlso Haut_Droite < 1024 AndAlso Haut_Droite > 0 AndAlso HD = 0 Then Liste_PO_Cellule.Add(Haut_Droite)
                    If Not Liste_PO_Cellule.Contains(Bas_Gauche) AndAlso Bas_Gauche < 1024 AndAlso Bas_Gauche > 0 AndAlso BG = 0 Then Liste_PO_Cellule.Add(Bas_Gauche)
                    If Not Liste_PO_Cellule.Contains(Bas_Droite) AndAlso Bas_Droite < 1024 AndAlso Bas_Droite > 0 AndAlso BD = 0 Then Liste_PO_Cellule.Add(Bas_Droite)

                    If i = 0 AndAlso Cellule_Départ = .Map.Entite(.Personnage.ID).Cellule Then Liste_PO_Cellule.Add(Cellule_Départ)

                    If En_Ligne = 0 Then '1 = le sort ne doit être lancer que en ligne

                        For a = Haut_Gauche To Haut_Droite
                            If Not Liste_PO_Cellule.Contains(a) AndAlso a < 1024 AndAlso a > 0 Then Liste_PO_Cellule.Add(a)
                        Next

                        For a = Bas_Gauche To Bas_Droite
                            If Not Liste_PO_Cellule.Contains(a) AndAlso a < 1024 AndAlso a > 0 Then Liste_PO_Cellule.Add(a)
                        Next

                        For a = Haut_Droite To Bas_Droite
                            If Not Liste_PO_Cellule.Contains(a) AndAlso a < 1024 AndAlso a > 0 Then Liste_PO_Cellule.Add(a)
                            a = a + (.Map.Largeur * 2 - 2) 'Car 'a' gagne +1 a chaque itération (voir si remplace +15 par mapcalcul1)
                        Next

                        For a = Haut_Gauche To Bas_Gauche
                            If Not Liste_PO_Cellule.Contains(a) AndAlso a < 1024 AndAlso a > 0 Then Liste_PO_Cellule.Add(a)
                            a = a + (.Map.Largeur * 2 - 2) '   a = a + (.Map_Hauteur * 2 - 2)'Car 'a' gagne +1 a chaque itération                           
                        Next

                    End If
                Next

                For Each Line As String In Liste_PO_Cellule
                    New_Liste.Add(Line)
                    'je retire les cellules où je peux pas voir à travers
                    If Line_Of_Sight = 1 AndAlso .Map.Handler(Line).lineOfSight = False Then
                        New_Liste.Remove(Line)
                        'Je retire les cellules sur lequel je peux pas marcher.
                    ElseIf Cellule_Move = 1 AndAlso .Map.Handler(Line).movement <= 1 Then
                        New_Liste.Remove(Line)
                    End If
                Next

            Catch ex As Exception
                ErreurFichier(Index, .Personnage.NomDuPersonnage, "Liste_Cellule_Porté", ex.Message)
            End Try
            Return New_Liste
        End With
    End Function 'FINI

#Region "Je le touche"

    Public Function Je_Le_Touche(ByVal Index As Integer, ByVal Cellule_Visé As Integer) As Boolean
        With Comptes(Index)

            Dim Yes As Boolean

            If Cellule_Visé > 0 AndAlso .Map.Handler(Cellule_Visé).movement > 0 AndAlso .Map.Handler(Cellule_Visé).lineOfSight Then

                MapViewerReste(Index)

                Dim b As Bitmap = New Bitmap(.Map.Map_Viewer_BitMap_All)
                Dim g As Graphics = Graphics.FromImage(b)

                'Création des points X/Y de mon personnage
                Dim X1_Moi As Integer = .Map.MapListeCelluleLeft(.Map.Entite(.Personnage.ID).Cellule).X + 40
                Dim Y1_Moi As Integer = .Map.MapListeCelluleLeft(.Map.Entite(.Personnage.ID).Cellule).Y

                'Création des points X/Y de la cible
                Dim X1_Cible As Integer = .Map.MapListeCelluleLeft(Cellule_Visé).X + 40
                Dim Y1_Cible As Integer = .Map.MapListeCelluleLeft(Cellule_Visé).Y

                'Création de la ligne + coloriage de la cellule ciblé + la mienne
                g.FillPolygon(Brushes.DarkRed, New Point() { .Map.MapListeCelluleLeft(Cellule_Visé), .Map.MapListeCelluleTop(Cellule_Visé), .Map.MapListeCelluleRight(Cellule_Visé), .Map.MapListeCelluleDown(Cellule_Visé)})
                g.FillPolygon(Brushes.DarkBlue, New Point() { .Map.MapListeCelluleLeft(.Map.Entite(.Personnage.ID).Cellule), .Map.MapListeCelluleTop(.Map.Entite(.Personnage.ID).Cellule), .Map.MapListeCelluleRight(.Map.Entite(.Personnage.ID).Cellule), .Map.MapListeCelluleDown(.Map.Entite(.Personnage.ID).Cellule)})
                g.DrawLine(Pens.Violet, X1_Moi, Y1_Moi, X1_Cible, Y1_Cible)

                'Choix du calcul du tracage
                If X1_Moi > X1_Cible AndAlso Y1_Moi < Y1_Cible Then
                    Yes = HD_BG_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi < X1_Cible AndAlso Y1_Moi < Y1_Cible Then
                    Yes = HG_BD_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi > X1_Cible AndAlso Y1_Moi > Y1_Cible Then
                    Yes = BD_HG_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi < X1_Cible AndAlso Y1_Moi > Y1_Cible Then
                    Yes = BG_HD_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi < X1_Cible AndAlso Y1_Moi = Y1_Cible Then
                    Yes = Ligne_Droite_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi > X1_Cible AndAlso Y1_Moi = Y1_Cible Then
                    Yes = Ligne_Gauche_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi = X1_Cible AndAlso Y1_Moi > Y1_Cible Then
                    Yes = Ligne_Haut_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                ElseIf X1_Moi = X1_Cible AndAlso Y1_Moi < Y1_Cible Then
                    Yes = Ligne_Bas_Touche(X1_Moi, Y1_Moi, X1_Cible, Y1_Cible, b)
                End If

                g.Dispose()
                b.Dispose()

            End If

            Return Yes

        End With

    End Function

    Private Function HD_BG_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi > X1_Cible AndAlso Y1_Moi < Y1_Cible
            Select Case B.GetPixel(X1_Moi, Y1_Moi).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" '"ffa9a9a9" = DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000" 'DarkRed (Cible visée)
                    Return True
                Case "ffee82ee" 'Violet (Ligne tracé)
                    Y1_Moi += 0.1
                Case Else
                    X1_Moi -= 0.1
            End Select
        End While
        Return False
    End Function

    Private Function HG_BD_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi < X1_Cible AndAlso Y1_Moi < Y1_Cible
            Select Case B.GetPixel(X1_Moi, Y1_Moi).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case "ffee82ee" 'Violet (Ligne tracé)
                    X1_Moi += 0.1
                Case Else
                    Y1_Moi += 0.1
            End Select
        End While
        Return False
    End Function

    Private Function BD_HG_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi > X1_Cible AndAlso Y1_Moi > Y1_Cible
            Select Case B.GetPixel(X1_Moi, Y1_Moi).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case "ffee82ee" 'Violet (Ligne tracé)
                    X1_Moi -= 0.1
                Case Else
                    Y1_Moi -= 0.1
            End Select
        End While
        Return False
    End Function

    Private Function BG_HD_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi < X1_Cible AndAlso Y1_Moi > Y1_Cible
            Select Case B.GetPixel(X1_Moi, Y1_Moi).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case "ffee82ee" 'Violet (Ligne tracé)
                    X1_Moi += 0.1
                Case Else
                    Y1_Moi -= 0.1
            End Select
        End While
        Return False
    End Function

    Private Function Ligne_Droite_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi <= X1_Cible AndAlso Y1_Moi = Y1_Cible
            Select Case B.GetPixel(X1_Moi, Y1_Moi + 2).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case Else
                    X1_Moi += 40
            End Select
        End While
        Return False
    End Function

    Private Function Ligne_Gauche_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi >= X1_Cible AndAlso Y1_Moi = Y1_Cible
            Select Case B.GetPixel(X1_Moi, Y1_Moi + 2).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case Else
                    X1_Moi -= 40
            End Select
        End While
        Return False
    End Function

    Private Function Ligne_Haut_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi = X1_Cible AndAlso Y1_Moi >= Y1_Cible
            Select Case B.GetPixel(X1_Moi + 2, Y1_Moi).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case Else
                    Y1_Moi -= 40
            End Select
        End While
        Return False
    End Function

    Private Function Ligne_Bas_Touche(ByVal X1_Moi As Double, ByVal Y1_Moi As Double, ByVal X1_Cible As Double, ByVal Y1_Cible As Double, ByVal B As Bitmap) As Boolean
        While X1_Moi = X1_Cible AndAlso Y1_Moi <= Y1_Cible
            Select Case B.GetPixel(X1_Moi + 2, Y1_Moi).Name
                Case "ffff0000", "ff00ffff", "ff008b8b", "ff9acd32", "ffffc0cb", "ff000000" 'DarkGray (Ldv = false) / Red (Ennemi) / Blue (Alliée) / Black (Obstacle total) / Pink (Pnj) / Cyan (Percepteur)
                    Return False
                Case "ff8b0000"  'DarkRed (Cible visée)
                    Return True
                Case Else
                    Y1_Moi += 40
            End Select
        End While
        Return False
    End Function

#End Region

End Module
