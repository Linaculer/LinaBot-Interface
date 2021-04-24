Public Module Maphandler

    'Simplification à faire.

    Private HEX_CHARS() As String = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"}
    Private ZKARRAY As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

    Private Function unescape(ByVal StringToDecode As String) As String
        Return Web.HttpUtility.UrlDecode(StringToDecode)
    End Function

    Public Function prepareKey(ByVal key As String)

        Dim d As String = key

        Dim _loc3 As String = ""
        Dim _loc4 As Integer = 0

        While _loc4 < d.Length

            _loc3 = _loc3 + Chr(Convert.ToInt64(d.Substring(_loc4, 2), 16))
            _loc4 += 2
        End While

        _loc3 = unescape(_loc3)

        Return _loc3

    End Function

    Public Function decypherData(ByVal d As String, ByVal k As String, ByVal c As Integer)

        Dim _loc5 As String = ""
        Dim _loc6 As Integer = k.Length
        Dim _loc7 As Integer = 0
        Dim _loc9 As Integer = 0

        While _loc9 < d.Length
            Dim a As Integer = Convert.ToInt64(d.Substring(_loc9, 2), 16)
            Dim b As Integer = Asc(k.Substring((_loc7 + c) Mod _loc6, 1))
            _loc5 &= Chr(a Xor b)
            _loc7 += 1
            _loc9 += 2
        End While

        _loc5 = unescape(_loc5)

        Return _loc5

    End Function

    Public Function checksum(ByVal s As String)

        Dim _loc3 As String = 0
        Dim _loc4 As String = 0

        While _loc4 < s.Length
            _loc3 = _loc3 + Asc(s.Substring(_loc4, 1)) Mod 16
            _loc4 += 1
        End While

        Return HEX_CHARS(_loc3 Mod 16)

    End Function

    Private Function hashCodes(ByVal a As String)
        Return ZKARRAY.IndexOf(a)
    End Function

    Public Structure Cell

        Dim movement As Integer
        Dim groundLevel As Integer
        Dim groundSlope As Integer
        Dim layerGroundRot As Integer
        Dim layerGroundNum As Integer
        Dim layerObject1Num As Integer
        Dim layerObject2Num As Integer
        Dim layerObject1Rot As Integer

        Dim active As Boolean 'Cellule active ou non (ou on peut marcher)
        Dim lineOfSight As Boolean
        Dim layerGroundFlip As Boolean
        Dim layerObject1Flip As Boolean
        Dim layerObject2Flip As Boolean
        Dim layerObject2Interactive As Boolean

    End Structure

    Private Function uncompressCell(ByVal sData As String) As Cell

        Dim _loc5 As Cell
        Dim _loc6 As String = sData
        Dim _loc7 As Integer = _loc6.Length - 1
        Dim _loc8(5000) As Integer

        While (_loc7 >= 0)
            _loc8(_loc7) = hashCodes(_loc6(_loc7))
            _loc7 -= 1
        End While

        _loc5.active = (_loc8(0) And 32) >> 5
        _loc5.lineOfSight = _loc8(0) And 1
        _loc5.layerGroundRot = (_loc8(1) And 48) >> 4
        _loc5.groundLevel = _loc8(1) And 15
        _loc5.movement = (_loc8(2) And 56) >> 3
        _loc5.layerGroundNum = ((_loc8(0) And 24) << 6) + ((_loc8(2) And 7) << 6) + _loc8(3)
        _loc5.groundSlope = (_loc8(4) And 60) >> 2
        _loc5.layerGroundFlip = (_loc8(4) And 2) >> 1
        _loc5.layerObject1Num = ((_loc8(0) And 4) << 11) + ((_loc8(4) And 1) << 12) + (_loc8(5) << 6) + _loc8(6)
        _loc5.layerObject1Rot = (_loc8(7) And 48) >> 4
        _loc5.layerObject1Flip = (_loc8(7) And 8) >> 3
        _loc5.layerObject2Flip = (_loc8(7) And 4) >> 2
        _loc5.layerObject2Interactive = (_loc8(7) And 2) >> 1
        _loc5.layerObject2Num = ((_loc8(0) And 2) << 12) + ((_loc8(7) And 1) << 12) + (_loc8(8) << 6) + _loc8(9)

        Return _loc5

    End Function

    Public Function uncompressMap(ByVal sData As String) As Cell()

        Dim _loc10(1024) As Cell
        Dim _loc11 As Integer = sData.Length
        Dim _loc13 As Integer = 0
        Dim _loc14 As Integer = 0
        While _loc14 < _loc11
            Dim _loc12 As Cell = uncompressCell(sData.Substring(_loc14, 10))
            _loc10(_loc13) = _loc12
            _loc14 += 10
            _loc13 += 1
            '  If _loc13 = 268 Then
            '  Dim veg = ""
            '  End If
        End While

        Return _loc10

    End Function

End Module
