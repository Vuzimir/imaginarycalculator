Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Label4.Text = "Im(z2)="
        Label3.Text = "Im(z1)="
        Label5.Text = "Re(z1)="
        Label6.Text = "Re(z2)="
        Dim z1
        Dim duzina As Long
        Dim sl As Long
        Dim pst As Long
        Dim TestChar As Char
        Dim provjera As Char
        Dim z1str As String
        Dim z1strzam As String
        Dim z2str As String
        Dim dodao = 0
        Dim Nasaop = 0
        Dim Remi1 As String
        Dim Remi2 As String
        duzina = Len(TextBox1.Text)
        Remi1 = TextBox1.Text
        Remi2 = TextBox2.Text
        z1 = TextBox1.Text
        sl = InStr(TextBox1.Text, "i")

        'Ovo ispod je forma ako su prvo brojev pa tek onda I
        For value As Integer = 1 To 20
            Dim check = sl - value
            If (check < 1) Then
                Continue For
            End If
            'Provjera dali se imaginarni broj mnozi sa necim.
            TestChar = GetChar(TextBox1.Text, sl - value)
            If (TestChar = "*") Then
                provjera = GetChar(TextBox1.Text, sl - value - 1)
                If (IsNumeric(provjera)) Then
                    provjera = GetChar(TextBox1.Text, sl - value + 1)
                    If (IsNumeric(provjera)) Then
                        Remi1 = Remi1.Replace(TestChar, "")
                        Nasaop = 1
                        z1str += "*"
                        dodao = 1
                        Continue For
                    End If
                Else
                    Exit For
                End If
            End If

            'provjera daili je upisano integri pa onda * i
            If (TestChar = "*") Then
                provjera = GetChar(TextBox1.Text, sl - value - 1)
                If (IsNumeric(provjera)) Then
                    Remi1 = Remi1.Replace(TestChar, "")
                    Continue For
                Else
                    Exit For
                End If
            End If

            'Provjera dali se dijeli imaginarni broj
            If (TestChar = "/") Then
                If (Nasaop = 1) Then
                    Exit For
                End If
                provjera = GetChar(TextBox1.Text, sl - value - 1)
                If (IsNumeric(provjera)) Then
                    Remi1 = Remi1.Replace(TestChar, "")
                    Nasaop = 1
                    z1str += "/"
                    dodao = 1
                    Continue For
                Else
                    Exit For
                End If
            End If
            If (IsNumeric(TestChar)) Then
                Remi1 = Remi1.Replace(TestChar, "")
                z1str += TestChar
                dodao = 1
            Else
                provjera = GetChar(TextBox1.Text, sl - value)
                Remi1 = Remi1.Replace(TestChar, "")
                If (provjera = "-") Then
                    z1str += "-"
                    Exit For
                Else
                    Exit For
                End If
            End If
        Next
        Nasaop = 0





        'Ovo je sad forma kada je prvo I pa tek onda su brojevi
        For value As Integer = 1 To 20
            Dim check = sl + value
            If (check > duzina) Then
                Continue For
            End If
            TestChar = GetChar(TextBox1.Text, sl + value)
            'Provjera dali se imaginarni broj mnozi sa necim.
            If (TestChar = "*") Then
                provjera = GetChar(TextBox1.Text, sl + value + 1)
                If (IsNumeric(provjera)) Then
                    provjera = GetChar(TextBox1.Text, sl + value - 1)
                    If (IsNumeric(provjera)) Then
                        Remi1 = Remi1.Replace(TestChar, "")
                        Nasaop = 1
                        z2str += "*"
                        dodao = 1
                        Continue For
                    End If
                Else
                    Exit For
                End If
            End If
            'provjera daili je upisano integri pa onda * i
            If (TestChar = "*") Then
                provjera = GetChar(TextBox1.Text, sl + value + 1)
                If (IsNumeric(provjera)) Then
                    Remi1 = Remi1.Replace(TestChar, "")
                    Continue For
                Else
                    Exit For
                End If
            End If
            'Provjera dali se dijeli imaginarni broj
            If (TestChar = "/") Then
                If (Nasaop = 1) Then
                    Exit For
                End If
                provjera = GetChar(TextBox1.Text, sl + value + 1)
                If (IsNumeric(provjera)) Then
                    Remi1 = Remi1.Replace(TestChar, "")
                    Nasaop = 1
                    z2str += "/"
                    dodao = 1
                    Continue For
                Else
                    Exit For
                End If
            End If
            If (IsNumeric(TestChar)) Then
                Remi1 = Remi1.Replace(TestChar, "")
                z2str += TestChar
                dodao = 1
                Continue For
            Else
                provjera = GetChar(TextBox1.Text, sl + value)
                Remi1 = Remi1.Replace(TestChar, "")
                If (provjera = "-") Then
                    z2str += "-"
                    Exit For
                End If
                Exit For
            End If
        Next


        Dim leni As Long
        leni = Len(z1str)
        For value As Integer = leni To 1 Step -1
            provjera = GetChar(z1str, value)
            z1strzam += provjera
            Continue For
        Next
        If (dodao = 1) Then
            Label3.Text += z1strzam
            Label3.Text += z2str
        Else
            Label3.Text += "1"
        End If
        z1str = ""
        z2str = ""
        z1strzam = ""
        Nasaop = 0
        dodao = 0
        duzina = Len(TextBox2.Text)
        z1 = TextBox2.Text
        sl = InStr(TextBox2.Text, "i")







        'Ovo ispod je forma ako su prvo brojev pa tek onda I
        For value As Integer = 1 To 20
            Dim check = sl - value
            If (check < 1) Then
                Continue For
            End If
            'Provjera dali se imaginarni broj mnozi sa necim.
            TestChar = GetChar(TextBox2.Text, sl - value)
            If (TestChar = "*") Then
                provjera = GetChar(TextBox2.Text, sl - value - 1)
                If (IsNumeric(provjera)) Then
                    provjera = GetChar(TextBox2.Text, sl - value + 1)
                    If (IsNumeric(provjera)) Then
                        Remi2 = Remi2.Replace(TestChar, "")
                        Nasaop = 1
                        z1str += "*"
                        dodao = 1
                        Continue For
                    End If
                Else
                    Exit For
                End If
            End If

            'provjera daili je upisano integri pa onda * i
            If (TestChar = "*") Then
                provjera = GetChar(TextBox2.Text, sl - value - 1)
                If (IsNumeric(provjera)) Then
                    Remi2 = Remi2.Replace(TestChar, "")
                    Continue For
                Else
                    Exit For
                End If
            End If

            'Provjera dali se dijeli imaginarni broj
            If (TestChar = "/") Then
                If (Nasaop = 1) Then
                    Exit For
                End If
                provjera = GetChar(TextBox2.Text, sl - value - 1)
                If (IsNumeric(provjera)) Then
                    Remi2 = Remi2.Replace(TestChar, "")
                    Nasaop = 1
                    z1str += "/"
                    dodao = 1
                    Continue For
                Else
                    Exit For
                End If
            End If
            If (IsNumeric(TestChar)) Then
                Remi2 = Remi2.Replace(TestChar, "")
                z1str += TestChar
                dodao = 1
            Else
                provjera = GetChar(TextBox2.Text, sl - value)
                Remi2 = Remi2.Replace(TestChar, "")
                If (provjera = "-") Then
                    z1str += "-"
                    Exit For
                Else
                    Exit For
                End If
            End If
        Next
        Nasaop = 0





        'Ovo je sad forma kada je prvo I pa tek onda su brojevi
        For value As Integer = 1 To 20
            Dim check = sl + value
            If (check > duzina) Then
                Continue For
            End If
            TestChar = GetChar(TextBox2.Text, sl + value)
            'Provjera dali se imaginarni broj mnozi sa necim.
            If (TestChar = "*") Then
                provjera = GetChar(TextBox2.Text, sl + value + 1)
                If (IsNumeric(provjera)) Then
                    provjera = GetChar(TextBox2.Text, sl + value - 1)
                    If (IsNumeric(provjera)) Then
                        Remi2 = Remi2.Replace(TestChar, "")
                        Nasaop = 1
                        z2str += "*"
                        dodao = 1
                        Continue For
                    End If
                Else
                    Exit For
                End If
            End If
            'provjera daili je upisano integri pa onda * i
            If (TestChar = "*") Then
                provjera = GetChar(TextBox2.Text, sl + value + 1)
                If (IsNumeric(provjera)) Then
                    Remi2 = Remi2.Replace(TestChar, "")
                    Continue For
                Else
                    Exit For
                End If
            End If
            'Provjera dali se dijeli imaginarni broj
            If (TestChar = "/") Then
                If (Nasaop = 1) Then
                    Exit For
                End If
                provjera = GetChar(TextBox2.Text, sl + value + 1)
                If (IsNumeric(provjera)) Then
                    Remi2 = Remi2.Replace(TestChar, "")
                    Nasaop = 1
                    z2str += "/"
                    dodao = 1
                    Continue For
                Else
                    Exit For
                End If
            End If
            If (IsNumeric(TestChar)) Then
                Remi2 = Remi2.Replace(TestChar, "")
                z2str += TestChar
                dodao = 1
                Continue For
            Else
                provjera = GetChar(TextBox2.Text, sl + value)
                Remi2 = Remi2.Replace(TestChar, "")
                If (provjera = "-") Then
                    z2str += "-"
                    Exit For
                End If
                Exit For
            End If
        Next


        leni = Len(z1str)
        For value As Integer = leni To 1 Step -1
            provjera = GetChar(z1str, value)
            z1strzam += provjera
            Continue For
        Next
        If (dodao = 1) Then
            Label4.Text += z1strzam
            Label4.Text += z2str
        Else
            Label4.Text += "1"
        End If


        Dim lenst = Len(Remi1)
        For value As Integer = lenst To 1 Step -1
            provjera = GetChar(Remi1, value)
            If (provjera = "i") Then
                Remi1 = Remi1.Replace(provjera, "")
                Exit For
            End If
        Next

        lenst = Len(Remi2)
        For value As Integer = lenst To 1 Step -1
            provjera = GetChar(Remi2, value)
            If (provjera = "i") Then
                Remi2 = Remi2.Replace(provjera, "")
                Exit For
            End If
        Next

        Label5.Text += Remi1
        Label6.Text += Remi2

    End Sub
End Class
