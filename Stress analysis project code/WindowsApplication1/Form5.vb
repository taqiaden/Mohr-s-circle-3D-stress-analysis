'برمجة تقي الدين احمد عبدالله احمد الشميري
'Programmed by : Taqi-Aden A. Abdullah Al-Shameri
'contact me on facebook : www.fb.com\taqiaden
'Yemen-Sana,a 
't: +967735795486
'Enjoy programming ^_^

Public Class d3


    Const pi As Double = Math.PI
    Dim x, y, z, xy, xz, yz, I1, I2, I3, S, Q, R, T, alpha, x1, x2, x3, r1, r2, r3, a, b, c, v, k, l1, m1, n1, transform_segma As Double
    Dim lr1, lr2, lr3, lr4, lr5, lr6, lr11, lr22, lr33, lr44, lr55, lr66 As String
    Dim process_x, process_y As Double
    Dim mouse As String
    Dim plane As String
    Dim reload As Integer
    Dim avs, theta1, theta2 As Double 'extra variables for 2d analysis
    'Dim sc As Double 'scroll bar control
    Private Function unit(ByVal cu As Object) As Double

        Dim u1 As Double
        If CStr(cu) = "MPa" Then u1 = 1000000
        If CStr(cu) = "KPa" Then u1 = 1000
        If CStr(cu) = "Pa" Then u1 = 1
        If CStr(cu) = "Ksi" Then u1 = (6895000)
        If CStr(cu) = "Psi" Then u1 = 6895
        If CStr(cu) = "Degree" Then u1 = (pi / 180)
        If CStr(cu) = "Radian" Then u1 = 1
        If CStr(cu) = "Psf" Then u1 = (6895 * 12)
        If CStr(cu) = "Ksf" Then u1 = (6895000 * 12)
        Return u1

    End Function 'هذا الجزء خاص بتحويل وتعريف الوحدات
    Private Sub recalculate()
        principle1.Text = ""
        principle2.Text = ""
        principle3.Text = ""
        TextBox_p1x.Text = ""
        TextBox_p1y.Text = ""
        TextBox_p1z.Text = ""
        TextBox_p2x.Text = ""
        TextBox_p2y.Text = ""
        TextBox_p2z.Text = ""
        TextBox_p3x.Text = ""
        TextBox_p3y.Text = ""
        TextBox_p3z.Text = ""
        tmax.Text = ""
        tmax12.Text = ""
        tmax23.Text = ""
    End Sub
    Private Sub mohar()
        On Error GoTo e
       
        Dim scale As Integer = CInt(Math.Abs((r1 - r3) / (PictureBox1.Height - 60)))
        Dim mohar_circle As Graphics
        mohar_circle = PictureBox1.CreateGraphics
        Dim pencolor As New Pen(Color.White)
        Dim pencolor1 As New Pen(Color.Red)
        Dim pencolor2 As New Pen(Color.Yellow)
        Dim pencolor3 As New Pen(Color.Blue)
        pencolor1.Width = 2.5
        pencolor2.Width = 2.5
        pencolor3.Width = 2.5

        Dim P, p11, p22, l1 As Point
        Dim s, s11, s22 As Size
        Dim c1, c2, c3 As Double
        Dim pendash As New Pen(Brushes.White)
        Dim textfont As New Font("verdana", 9, FontStyle.Regular)

        s.Width = (r1 - r3) / scale
        P.X = 280 + (r3 / scale)

        Dim c As Double
        If s.Width + P.X >= 550 Then
            c = 200 - (s.Width + P.X - 560)
        ElseIf P.X <= 10 Then
            c = 350 + (Math.Abs(P.X))
        Else
            c = 280
        End If
        v = 175

        l1.X = c
        s.Width = (r1 - r3) / scale
        s11.Width = (r2 - r3) / scale
        s22.Width = (r1 - r2) / scale

        P.X = c + r3 / scale
        P.Y = v - ((r1 - r3) / 2) / scale
        p11.X = c + r3 / scale
        p11.Y = v - ((r2 - r3) / 2) / scale
        p22.X = c + r2 / scale
        p22.Y = v - ((r1 - r2) / 2) / scale

        c1 = P.X + (s.Width / 2)
        c2 = p11.X + (s11.Width / 2)
        c3 = p22.X + (s22.Width / 2)

        Dim mm As Graphics
        mm = PictureBox1.CreateGraphics
        mm.FillRectangle(Brushes.Black, 2, 348, 400, 20)
        Dim mousex, mousey As Double
        mousex = CDbl(MousePosition.X.ToString())
        mousey = CDbl(MousePosition.Y.ToString())
        If (mousex - CDbl(Me.Location.X) - 8 - CDbl(GroupBox3.Location.X) - CDbl(PictureBox1.Location.X)) > 0 And (mousex - CDbl(Me.Location.X) - 8 - CDbl(GroupBox3.Location.X) - CDbl(PictureBox1.Location.X)) < 560 And (mousey - CDbl(Me.Location.Y) - 31 - CDbl(GroupBox3.Location.Y) - CDbl(PictureBox1.Location.Y)) > 0 And (mousey - CDbl(Me.Location.Y) - 31 - CDbl(GroupBox3.Location.Y) - CDbl(PictureBox1.Location.Y)) < 365 Then

            process_x = (mousex - CDbl(Me.Location.X) - 8 - CDbl(GroupBox3.Location.X) - CDbl(PictureBox1.Location.X) - l1.X) * scale / 1000000
            process_y = (mousey - CDbl(Me.Location.Y) - 31 - CDbl(GroupBox3.Location.Y) - CDbl(PictureBox1.Location.Y) - v) * -1 * scale / 1000000
            mouse = "σ=" + CStr(process_x) + " MPa ," + "Ʈ=" + CStr(process_y) + " MPa"
            mm.DrawString(mouse, textfont, Brushes.White, 2, 348)
            GoTo norefersh
            
        End If

       

        If TextBox1.Text = lr1 And TextBox2.Text = lr2 And TextBox3.Text = lr3 And TextBox4.Text = lr4 And TextBox5.Text = lr5 And TextBox6.Text = lr6 And ComboBox1.Text = lr11 And ComboBox2.Text = lr22 And ComboBox3.Text = lr33 And ComboBox4.Text = lr44 And ComboBox5.Text = lr55 And ComboBox6.Text = lr66 Then
            GoTo a
        End If

        On Error GoTo ex
        lr1 = TextBox1.Text
        lr2 = TextBox2.Text
        lr3 = TextBox3.Text
        lr4 = TextBox4.Text
        lr5 = TextBox5.Text
        lr6 = TextBox6.Text
        lr11 = ComboBox1.Text
        lr22 = ComboBox2.Text
        lr33 = ComboBox3.Text
        lr44 = ComboBox4.Text
        lr55 = ComboBox5.Text
        lr66 = ComboBox6.Text
ex:

        PictureBox1.Refresh()
norefersh:
        mohar_circle.FillEllipse(Brushes.DarkSlateGray, P.X, P.Y, s.Width, s.Width)
        mohar_circle.FillEllipse(Brushes.Black, p11.X, p11.Y, s11.Width, s11.Width)
        mohar_circle.FillEllipse(Brushes.Black, p22.X, p22.Y, s22.Width, s22.Width)

        mohar_circle.DrawLine(pencolor, 2, 175, 558, 175)
        mohar_circle.DrawLine(pencolor, l1.X, 2, l1.X, 348)
        mohar_circle.DrawEllipse(pencolor3, p11.X, p11.Y, s11.Width, s11.Width)
        mohar_circle.DrawEllipse(pencolor2, p22.X, p22.Y, s22.Width, s22.Width)
        mohar_circle.DrawEllipse(pencolor1, P.X, P.Y, s.Width, s.Width)


        pendash.DashStyle = Drawing2D.DashStyle.Dash
        mohar_circle.DrawLine(pendash, CInt(c1), 175, CInt(c1), CInt(175 - (s.Width / 2)))
        mohar_circle.DrawLine(pendash, CInt(c2), 175, CInt(c2), CInt(175 - (s11.Width / 2)))
        mohar_circle.DrawLine(pendash, CInt(c3), 175, CInt(c3), CInt(175 - (s22.Width / 2)))


        mohar_circle.DrawString("σ3", textfont, Brushes.White, (P.X - 20), 158)
        mohar_circle.DrawString("σ2", textfont, Brushes.White, (p22.X - 20), 158)
        mohar_circle.DrawString("σ1", textfont, Brushes.White, (P.X + s.Width + 1), 158)
        mohar_circle.DrawString("Ʈ(13)max", textfont, Brushes.White, (c1 - 25), (155 - (s.Width / 2)))
        mohar_circle.DrawString("Ʈ(23)", textfont, Brushes.White, (c2 - 15), (155 - (s11.Width / 2)))
        mohar_circle.DrawString("Ʈ(12)", textfont, Brushes.White, (c3 - 15), (155 - (s22.Width / 2)))


        GoTo a
e:
        PictureBox1.Refresh()
        lr1 = "taqi-design"

a:

    End Sub
    Public Sub calculations()
      
        I1 = x + y + z
        I2 = (x * y) + (x * z) + (y * z) - (xy ^ 2) - (yz ^ 2) - (xz ^ 2)
        I3 = (x * y * z) + (2 * xy * yz * xz) - (x * (yz ^ 2)) - (y * (xz ^ 2)) - (z * (xy ^ 2))
        R = ((I1 ^ 2) / 3) - I2
        Q = ((I1 * I2) / 3) - I3 - ((2 * (I1 ^ 3)) / 27)
        T = Math.Sqrt((R ^ 3) / 27)
        alpha = Math.Acos((-Q) / (2 * T))
        S = Math.Sqrt(R / 3)
        x1 = (2 * S * (Math.Cos(alpha / 3))) + (I1 / 3)
        x2 = (2 * S * (Math.Cos((alpha / 3) + (((2 * pi)) / 3)))) + (I1 / 3)
        x3 = (2 * S * (Math.Cos((alpha / 3) + (((4 * pi)) / 3)))) + (I1 / 3)
        If (x1 >= x2 And x2 >= x3) Then
            r1 = x1
            r2 = x2
            r3 = x3
        End If
        If (x1 >= x3 And x3 >= x2) Then
            r1 = x1
            r2 = x3
            r3 = x2
        End If
        If (x2 >= x3 And x3 >= x1) Then
            r1 = x2
            r2 = x3
            r3 = x1
        End If

        If (x2 >= x1 And x1 >= x3) Then
            r1 = x2
            r2 = x1
            r3 = x3
        End If

        If (x3 >= x2 And x2 >= x1) Then
            r1 = x3
            r2 = x2
            r3 = x1
        End If
        If (x3 >= x1 And x1 >= x2) Then
            r1 = x3
            r2 = x1
            r3 = x2
        End If
        principle1.Text = (r1 / unit(compo_p1.Text))
        principle2.Text = (r2 / unit(compo_p2.Text))
        principle3.Text = (r3 / unit(compo_p3.Text))
        TextBox_p1x.Text = (l(r1) / unit(theta_p1_x.Text))
        TextBox_p1y.Text = (m(r1) / unit(theta_p1_y.Text))
        TextBox_p1z.Text = (n(r1) / unit(theta_p1_z.Text))
        TextBox_p2x.Text = (l(r2) / unit(theta_p2_x.Text))
        TextBox_p2y.Text = (m(r2) / unit(theta_p2_y.Text))
        TextBox_p2z.Text = (n(r2) / unit(theta_p2_z.Text))
        TextBox_p3x.Text = (l(r3) / unit(theta_p3_x.Text))
        TextBox_p3y.Text = (m(r3) / unit(theta_p3_y.Text))
        TextBox_p3z.Text = (n(r3) / unit(theta_p3_z.Text))

        tmax.Text = (((r1 - r3) / 2) / unit(tmax_u.Text))
        tmax12.Text = (((r1 - r2) / 2) / unit(tmax12u.Text))
        tmax23.Text = (((r2 - r3) / 2) / unit(tmax23u.Text))


    End Sub 'هذا الجزء خاص بالعمليات الحسابية
    Public Function l(ByVal segma_p As Double) As Double
        a = ((y - segma_p) * (z - segma_p)) - (yz ^ 2)
        b = -((xy * (z - segma_p)) - (xz * yz))
        c = (xy * yz) - (xz * (y - segma_p))
        k = 1 / (Math.Sqrt(a ^ 2 + b ^ 2 + c ^ 2))
        Return Math.Acos(a * k)

    End Function
    Public Function error_detection(ByVal test As Object) As Object
        Dim er As Double
        If Not (test.Text = "") Then
            On Error GoTo a
            er = CDbl(test.Text)
            If Not (test.Text = 0) Then
                test.BackColor = Color.WhiteSmoke

            Else
                test.BackColor = Color.PeachPuff
            End If
            GoTo z
a:          If Not test.BackColor = Color.Red Then
                Beep()
                recalculate()
            End If
            test.BackColor = Color.Red
            GoTo z
a1:         test.BackColor = Color.PeachPuff
z:
        Else
            test.BackColor = Color.PeachPuff
        End If

    End Function
    Public Function m(ByVal segma_p As Double) As Double
        a = ((y - segma_p) * (z - segma_p)) - (yz ^ 2)
        b = -((xy * (z - segma_p)) - (xz * yz))
        c = (xy * yz) - (xz * (y - segma_p))
        k = 1 / (Math.Sqrt(a ^ 2 + b ^ 2 + c ^ 2))
        Return Math.Acos(b * k)

    End Function
    Public Function n(ByVal segma_p As Double) As Double
        a = ((y - segma_p) * (z - segma_p)) - (yz ^ 2)
        b = -((xy * (z - segma_p)) - (xz * yz))
        c = (xy * yz) - (xz * (y - segma_p))
        k = 1 / (Math.Sqrt(a ^ 2 + b ^ 2 + c ^ 2))
        Return Math.Acos(c * k)
    End Function

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        
        'wrong interance detection
        error_detection(TextBox1)
        error_detection(TextBox2)
        error_detection(TextBox3)
        error_detection(TextBox4)
        error_detection(TextBox5)
        error_detection(TextBox6)

        'these codes indicate that when field is empty then its equal to zero""=0
        On Error GoTo x
        If CStr(TextBox1.Text) = "" Then x = 0 Else x = Convert.ToDouble(TextBox1.Text) * unit(ComboBox1.Text)
        If CStr(TextBox2.Text) = "" Then y = 0 Else y = Convert.ToDouble(TextBox2.Text) * unit(ComboBox2.Text)
        If CStr(TextBox3.Text) = "" Then z = 0 Else z = Convert.ToDouble(TextBox3.Text) * unit(ComboBox3.Text)
        If CStr(TextBox4.Text) = "" Then xy = 0 Else xy = Convert.ToDouble(TextBox4.Text) * unit(ComboBox4.Text)
        If CStr(TextBox5.Text) = "" Then xz = 0 Else xz = Convert.ToDouble(TextBox5.Text) * unit(ComboBox5.Text)
        If CStr(TextBox6.Text) = "" Then yz = 0 Else yz = Convert.ToDouble(TextBox6.Text) * unit(ComboBox6.Text)

        'no interance detection
        If x = 0 And y = 0 And z = 0 And xy = 0 And xz = 0 And yz = 0 Then
            Me.Height = 463
            If givinfield.BackColor = Color.RoyalBlue Then
                givinfield.BackColor = Color.Chocolate
            Else
                givinfield.BackColor = Color.RoyalBlue
            End If
            PictureBox1.Refresh()
            recalculate()
            sign1.Show()
            sign2.Show()
            GoTo z
        Else
            sign1.Hide()
            sign2.Hide()
        End If

        '2d detection and analysis
        If z = 0 And xz = 0 And yz = 0 Then
            '(process in xy plane)
            x = x
            y = y
            xy = xy
            plane = "xy"
            GoTo two_d
        End If
        If y = 0 And xy = 0 And yz = 0 Then
            '(process in xz plane)
            x = x
            y = z
            xy = xz
            plane = "xz"
            GoTo two_d
        End If
        If x = 0 And xz = 0 And xy = 0 Then
            '(process in yz plane)
            x = y
            y = z
            xy = yz
            plane = "yz"
            GoTo two_d
        End If
        GoTo three_d
two_d:
        Me.Height = 507
        recalculate()
        calculation_2d()
        mohar_2d()
        GoTo z
        'end 2d process

three_d:
        Me.Height = 622
        calculations()
        mohar()





        If xy = 0 And xz = 0 And yz = 0 Then

            TextBox_p1x.Text = ""
            TextBox_p1y.Text = ""
            TextBox_p1z.Text = ""
            TextBox_p2x.Text = ""
            TextBox_p2y.Text = ""
            TextBox_p2z.Text = ""
            TextBox_p3x.Text = ""
            TextBox_p3y.Text = ""
            TextBox_p3z.Text = ""

        End If
        GoTo z

x:

        PictureBox1.Refresh()
        lr1 = "taqi-design"
z:

    End Sub

    Private Sub d3_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.Height = 463
        Me.Opacity = 0.95
    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        Dim my_link_address = "http://www.facebook.com/taqiaden?"
        Process.Start(my_link_address)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        Form6.Show()

    End Sub
   
    
    Private Sub calculation_2d()
        R = Math.Sqrt((((x - y) / 2) ^ 2) + (xy ^ 2))
        avs = ((x + y) / 2)
        principle1.Text = ((avs + R) / unit(compo_p1.Text))
        principle2.Text = ((avs - R) / unit(compo_p2.Text))
        tmax.Text = (R / unit(tmax_u.Text))
        r1 = principle1.Text * unit(compo_p1.Text)
        r2 = principle2.Text * unit(compo_p2.Text)
        principle3.Text = 0
        theta1 = (Math.Atan((2 * xy) / (x - y))) / 2
        theta2 = theta1 + (pi / 2)
        If x < y Then

            theta2 = (Math.Atan(2 * xy / (x - y))) / 2
            theta1 = theta2 + (pi / 2)
        End If
        
        'maxshearangle = (Math.Atan(0 - ((x - y) / (2 * xy)))) / 2
        'TextBox12.Text = (maxshearangle / unit(ComboBox12.Text))
        'TextBox10.Text = (theta1 / unit(ComboBox10.Text))
        'TextBox11.Text = (theta2 / unit(ComboBox11.Text))
    End Sub
    Private Sub mohar_2d()
        On Error GoTo e

        ''scrollbar
        'Dim ns As Double
        'sc = sc + 0
        'If Not (ns = sc) Then
        '    PictureBox1.Refresh()
        'End If
        'ns = sc

        Dim scale As Double = Math.Abs((r1 - r2) / (PictureBox1.Height - 60))
        Dim mohar_circle As Graphics
        mohar_circle = PictureBox1.CreateGraphics

        Dim pencolor As New Pen(Color.White)
        Dim pencolor1 As New Pen(Color.Red)
       

        pencolor1.Width = 2.5
       
        Dim P, l1, point1, point2 As Point
        Dim s As Size
        Dim c1 As Double
        Dim pendash As New Pen(Brushes.White)
        Dim textfont As New Font("verdana", 9, FontStyle.Regular)


        s.Width = (r1 - r2) / scale
        P.X = 280 + (r2 / scale)

        Dim c As Double
        If s.Width + P.X >= 550 Then
            c = 200 - (s.Width + P.X - 560)
        ElseIf P.X <= 10 Then
            c = 350 + (Math.Abs(P.X))
        Else
            c = 280
        End If
        v = 175

        point1.X = c + x / scale
        point1.Y = v + xy / scale
        point2.X = c + y / scale
        point2.Y = v - xy / scale

        l1.X = c
        s.Width = (r1 - r2) / scale
     
        P.X = c + r2 / scale
        P.Y = v - ((r1 - r2) / 2) / scale
      
        c1 = P.X + (s.Width / 2)
   
        Dim mm As Graphics
        mm = PictureBox1.CreateGraphics
        mm.FillRectangle(Brushes.Black, 2, 348, 400, 20)
        Dim mousex, mousey As Double
        mousex = CDbl(MousePosition.X.ToString())
        mousey = CDbl(MousePosition.Y.ToString())
        If (mousex - CDbl(Me.Location.X) - 8 - CDbl(GroupBox3.Location.X) - CDbl(PictureBox1.Location.X)) > 0 And (mousex - CDbl(Me.Location.X) - 8 - CDbl(GroupBox3.Location.X) - CDbl(PictureBox1.Location.X)) < 560 And (mousey - CDbl(Me.Location.Y) - 31 - CDbl(GroupBox3.Location.Y) - CDbl(PictureBox1.Location.Y)) > 0 And (mousey - CDbl(Me.Location.Y) - 31 - CDbl(GroupBox3.Location.Y) - CDbl(PictureBox1.Location.Y)) < 365 Then

            process_x = (mousex - CDbl(Me.Location.X) - 8 - CDbl(GroupBox3.Location.X) - CDbl(PictureBox1.Location.X) - l1.X) * scale / 1000000
            process_y = (mousey - CDbl(Me.Location.Y) - 31 - CDbl(GroupBox3.Location.Y) - CDbl(PictureBox1.Location.Y) - v) * -1 * scale / 1000000
            mouse = "σ=" + CStr(process_x) + " MPa ," + "Ʈ=" + CStr(process_y) + " MPa"
            mm.DrawString(mouse, textfont, Brushes.White, 2, 348)
            GoTo norefersh

        End If


        'entrance change detection codes
        If TextBox1.Text = lr1 And TextBox2.Text = lr2 And TextBox3.Text = lr3 And TextBox4.Text = lr4 And TextBox5.Text = lr5 And TextBox6.Text = lr6 And ComboBox1.Text = lr11 And ComboBox2.Text = lr22 And ComboBox3.Text = lr33 And ComboBox4.Text = lr44 And ComboBox5.Text = lr55 And ComboBox6.Text = lr66 Then
            GoTo a
        End If

        On Error GoTo ex
        lr1 = TextBox1.Text
        lr2 = TextBox2.Text
        lr3 = TextBox3.Text
        lr4 = TextBox4.Text
        lr5 = TextBox5.Text
        lr6 = TextBox6.Text
        lr11 = ComboBox1.Text
        lr22 = ComboBox2.Text
        lr33 = ComboBox3.Text
        lr44 = ComboBox4.Text
        lr55 = ComboBox5.Text
        lr66 = ComboBox6.Text
ex:

        PictureBox1.Refresh()
norefersh:
       

        mohar_circle.FillEllipse(Brushes.Transparent, P.X, P.Y, s.Width, s.Width)
       
        mohar_circle.DrawLine(pencolor, 2, 175, 558, CInt(v))
        mohar_circle.DrawLine(pencolor, l1.X, 2, l1.X, 348)
        mohar_circle.DrawLine(pencolor, point1, point2)
       
      
        mohar_circle.DrawEllipse(pencolor1, P.X, P.Y, s.Width, s.Width)


        pendash.DashStyle = Drawing2D.DashStyle.Dash
        mohar_circle.DrawLine(pendash, CInt(c1), CInt(v), CInt(c1), CInt(v - (s.Width / 2)))

        mohar_circle.FillEllipse(Brushes.GreenYellow, CInt(point1.X - 5), CInt(point1.Y - 5), 10, 10)
        mohar_circle.FillEllipse(Brushes.GreenYellow, CInt(point2.X - 5), CInt(point2.Y - 5), 10, 10)

        Dim rr As Rectangle
        rr.X = P.X + 70
        rr.Y = P.Y + 70
        rr.Height = s.Width - 140
        rr.Width = s.Width - 140
        theta1 = theta1 * 180 / pi
        Dim arccolor As New Pen(Color.LawnGreen)
        arccolor.Width = 2
        mohar_circle.DrawArc(arccolor, rr, 0, CSng(2 * theta1))
        rr.X = P.X + 120
        rr.Y = P.Y + 120
        rr.Height = s.Width - 240
        rr.Width = s.Width - 240
        theta1 = theta1 + 45
        arccolor.Color = Color.Blue
        mohar_circle.DrawArc(arccolor, rr, -90, CSng(2 * theta1))



        mohar_circle.DrawString("σ2", textfont, Brushes.White, (P.X - 20), 158)

        mohar_circle.DrawString("σ1", textfont, Brushes.White, (P.X + s.Width + 1), 158)
        mohar_circle.DrawString("Ʈmax", textfont, Brushes.White, (c1 - 25), (155 - (s.Width / 2)))

        mohar_circle.DrawString(plane + " plane Analysis", textfont, Brushes.White, 2, 2)

        GoTo a
e:
        PictureBox1.Refresh()
        lr1 = "taqi-design"

a:

    End Sub

    'tasks:
    'correct the cosin function
    'make go invent group on facebook
    'auto hide priciple 3 and angles
    'make abrviation for numbers for example 3.00015 = 3
    'add list menu bar and the chance to save mohrs circle as a photo
    'add angle detection for 2d analysis
    'needs codes to resize the form during analysis
    'make given fields group change color or any thing to bring attention when no interance exists in the fields
    'correct the mistakes in angles
    'Add save mohr's circle as a photo option
    'add octahdral stress as extended options
    'in 2d analyzing make it imposiple to clic at a specific angle in graph and then calculate stress at that point
    'correct angle sign or lift them in cosines 
    'add failure theories
    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        Form6.Show()
    End Sub

    Private Sub PictureBox2_Click(sender As Object, e As EventArgs) Handles PictureBox2.Click

    End Sub
End Class