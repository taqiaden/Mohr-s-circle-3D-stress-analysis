Option Strict On 'هاذا الخيار يلزم اختيار نوع التحويل عند ادخال قيم المتغيرات
Public Class Form3
    Dim x, y, z, xy, xz, yz, I1, I2, I3, S, Q, R, T, alpha, x1, x2, x3, r1, r2, r3, a, b, c, k As Double
    Public Const pi As Double = Math.PI

    Private Sub Form3_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.Opacity = 0.95

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged

    End Sub
    Private Function unit(ByVal cu As String) As Double
        Dim u1 As Double
        If CStr(cu) = "MPa" Then u1 = 1000000
        If CStr(cu) = "KPa" Then u1 = 1000
        If CStr(cu) = "Pa" Then u1 = 1
        If CStr(cu) = "Ksi" Then u1 = 6.895
        If CStr(cu) = "psi" Then u1 = 6895
        If CStr(cu) = "Degree" Then u1 = pi / 180
        If CStr(cu) = "Radian" Then u1 = 1
        Return u1

    End Function 'هذا الجزء خاص بتحويل وتعريف الوحدات
    Public Sub calculations()
        x = Convert.ToDouble(TextBox1.Text) * unit(ComboBox1.Text)
        y = Convert.ToDouble(TextBox2.Text) * unit(ComboBox2.Text)
        z = Convert.ToDouble(TextBox3.Text) * unit(ComboBox3.Text)
        xy = Convert.ToDouble(TextBox4.Text) * unit(ComboBox4.Text)
        xz = Convert.ToDouble(TextBox5.Text) * unit(ComboBox5.Text)
        yz = Convert.ToDouble(TextBox6.Text) * unit(ComboBox6.Text)
        I1 = x + y + z
        I2 = (x * y * z) + (2 * xy * yz * xz) - (x * (yz ^ 2)) - (y * (xz ^ 2)) - (z * (xy ^ 2))
        I3 = (x * y) + (x * z) + (y * z) - (xy ^ 2) - (yz ^ 2) - (xz ^ 2)
        R = ((I1 ^ 2) / 3) - I2
        Q = ((I1 * I2) / 3) - I3 - ((2 * (I1 ^ 3)) / 27)
        T = Math.Sqrt((R ^ 3) / 27)
        alpha = Math.Acos((-Q) / (2 * T))
        S = Math.Sqrt(R / 3)
        x1 = (2 * S * (Math.Cos(alpha / 3))) + (I1 / 3)
        x2 = (2 * S * (Math.Cos((alpha / 3) + ((2 * pi)) / 3))) + (I1 / 3)
        x3 = (2 * S * (Math.Cos((alpha / 3) + ((4 * pi)) / 3))) + (I1 / 3)
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
        TextBox7.Text = CStr(r1)
        TextBox8.Text = CStr(r2)
        TextBox9.Text = CStr(r3)
        TextBox_p1x.Text = CStr(l(r1) / unit(theta_p1_y.Text))
        TextBox_p1y.Text = CStr(m(r1) / unit(theta_p1_y.Text))
        TextBox_p1z.Text = CStr(n(r1) / unit(theta_p1_z.Text))
        TextBox_p2x.Text = CStr(l(r2) / unit(theta_p2_x.Text))
        TextBox_p2y.Text = CStr(m(r2) / unit(theta_p2_y.Text))
        TextBox_p2z.Text = CStr(n(r2) / unit(theta_p2_z.Text))
        TextBox_p3x.Text = CStr(l(r3) / unit(theta_p3_x.Text))
        TextBox_p3y.Text = CStr(m(r3) / unit(theta_p3_y.Text))
        TextBox_p3z.Text = CStr(n(r3) / unit(theta_p3_z.Text))

    End Sub 'هذا الجزء خاص بالعمليات الحسابية
    Public Function l(ByVal segma_p As Double) As Double
        a = ((y - segma_p) * (z - segma_p)) - (yz ^ 2)
        k = 1 / (Math.Sqrt(a ^ 2 + b ^ 2 + c ^ 2))
        Return Math.Acos(a * k)

    End Function
    Public Function m(ByVal segma_p As Double) As Double
        b = -((xy * (z - segma_p)) - (xz * yz))
        k = 1 / (Math.Sqrt(a ^ 2 + b ^ 2 + c ^ 2))
        Return Math.Acos(b * k)

    End Function
    Public Function n(ByVal segma_p As Double) As Double
        c = (xy * yz) - (xz * (y - segma_p))
        k = 1 / (Math.Sqrt(a ^ 2 + b ^ 2 + c ^ 2))
        Return Math.Acos(c * k)
    End Function

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        On Error GoTo x
        calculations()
        GoTo y
x:
        MsgBox("الرجاء التأكد من صحة المدخلات وانها قابله للحل في اطار الاعداد الحقيقية")

y:

    End Sub 'عند النقر على زر النتيجة

    Private Sub التحليلفيمستوىToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles التحليلفيمستوىToolStripMenuItem.Click

        Form1.Show()
        Hide()
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        On Error GoTo x
        calculations()


x:
    End Sub 'عمليات المؤقت الزمني


End Class