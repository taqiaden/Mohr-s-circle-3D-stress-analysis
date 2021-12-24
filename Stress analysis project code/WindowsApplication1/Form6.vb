Public Class Form6


    Private Sub PictureBox8_Click(sender As Object, e As EventArgs) Handles PictureBox8.Click
        Dim my_link_address = "http://www.fb.com/taqiaden?"
        Process.Start(my_link_address)
    End Sub

    Private Sub figure_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/yahia.alsamawi"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox3_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/profile.php?id=100002283540025"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/haitham.alselwi"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox4_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/profile.php?id=100004962003356&ref=ts&fref=ts"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox5_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/ak.ve.5"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox6_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/profile.php?id=100004406412596"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox7_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/haythem.taher.5"
        Process.Start(my_link_address)
    End Sub

    Private Sub PictureBox2_Click(sender As Object, e As EventArgs)
        Dim my_link_address = "https://www.facebook.com/monthershamsaddin"
        Process.Start(my_link_address)
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        If Me.Opacity = 1 Then
           
            GoTo w
        End If

        Me.Opacity += 0.03



w:
    End Sub
End Class