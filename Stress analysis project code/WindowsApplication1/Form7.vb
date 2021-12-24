Public Class Form7
    Dim after, before As Integer
    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick


        If after = 200 Then
            GoTo w
        End If
        If Me.Opacity = 1 Then
            If after = 200 Then
                GoTo w
            Else
                after += 1
                GoTo e
            End If

        End If

        Me.Opacity += 0.03

        GoTo e

w:
        If Me.Opacity = 0 Then

            Me.Close()
        Else
            Me.Opacity -= 0.03
        End If
e:
    End Sub

    
   
End Class