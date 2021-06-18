Module Program

    

    Public Const MyValue As Integer = 1


    Sub Main(args As String())

        Dim Adam As New Student(Now, 1, "Adam", ("New Yourk", "Hello St.", 10), 3, 80.5)
        Console.WriteLine(Adam)

        Dim John = Adam.With(ID:=2, Name:="John")
        Console.WriteLine(John)

        Dim Mark = Adam.WithID(3).WithName("Mark").WithGrades(90)
        Console.WriteLine(Mark)


        Console.WriteLine(Mark.Print)

        Dim Mohmmad = New UniStudent().
            WithName("Mohmmad").
            WithCollage("Engineering").
            WithUniversity("Cairo University")

        Console.WriteLine(Mohmmad.Print)
    End Sub

End Module

Public Class Test
    Public Property [Date] As Date
End Class
