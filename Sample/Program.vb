Imports System

Module Program
    Sub Main(args As String())
        Dim Adam As New Studnt(1, "Adam", 80.5, ("New Yourk", "Hello St.", 10))
        Console.WriteLine(Adam)

        Dim John = Adam.With(ID:=2, Name:="John")
        Console.WriteLine(John)

        Dim Mark = Adam.WithID(3).WithName("Mark").WithGrades(90)
        Console.WriteLine(Mark)  

    End Sub
End Module
