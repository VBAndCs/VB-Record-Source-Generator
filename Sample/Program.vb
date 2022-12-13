
Module Program
    Public Const MyValue As Integer = 1

    Sub Main(args As String())
        Dim auth1 As New Author(iD:=1, name:="Author1") With {
            .Books = New List(Of Book) From {
                New Book(iD:=1, name:="Book1") With {.AuthorID = 1},
                New Book() With {.AuthorID = 1}.WithID(2).WithName("Book2")
            }
        }

        Console.WriteLine(auth1)

        Dim Adam As New Student(
            Now, 1, "Adam",
            ("New Yourk", "Hello St.", 10),
            3, 80.5)
        Console.WriteLine(Adam)

        Dim John = Adam.With(iD:=2, name:="John")
        Console.WriteLine(John)

        Dim Mark = Adam.WithID(3).WithName("Mark").WithGrades(90)
        Console.WriteLine(Mark)


        Console.WriteLine(Mark.Print)

        Dim Mohmmad = New UniStudent().
            WithName("Mohmmad").
            WithCollage("Engineering").
            WithUniversity("Cairo University")

        Console.WriteLine(Mohmmad.Print)


        Try
            Dim auth2 As New Author(iD:=1, name:="Author1")
            Dim authors As New Dictionary(Of Author, String) From {
                {auth1, "test1"},
                {auth2, "test2"}
            }
            Console.WriteLine(authors.Count)

            Dim p1 As New Person(iD:=1, name:="Ali")
            Dim p2 As New Person(iD:=1, name:="John")
            ' The 2 persons has the same key, and the dictionary should refuse to add them both
            Dim persons As New Dictionary(Of Person, String) From {
                {p1, "test3"},
                {p2, "test4"}
            }
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try

    End Sub

End Module


