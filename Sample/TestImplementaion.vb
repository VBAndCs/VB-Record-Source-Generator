
Namespace MyApp.Tests
    Partial Public Class TestImplementaion
        Default Property Item(i As Integer) As Integer Implements ITest(Of Integer).Item
            Get
                Return 0
            End Get

            Set(value As Integer)

            End Set
        End Property

        Public Property Data(x As Integer) As Integer Implements ITest(Of Integer).Data
            Get
                Return 0
            End Get
            Set(value As Integer)

            End Set
        End Property


    End Class


End Namespace

