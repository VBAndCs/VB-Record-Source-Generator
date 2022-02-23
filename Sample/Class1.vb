Namespace MyApp.Test

    Imports System

    Dim x As Integer
    ? "Hellow world"

    Function Foo(a As Integer) => a + 1

    Class Test
        Public Value As Integer
    End Class

    Structure Student As(
       ID%, Name$, 
       Addresss As (City$, Street$, No%),
       Print = Sub() Console.WriteLine($"{ID}-{Name}-address")
    )