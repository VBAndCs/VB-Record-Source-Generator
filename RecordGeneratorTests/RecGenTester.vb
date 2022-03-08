
Imports System.Collections.Immutable
Imports System.Reflection
Imports System.Security.Cryptography
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace RecordGeneratorTests
    <TestClass>
    Public Class RecGenTester

        Private Function GetGeneratedOutput(source As String, additionalFile As String) As (Diagnostics As ImmutableArray(Of Diagnostic), Output As String)

            Dim compilation = GetCompilation(source)

            Dim generator1 As ISourceGenerator = New RecordGenerator.RecGen()

            Dim iaGenerator = {generator1}.ToImmutableArray

            Dim driver = VisualBasicGeneratorDriver.Create(iaGenerator,
                                                       {CType(New MyAdditionalText("__Just_For_Test__.rec", additionalFile), AdditionalText)}.ToImmutableArray,
                                                       Nothing,
                                                       Nothing)

            Dim outputCompilation As Compilation = Nothing
            Dim generateDiagnostics As ImmutableArray(Of Diagnostic) = Nothing
            driver.RunGeneratorsAndUpdateCompilation(compilation, outputCompilation, generateDiagnostics)

            Return (generateDiagnostics, outputCompilation.SyntaxTrees.Last().ToString())

        End Function

        Function GetHash(sourceStr As String) As String
            Dim b = Encoding.ASCII.GetBytes(sourceStr)
            Dim hash = New MD5CryptoServiceProvider().ComputeHash(b)
            Dim sb As New StringBuilder(hash.Length)
            For i = 0 To hash.Length - 1
                sb.Append(hash(i).ToString("X2"))
            Next
            Return sb.ToString()
        End Function

        <TestMethod>
        Public Sub [Imports]()
            Dim TestRecord = <![CDATA[
Imports System.Text
Record TestSB(SB As StringBuilder)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "E241C5FF0BE927480D716AC7A9D50F57")
        End Sub

        <TestMethod>
        Public Sub Delegates()
            Dim TestRecord = <![CDATA[
' Comment Line 1
Public Record Zero()

' Comment Line 2
' Comment Line 3
Public Record TestDelegate(
    Delegate1 As Action, 
    Delegate2 As Func(Of Integer),
    Delegate3 As Action(Of String) = AddressOf MySub,
    Delegate4 As Func(Of Integer, Integer) = Function(x) x + 1,
    Method1 = Function( ) "This is a Function"
)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "7B1D54CEAFC2F7C56EC3C1952EBD9F13")
        End Sub

        <TestMethod>
        Public Sub XMLComments()
            Dim TestRecord = <![CDATA[
''' <summary>
''' Person record type
''' </summary>
''' <param name="FirstName">First Name</param>
''' <param name="LastName">Last Name</param>
''' <remarks>
''' The person type is a positional record containing the
''' properties for the first and last names.
''' </remarks>
Public Record Test(
    FirstName$,
    LastName$
)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "57F5B2171FCAA55326E28289F18E93C8")
        End Sub

        <TestMethod>
        Public Sub ImpInterface()
            Dim TestRecord = <![CDATA[
Record Test(
    Name$,
    Pos As (X%, Y%),
    Age%,
    Move = Sub(x%) _Pos.X = x,
    Move = Function(x%, y%) 
                       _Pos = (x, y)
                       Return True
                  End Function,
    GetText = Function(name$) "Hello " & name,
    GetText = Function( ) "Hello",
    Concat = Function(x$, y$) x & y,
    Concat = Fn(x$, y$, n%) => x & y & n.ToString()
) Implements ITest(Of Integer) , Inherits DateInfo
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "B220D670F79D15B1908AE3B6BAA1BA23")
        End Sub

        <TestMethod>
        Public Sub Lists()
            Dim TestRecord = <![CDATA[
Record Test(
    A As List(Of Byte),
    B As List(Of Byte) = New List(Of Byte)(),
    C = New List(Of Byte)(),
    D As New List(Of Byte) From {1, 2, 3}
)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "D5103F1F7777F1F012C30D97E3302D1D")
        End Sub


        <TestMethod>
        Public Sub PropertyLess()
            Dim TestRecord = <![CDATA[
  
   Namespace MyApp.Tests

Class Zero()
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "4D79620A60F86AC4E76D626C2C685C12")
        End Sub

        <TestMethod>
        Public Sub AsNew()
            Dim TestRecord = <![CDATA[
Public Record Test(Of T1 As New, T2 As New)(
      Items As New List(Of Integer)
)
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "8B5361FFA9C555604EDFDCDB2CF550C7")
        End Sub


        <TestMethod>
        Public Sub Nameless()
            Dim TestRecord = <![CDATA[Public Record (Name ="", Value = 0.0)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            ' Must Fail
            Assert.AreEqual(result.Diagnostics(0).Id, "BC42502", result.Diagnostics(0).ToString())
        End Sub

        <TestMethod>
        Public Sub NameValue()
            Dim TestRecord = <![CDATA[Public Record NameValue(Name ="", Value = 0.0)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "9F69DDF92102595D96E7FFC5BD511778")
        End Sub

        <TestMethod>
        Public Sub Info()
            Dim TestRecord = <![CDATA[Record Info(Of T1, T2, T3 As New)(A As T1, B As T2, C As T3)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "C61264779BA07BAA65FC41A075713C5E")
        End Sub

        <TestMethod>
        Public Sub ROStruct()
            Dim TestRecord = <![CDATA[Public ReadOnly Structure ROStruct(X$, Y%, Z@)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "D7EA876A72D6310B941A5BC53DB79720")
        End Sub

        <TestMethod>
        Public Sub ROClass()
            Dim TestRecord = <![CDATA[Friend ReadOnly Class ROClass(A As Integer, B As Integer)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "23D3E71CA11B1B810AE87A4D7953EF2A")
        End Sub

        <TestMethod>
        Public Sub Author()
            Dim TestRecord = <![CDATA[<MyAttr>
Public Key Class Author(
	 ReadOnly Key ID = 0, 
	ReadOnly Name = "",	
    Books As List(Of Book)
)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "1918D5A56BD6EC33B0E2ADF9D9A25868")
        End Sub

        <TestMethod>
        Public Sub Book()
            Dim TestRecord = <![CDATA[
Public Class Book(
	ReadOnly Key ID%, 
	<MyAttr>ReadOnly Name As String,	
    AuthorID As Integer
) ]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "AF8DA514203BEE612CB91C1CA3292BE6")
        End Sub

        <TestMethod>
        Public Sub Person()
            Dim TestRecord = <![CDATA[
Imports System.Text, System.IO
Imports System.Collections 

Public Class Person(
	Key ID = 0, 
	Name = "", 
    <MyAttr>Address = (City := "", Street := "", No := 0)
) Inherits Test

]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "A17F698F838C21C68B5B3695104C4175")
        End Sub

        <TestMethod>
        Public Sub Student()
            Dim TestRecord = <![CDATA[
Public Record Human(
	ID = 0, 
	Name = "", 
    Address As (City$, Street$, No%)
) Inherits DateInfo

<MyAttr>
Public Record Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
) Inherits Human
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "5AFB8567FB05AD8C579F885749D36D3F")
        End Sub

        <TestMethod>
        Public Sub EnumsAndConsts()
            Dim TestRecord = <![CDATA[
Public Class TestEnums(
    Immutable State = TriState.False,
    ImmutableKey List = new List(Of Integer),
    Immutable Key Value = MyValue
 )]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "CDBF77A523FF4ED2A6A67AA0087C62DA")
        End Sub

        <TestMethod>
        Public Sub ThreeSingleLineRecords()
            Dim TestRecord = <![CDATA[Public Record NameValue(Name ="", Value = 0.0)

Public ReadOnly Structure ROStruct(X$, Y%, Z@)

Friend ReadOnly Class ROClass(A As Integer, B As Integer)

]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next

            Assert.AreEqual(GetHash(result.Output), "23D3E71CA11B1B810AE87A4D7953EF2A")

        End Sub

        <TestMethod>
        Public Sub ThreeMultiLineRecords()
            Dim TestRecord = <![CDATA[
Imports System.Text, System.IO
Imports System.Collections 

Public Class Person(
	Key ID = 0, 
	Name = "", 
    <MyAttr>Address = (City := "", Street := "", No := 0)
) Inherits Test

<MyAttr>
Public Record Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
) Inherits Person

<MyAttr>Public Class UniStudent(
    University As String,
    Collage As String,
    Print = Function() $"{Name}, {University}, {Collage}"
) Inherits Student
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next

            Assert.AreEqual(GetHash(result.Output), "16027E70DBC3442A862FF2D1753932CD")

        End Sub

        <TestMethod>
        Public Sub TwoMultiLineRecords()
            Dim TestRecord = <![CDATA[<MyAttr>
Public Key Class Author(
	 ReadOnly Key ID = 0, 
	ReadOnly Name = "",	
    Books As List(Of Book)
)

Public Class Book(
	ReadOnly Key ID%, 
	<MyAttr>ReadOnly Name As String,	
    AuthorID As Integer
) 
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next

            Assert.AreEqual(GetHash(result.Output), "AF8DA514203BEE612CB91C1CA3292BE6")

        End Sub

        <TestMethod>
        Public Sub TestValueTypes()
            Dim TestRecord = <![CDATA[
Imports Microsoft.VisualBasic

Record TestStructs(
    A As MyStruct,
    B As MyStruct?,
    C  As MyStruct = new MyStruct(),
    D = New MyStruct() With {.Value = 1}
)

Record TestEums(
    A As TriState,
    B As TriState?,
    C = TriState.True,
    D  As TriState = TriState.True,
)

Record TestTuples(
    A As System.ValueTuple,
    B As System.ValueTuple?,
    C As (Integer, String) = (1, "abc"),
    D = (1, "abc"),
    E = (ID:= 1, Value:= "abc")
)
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next

            Assert.AreEqual(GetHash(result.Output), "5A69D7408D6BAA15A58AC12DA6E5C56C")

        End Sub

    End Class
End Namespace