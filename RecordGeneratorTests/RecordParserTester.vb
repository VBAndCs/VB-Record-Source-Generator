Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace RecordGeneratorTests
    <TestClass>
    Public Class RecordParserTester
        <TestMethod>
        Sub TestParse()
            Dim rec As String = <![CDATA[
<Record>
Public Class Studnt(
	ID = 0, 
	Name = "", 
	Grades As double, 
	Address = (City := "", Street := "", No := 0)	,
  Print = Sub() Console.WriteLine($"Student `{Name}` got {Grades}" )
)
]]>.Value
            ' Test passes if no exception thrown
            ' This is a trick to debut the generator code, until it sipports debugging
            ' Just clicl the Debug Test(s) command from the contesct menu
            RecordGenerator.RecordParser.Debug(rec)
        End Sub
    End Class
End Namespace

