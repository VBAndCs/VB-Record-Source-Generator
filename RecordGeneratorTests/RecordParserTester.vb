Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace RecordGeneratorTests
    <TestClass>
    Public Class RecordParserTester
        <TestMethod>
        Sub Debug()
            Dim rec As String = <![CDATA[
Public Class Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function() As String
                     return Name & Grades
                End Function
)
]]>.Value

            ' Test passes if no exception thrown
            ' This is a trick to debug the generator code, until it supports debugging
            ' Just click the Debug Test(s) command from the context menu
            RecordGenerator.RecordParser.Debug(rec)
        End Sub
    End Class
End Namespace

