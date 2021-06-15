Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace RecordGeneratorTests
    <TestClass>
    Public Class RecordParserTester
        <TestMethod>
        Sub Debug()
            Dim rec As String = <![CDATA[
Imports System.Text, System.IO
Imports System.Collections
<Record>
Public Class Student(
	ID = 0, 
	Name = "", 
	Grades As double, 
	Address = (City := "", Street := "", No := 0)	,
    Print = Function() "Test" 
)
]]>.Value

            ' Test passes if no exception thrown
            ' This is a trick to debug the generator code, until it supports debugging
            ' Just click the Debug Test(s) command from the context menu
            RecordGenerator.RecordParser.Debug(rec)
        End Sub
    End Class
End Namespace

