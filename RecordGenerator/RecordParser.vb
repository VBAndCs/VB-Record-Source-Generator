' Created By, Mohammad Hamdy Ghanem, 
' Egypt, 2021

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Class RecordParser
    Public Shared CurrentCompilation As Compilation = Nothing
    Private Const StringQuote As String = "__QUOTE__"

    Public Shared Sub Debug(code As String)
        CurrentCompilation = VisualBasicCompilation.Create("Debug")
        Generate(Nothing, code)
    End Sub

    Public Shared importsList As New StringBuilder()

    Public Shared Sub Generate(context? As GeneratorExecutionContext, code As String)
        importsList.Clear()

        For Each node In SyntaxFactory.ParseSyntaxTree(code).GetRoot.ChildNodes
            If node.Kind <> SyntaxKind.ImportsStatement Then
                code = code.Substring(node.SpanStart)
                Exit For
            End If

            For Each ImportsClause In CType(node, ImportsStatementSyntax).ImportsClauses
                importsList.AppendLine("Imports " & ImportsClause.ToString())
            Next
        Next

        Parse(context, code)
    End Sub

    Public Shared Sub Parse(context? As GeneratorExecutionContext, code As String)

        Dim tokens = SyntaxFactory.ParseTokens(code).ToArray
        For i = 0 To tokens.Length - 1
            Dim token = tokens(i)
            If token.Text = "(" Then
                If tokens(i + 1).Text.ToLower = "of" Then ' Skip tokens untile reaching "("
                    For j = i + 1 To tokens.Length - 1
                        token = tokens(j)
                        If token.Text = "(" Then Exit For
                    Next
                End If
                Dim definition = code.Substring(0, token.SpanStart)
                Dim members = SyntaxFactory.ParseParameterList(code.Substring(token.SpanStart))

                'GenerateRecord(context, definition, members)
                'Dim pos = token.SpanStart + members.Span.Length
                'If pos < code.Length Then Parse(context, code.Substring(pos))
                'Exit For

                Dim pos = token.SpanStart + members.Span.Length
                If pos >= code.Length Then Return
                Dim nextNode = SyntaxFactory.ParseSyntaxTree(code.Substring(pos)).GetRoot().ChildNodes(0)
                Dim inheritance = ""
                If nextNode?.Kind = SyntaxKind.InheritsStatement Then
                    inheritance = nextNode.ToString()
                    pos += nextNode.Span.Length + 1
                End If
                GenerateRecord(context, inheritance, definition, members)
                If pos < code.Length Then Parse(context, code.Substring(pos))
                Return
            End If
        Next
    End Sub

    Private Shared Sub GenerateRecord(
                       context? As GeneratorExecutionContext,
                       inheritance As String,
                       definition As String,
                       paramList As ParameterListSyntax)

        Dim tokens = SyntaxFactory.ParseSyntaxTree(definition).GetRoot().ChildNodes(0).ChildNodes(0).ChildNodesAndTokens.ToArray
        Dim DefaultPropInfo As New PropertyInfo

        If tokens(0).Kind = SyntaxKind.AttributeList Then
            DefaultPropInfo = GetPropertyInfo(tokens(0))
            definition = definition.Substring(tokens(1).SpanStart)
        End If

        Dim isClass = (From token In tokens
                       Where token.Kind = SyntaxKind.ClassKeyword).Count > 0

        Dim result = (From token In tokens
                      Where token.Kind = SyntaxKind.IdentifierToken)


        If Not result.Any Then
            Throw New Exception("Record must have a name")
        End If

        Dim className = result.First.ToString()
        Dim typeParams = ""
        Dim lastToken = tokens(tokens.Length - 1)
        If lastToken.Kind = SyntaxKind.TypeParameterList Then
            typeParams = lastToken.ToString()
        End If

        Dim Properties As New List(Of PropertyInfo)
        Dim Methods As New List(Of String)

        If inheritance <> "" Then AddInheritedPropertiesInfo(inheritance, Properties)

        For Each member As ParameterSyntax In paramList.ChildNodes
            Dim valueExpr = member.Default?.DescendantNodes?(0)

            If TypeOf valueExpr Is LambdaExpressionSyntax Then
                LambdaToMethod(inheritance, Methods, Properties, member, valueExpr)
            Else
                AddPropertyInfo(inheritance, Methods, Properties, DefaultPropInfo, member)
            End If
        Next

        ' ------------------------Generate the record class/struct ----------------------------

        Dim record As New StringBuilder(
"Option Explicit on
Option Strict off
Option Infer On
Option Compare Binary

")

        record.Append(importsList.ToString())

        If importsList.ToString().ToLower().IndexOf("imports system.componentmodel" & vbCrLf) = -1 Then
            record.AppendLine("Imports System.ComponentModel")
            record.AppendLine()
        End If

        record.AppendLine()

        record.AppendLine(definition)
        record.AppendLine("    " & inheritance)
        record.AppendLine()
        record.AppendLine(WriteProperties(Properties))
        Dim params = WriteConstructor(Properties, record)
        record.AppendLine(WriteMethods(Methods))
        WriteWith(className, typeParams, Properties, record, params)
        WriteWithProps(className, typeParams, Properties, record)
        WriteClone(className, typeParams, record)
        WriteToString(className, Properties, record)
        WriteEquals(className, typeParams, Properties, record)
        WriteEqualityOps(className, typeParams, record)
        WriteTuplesOps(className, typeParams, Properties, record)
        record.Append(If(isClass, "End Class", "End Structure"))

        If context.HasValue Then
            Dim rec = record.ToString()
            context.Value.AddSource(className & "Record", SourceText.From(rec, Encoding.UTF8))
            Dim syntx = SyntaxFactory.ParseSyntaxTree(rec)
            CurrentCompilation = CurrentCompilation.AddSyntaxTrees(syntx)
            'Dim variableDeclaration = SyntaxTree.GetRoot().DescendantNodes().OfType(Of LocalDeclarationStatementSyntax)().First
            'Dim t = CurrentCompilation.GetSemanticModel(syntx).GetTypeInfo()

        End If

    End Sub

    Private Shared Sub LambdaToMethod(
                                     inheritance As String,
                                     methods As List(Of String),
                                     properties As List(Of PropertyInfo),
                                     param As ParameterSyntax,
                                     lanbdaExpr As LambdaExpressionSyntax)

        Dim Header = lanbdaExpr.SubOrFunctionHeader
        Dim MethodType = Header.DeclarationKeyword.Text
        Dim isSub = MethodType.ToLower() = "sub"
        Dim AsClause = ""
        If Not isSub Then
            AsClause = Header.AsClause?.ToString()
            If AsClause <> "" Then InferType(inheritance, methods, properties, param.Default.ToString(), True)
        End If
        Dim lambdaBody As String = ""
        If TypeOf lanbdaExpr Is SingleLineLambdaExpressionSyntax Then
            lambdaBody = If(isSub, "", "Return ") & CType(lanbdaExpr, SingleLineLambdaExpressionSyntax).Body.ToString()
        Else
            For Each statement In CType(lanbdaExpr, MultiLineLambdaExpressionSyntax).Statements
                lambdaBody &= statement.ToString() & vbCrLf
            Next
            lambdaBody = lambdaBody.Trim({ChrW(10), ChrW(13)})
        End If

        methods.Add(
$"    Public {MethodType} { param.Identifier}{Header.ParameterList} {AsClause}
        {lambdaBody}
    End {MethodType}")
    End Sub

    Private Shared Sub AddPropertyInfo(
                                      inheritance As String,
                                      methods As List(Of String),
                                      properties As List(Of PropertyInfo),
                                      DefaultPropInfo As PropertyInfo,
                                      param As ParameterSyntax
                                  )

        Dim AccessAttr = param.AttributeLists
        Dim prop As PropertyInfo = Nothing
        If AccessAttr.Count > 0 Then prop = GetPropertyInfo(AccessAttr(0))

        prop.Name = param.Identifier.ToString()
        If inheritance <> "" Then
            For i = 0 To properties.Count - 1
                Dim baseProp = properties(i)
                If baseProp.Name.ToLower() = prop.Name.ToLower() Then
                    prop.IsReadOnly = baseProp.IsReadOnly
                    prop.IsKey = baseProp.IsKey Or prop.IsKey Or DefaultPropInfo.IsKey
                    prop.Type = baseProp.Type
                    prop.DefaultValue = If(param.Default?.ToString(), baseProp.DefaultValue)
                    prop.InheritanceModifier = If(baseProp.AddOverrides, "Overrides", "Shadows")
                    properties(i) = prop
                    Exit Sub
                End If
            Next
        End If

        prop.IsReadOnly = prop.IsReadOnly Or DefaultPropInfo.IsReadOnly
        prop.IsKey = prop.IsKey Or DefaultPropInfo.IsKey

        prop.Type = param.AsClause?.ToString()
        prop.DefaultValue = param.Default?.ToString()
        If prop.Type = "" Then
            prop.Type = If(prop.DefaultValue = "", "As Object", InferType(inheritance, methods, properties, prop.DefaultValue))
        End If
        prop.Type = prop.Type.Substring(2).Trim
        properties.Add(prop)
    End Sub


    Private Shared Function WriteProperties(Properties As List(Of PropertyInfo)) As String
        Dim props As New StringBuilder
        For Each p In Properties
            If p.IsKey Then
                props.AppendLine("   <Key>")
            End If

            If p.DefaultValue <> "" Then
                props.AppendLine($"   <DefaultValue(""{ p.DefaultValue.Replace("""", StringQuote)}"")>")
            End If

            props.Append("   Public ")
            If p.InheritanceModifier <> "" Then props.Append(p.InheritanceModifier & " ")
            If p.IsReadOnly = True Then props.Append("ReadOnly ")
            props.Append($"Property [{p.Name}] As {p.Type}")
            props.AppendLine(vbCrLf)
        Next
        props.AppendLine()
        Return props.ToString()
    End Function

    Private Shared Function WriteConstructor(Properties As List(Of PropertyInfo), record As StringBuilder) As String
        Dim params As New StringBuilder
        Dim body As New StringBuilder
        Dim addSep = False

        record.AppendLine("    Public Sub New(")

        For Each p In Properties
            If addSep Then
                params.Append("," & vbCrLf & "")
            Else
                addSep = True
            End If

            params.Append($"                Optional [{p.camelCaseName}] As [Optional](Of {p.Type}) = Nothing")
            body.AppendLine(
$"        If [{p.camelCaseName}].HasValue
            Me.{p.Name} = [{p.camelCaseName}].Value
        Else
            Me.{p.Name} {If(p.DefaultValue = "", "= Nothing", p.DefaultValue)}
        End If
")
        Next
        record.Append(params.ToString())
        record.AppendLine(vbCrLf & "            )")
        record.AppendLine()
        record.Append(body.ToString())
        record.AppendLine("    End Sub")
        record.AppendLine()
        Return params.ToString
    End Function

    Private Shared Function WriteMethods(Methods As List(Of String)) As String
        Dim sb As New StringBuilder
        For Each method In Methods
            sb.AppendLine(method)
            sb.AppendLine()
        Next
        Return sb.ToString
    End Function

    Private Shared Sub WriteTuplesOps(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        For n = 1 To Properties.Count - 1
            Dim body As New StringBuilder
            Dim addSep = False
            record.Append($"    Public Shared Widening Operator CType(anotherRecord As {className}{typeParams}) As (")
            For i = 0 To n
                Dim p = Properties(i)
                If addSep Then
                    record.Append(", ")
                    body.Append(", ")
                Else
                    addSep = True
                End If
                record.Append($"[{p.Name}] As {p.Type}")
                body.Append($"anotherRecord.{p.Name}")
            Next
            record.AppendLine(")")
            record.Append("        Return (")
            record.Append(body.ToString())
            record.AppendLine(")")
            record.AppendLine("    End Operator")
            record.AppendLine()

            record.Append($"    Public Shared Widening Operator CType(fromTuple As (")
            Dim methodType = "Operator"
            body.Clear()
            addSep = False

            For i = 0 To n
                Dim p = Properties(i)
                If addSep Then
                    record.Append(", ")
                    body.Append(", ")
                Else
                    addSep = True
                End If

                record.Append($"[{p.Name}] As {p.Type}")
                body.Append($"fromTuple.{p.Name}")
            Next
            record.AppendLine($")) As {className}{typeParams}")
            record.Append($"        Return new {className}{typeParams}(")
            record.Append(body.ToString)
            record.AppendLine(")")
            record.AppendLine("    End Operator")
            record.AppendLine()
        Next
    End Sub

    Private Shared Sub WriteEqualityOps(className As String, typeParams As String, record As StringBuilder)
        record.AppendLine(
$"    Public Shared Operator =(FirstRecord As {className}{typeParams}, secondRecord As {className}{typeParams}) As Boolean
        Return FirstRecord.Equals(secondRecord)
    End Operator

    Public Shared Operator <>(FirstRecord As {className}{typeParams}, secondRecord As {className}{typeParams}) As Boolean
        Return Not FirstRecord.Equals(secondRecord)
    End Operator")
        record.AppendLine()
    End Sub

    Private Shared Sub WriteEquals(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        Dim keys = From p In Properties
                   Where p.IsKey

        If keys.Any Then
            record.AppendLine(
 $"    Public Overrides Function Equals(anotherObject) As Boolean
            Dim anotherRecord = TryCast(anotherObject, {className}{typeParams})
            If anotherRecord Is Nothing Then Return False
            Return Equals(anotherRecord)
    End Function")
            record.AppendLine()

            record.AppendLine($"    Public Overloads Function Equals(anotherRecord As {className}{typeParams}) As Boolean")
            For Each p In keys
                record.AppendLine($"        If Not _{p.Name}.Equals(anotherRecord._{p.Name}) Then Return False")
            Next
            record.AppendLine("        Return True")
            record.AppendLine("    End Function")
        End If
        record.AppendLine()
    End Sub

    Private Shared Sub WriteToString(className As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        record.AppendLine(
$"    Public Overrides Function ToString() As String
        Return ""{className}"" & "" {{ "" & RecordHelper.GetPropertyValuePairs(Me) & "" }}""
    End Function")

        record.AppendLine()
    End Sub

    Private Shared Sub WriteClone(className As String, typeParams As String, record As StringBuilder)
        record.AppendLine(
$"    Public Function Clone() As {className}{typeParams}
        Return Me.With()
    End Function")
        record.AppendLine()
    End Sub

    Private Shared Sub WriteWithProps(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        For Each p In Properties
            record.AppendLine($"    Public Function With{p.Name}([{p.camelCaseName}] As {p.Type}) As {className}{typeParams}")
            record.AppendLine($"        Return Me.With([{p.camelCaseName}]:=[{p.camelCaseName}])")
            record.AppendLine("    End Function")
            record.AppendLine()
        Next
    End Sub

    Private Shared Sub WriteWith(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder, params As String)
        record.AppendLine("    Public Function [With](")
        record.Append(params)
        record.AppendLine(vbCrLf & $"            ) As {className}{typeParams}")
        record.AppendLine()

        Dim body As New StringBuilder
        body.AppendLine($"        Return  New {className}{typeParams}(")
        Dim addSep = False
        For Each p In Properties
            If addSep Then
                body.AppendLine(",")
            Else
                addSep = True
            End If
            body.Append($"            If ([{p.camelCaseName}].HasValue, [{p.camelCaseName}].Value, Me.{p.Name})")
        Next
        body.AppendLine()
        body.AppendLine("        )")
        record.Append(body.ToString())
        record.AppendLine("    End Function")
        record.AppendLine()
    End Sub

    Private Shared Sub AddInheritedPropertiesInfo(
                                     inheritance As String,
                                     properties As List(Of PropertyInfo)
                         )

        Dim code = $"
{importsList}
Class Test_00000000000000
   {inheritance}
End Class
"

        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = CurrentCompilation.AddSyntaxTrees(syntaxTree)
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim inhertsStatement = syntaxTree.GetRoot().DescendantNodes().OfType(Of InheritsStatementSyntax)().First

        Try
            Dim baseClass = sem.GetTypeInfo(inhertsStatement.ChildNodes(0)).Type
            For Each m In baseClass.GetMembers()
                Dim prop = TryCast(m, IPropertySymbol)
                If prop IsNot Nothing AndAlso prop.DeclaredAccessibility = Accessibility.Public Then
                    Dim attrs = prop.GetAttributes()
                    Dim defValue = ""
                    Dim isKey = False
                    For Each attr In attrs
                        If attr.AttributeClass.Name = "DefaultValueAttribute" Then
                            defValue = attr.ConstructorArguments(0).Value.ToString().Replace(StringQuote, """")
                        ElseIf attr.AttributeClass.Name = "Key" Then
                            isKey = True
                        End If
                    Next

                    properties.Add(New PropertyInfo() With {
                        .Name = prop.Name,
                        .Type = prop.Type.ToString(),
                        .DefaultValue = defValue,
                        .IsKey = isKey,
                        .IsReadOnly = prop.IsReadOnly,
                        .AddOverrides = prop.IsMustOverride OrElse prop.IsOverridable
                    })
                End If
            Next
        Catch
        End Try
    End Sub

    Private Shared Function InferType(
                                     inheritance As String,
                                     methods As List(Of String),
                                     properties As List(Of PropertyInfo),
                                     defaultValue As String,
                                     Optional isLambda As Boolean = False
                         ) As String

        Dim code = $"
{importsList}
Class Test_00000000000001
   {inheritance}
    Sub Foo
        Dim a {defaultValue}
    End Sub

{WriteProperties(properties)}
{WriteMethods(methods)}
End Class
"

        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = CurrentCompilation.AddSyntaxTrees(syntaxTree)
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim variableDeclaration = syntaxTree.GetRoot().DescendantNodes().OfType(Of LocalDeclarationStatementSyntax)().First

        Try
            If isLambda Then
                Dim typeSymbol = CType(CType(sem.GetDeclaredSymbol(variableDeclaration.ChildNodes(0).ChildNodes(0)), ILocalSymbol).Type, INamedTypeSymbol).DelegateInvokeMethod.ReturnType
                If typeSymbol Is Nothing Then Return "As Object"
                Dim typeName = typeSymbol.ToDisplayString()
                Return "As " & If(typeName = "?" OrElse typeName.StartsWith("<anonymous type: "), "Object", typeName)
            Else
                Dim typeSymbol = sem.GetOperation(variableDeclaration.Declarators(0).Initializer.Value).Type
                Dim typeName = typeSymbol.ToDisplayString()
                Return "As " & If(typeName = "?" OrElse typeName.StartsWith("<anonymous type: "), "Object", typeName)
            End If
        Catch
        End Try

        Return "As Object"

    End Function

    Private Shared Function GetPropertyInfo(attributeList As AttributeListSyntax) As PropertyInfo
        Dim propInfo As New PropertyInfo
        Select Case attributeList.Attributes(0).ToString().ToLower()
            Case "key"
                propInfo.IsKey = True

            Case "readonly"
                propInfo.IsReadOnly = True

            Case "readonlykey", "immitable", "record"
                propInfo.IsKey = True
                propInfo.IsReadOnly = True

        End Select

        Return propInfo
    End Function

End Class


