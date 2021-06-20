Module Data


    Friend OptionalStruct As String = <![CDATA[
Option Explicit on
Option Strict off
Option Infer On
Option Compare Binary

Public Structure [Nothing](Of T)
    Public Shared Widening Operator CType(value As [Nothing](Of T)) As [Optional](Of T)
        Return New [Optional](Of T)(Nothing)
    End Operator
End Structure

Public Structure [Optional](Of T)

    Private Sub New(value As T, hasValue As Boolean)
        _HasValue = hasValue
        _value = value
    End Sub

    Public Sub New(value As T)
        _HasValue = True
        _value = value
    End Sub

    Public Shared ReadOnly [Nothing] As [Optional](Of T) = New [Optional](Of T)(Nothing, True)

    Public ReadOnly Property HasValue As Boolean

    Dim _value As T
    Public ReadOnly Property Value As T
        Get
            If Not _HasValue Then Throw New Exception("Value is not set")
            Return _value
        End Get
    End Property

    Public Shared Widening Operator CType(value As T) As [Optional](Of T)
        If value Is Nothing Then
            Return New [Optional](Of T)(value, False)
        Else
            Return New [Optional](Of T)(value, True)
        End If
    End Operator

    Public Shared Widening Operator CType(value As [Optional](Of T)) As T
            Return value.Value
    End Operator

End Structure
]]>.Value

    Friend HelperClass As String = <![CDATA[
Option Explicit on
Option Strict off
Option Infer On
Option Compare Binary

Friend Class KeyAttribute
    Inherits Attribute
End Class

Public Class RecordHelper
    Public Shared Function GetPropertyValuePairs(obj As Object) As String
        Dim sb As New System.Text.StringBuilder
        Dim addSep = False
        For Each p In obj.GetType.GetProperties(Reflection.BindingFlags.Public Or Reflection.BindingFlags.Instance)
            If addSep Then
                sb.Append(", ")
            Else
                addSep = True
            End If

            Dim value = p.GetValue(obj, Nothing)
            If TypeOf value IsNot String Then
                Dim list = TryCast(value, IEnumerable)
                If list IsNot Nothing Then
                    Dim sbValue As New System.Text.StringBuilder()
                    Dim addSep2 = False
                    sbValue.Append("[")
                    For Each item In list
                        If addSep2 Then
                            sbValue.Append(", ")
                        Else
                            addSep2 = True
                        End If
                        sbValue.Append(item.ToString())
                    Next
                    sbValue.Append("]")
                    value = sbValue.ToString()
                Else
                    value = value.ToString()
                End If
            End If

            sb.Append($"{p.Name} = {value}")
        Next
        Return sb.ToString()
    End Function
End Class
]]>.Value
End Module
