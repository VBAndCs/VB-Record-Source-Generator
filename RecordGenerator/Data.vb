Module Data
    Friend DefaultOfStruct As String = <![CDATA[
Public Structure DefaultOf
    Public Shared ReadOnly [Byte] As [Optional](Of Byte) = New [Optional](Of Byte)(Nothing)
    Public Shared ReadOnly [SByte] As [Optional](Of SByte) = New [Optional](Of SByte)(Nothing)
    Public Shared ReadOnly [Short] As [Optional](Of Short) = New [Optional](Of Short)(Nothing)
    Public Shared ReadOnly Int16 As [Optional](Of Short) = New [Optional](Of Short)(Nothing)
    Public Shared ReadOnly [UShort] As [Optional](Of UShort) = New [Optional](Of UShort)(Nothing)
    Public Shared ReadOnly UInt16 As [Optional](Of UShort) = New [Optional](Of UShort)(Nothing)
    Public Shared ReadOnly [Integer] As [Optional](Of Integer) = New [Optional](Of Integer)(Nothing)
    Public Shared ReadOnly Int32 As [Optional](Of Integer) = New [Optional](Of Integer)(Nothing)
    Public Shared ReadOnly [UInteger] As [Optional](Of UInteger) = New [Optional](Of UInteger)(Nothing)
    Public Shared ReadOnly UInt32 As [Optional](Of UInteger) = New [Optional](Of UInteger)(Nothing)
    Public Shared ReadOnly [Long] As [Optional](Of Long) = New [Optional](Of Long)(Nothing)
    Public Shared ReadOnly Int64 As [Optional](Of Long) = New [Optional](Of Long)(Nothing)
    Public Shared ReadOnly [ULong] As [Optional](Of ULong) = New [Optional](Of ULong)(Nothing)
    Public Shared ReadOnly IInt64 As [Optional](Of ULong) = New [Optional](Of ULong)(Nothing)
    Public Shared ReadOnly [Single] As [Optional](Of Single) = New [Optional](Of Single)(Nothing)
    Public Shared ReadOnly [Double] As [Optional](Of Double) = New [Optional](Of Double)(Nothing)
    Public Shared ReadOnly [Decimal] As [Optional](Of Decimal) = New [Optional](Of Decimal)(Nothing)
    Public Shared ReadOnly [Date] As [Optional](Of Date) = New [Optional](Of Date)(Nothing)
    Public Shared ReadOnly DateTime As [Optional](Of Date) = New [Optional](Of Date)(Nothing)
    Public Shared ReadOnly [Char] As [Optional](Of Char) = New [Optional](Of Char)(Nothing)
    Public Shared ReadOnly [String] As [Optional](Of String) = New [Optional](Of String)(Nothing)
    Public Shared ReadOnly [Object] As [Optional](Of Object) = New [Optional](Of Object)(Nothing)
End Structure
]]>.Value

    Friend DefaultStruct As String = <![CDATA[
Public Structure [Default]
    Public Shared ReadOnly [Byte] As [Optional](Of Byte) = New [Optional](Of Byte)(Nothing)
    Public Shared ReadOnly [SByte] As [Optional](Of SByte) = New [Optional](Of SByte)(Nothing)
    Public Shared ReadOnly [Short] As [Optional](Of Short) = New [Optional](Of Short)(Nothing)
    Public Shared ReadOnly Int16 As [Optional](Of Short) = New [Optional](Of Short)(Nothing)
    Public Shared ReadOnly [UShort] As [Optional](Of UShort) = New [Optional](Of UShort)(Nothing)
    Public Shared ReadOnly UInt16 As [Optional](Of UShort) = New [Optional](Of UShort)(Nothing)
    Public Shared ReadOnly [Integer] As [Optional](Of Integer) = New [Optional](Of Integer)(Nothing)
    Public Shared ReadOnly Int32 As [Optional](Of Integer) = New [Optional](Of Integer)(Nothing)
    Public Shared ReadOnly [UInteger] As [Optional](Of UInteger) = New [Optional](Of UInteger)(Nothing)
    Public Shared ReadOnly UInt32 As [Optional](Of UInteger) = New [Optional](Of UInteger)(Nothing)
    Public Shared ReadOnly [Long] As [Optional](Of Long) = New [Optional](Of Long)(Nothing)
    Public Shared ReadOnly Int64 As [Optional](Of Long) = New [Optional](Of Long)(Nothing)
    Public Shared ReadOnly [ULong] As [Optional](Of ULong) = New [Optional](Of ULong)(Nothing)
    Public Shared ReadOnly IInt64 As [Optional](Of ULong) = New [Optional](Of ULong)(Nothing)
    Public Shared ReadOnly [Single] As [Optional](Of Single) = New [Optional](Of Single)(Nothing)
    Public Shared ReadOnly [Double] As [Optional](Of Double) = New [Optional](Of Double)(Nothing)
    Public Shared ReadOnly [Decimal] As [Optional](Of Decimal) = New [Optional](Of Decimal)(Nothing)
    Public Shared ReadOnly [Date] As [Optional](Of Date) = New [Optional](Of Date)(Nothing)
    Public Shared ReadOnly DateTime As [Optional](Of Date) = New [Optional](Of Date)(Nothing)
    Public Shared ReadOnly [Char] As [Optional](Of Char) = New [Optional](Of Char)(Nothing)
    Public Shared ReadOnly [String] As [Optional](Of String) = New [Optional](Of String)(Nothing)
    Public Shared ReadOnly [Object] As [Optional](Of Object) = New [Optional](Of Object)(Nothing)
End Structure
]]>.Value

    Friend DefaultOfTStruct As String = <![CDATA[
Public Structure [Default](Of T)
    Public Shared Widening Operator CType(value As [Default](Of T)) As [Optional](Of T)
        Return New [Optional](Of T)(Nothing)
    End Operator
End Structure
]]>.Value

    Friend OptionalStruct As String = <![CDATA[
Public Structure [Optional](Of T)

    Private Sub New(value As T, hasValue As Boolean)
        _HasValue = hasValue
        _value = value
    End Sub

    Public Sub New(value As T)
        _HasValue = True
        _value = value
    End Sub

    Public Shared ReadOnly [Default] As [Optional](Of T) = New [Optional](Of T)(Nothing, True)

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

End Module
