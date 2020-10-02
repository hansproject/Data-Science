Attribute VB_Name = "Module1"
Private Sub RemoveBlanks()
Dim cell As range
Dim range As range

Set range = Worksheets("showwcase_sessions").range("A2:N30")

For Each cell In range

If cell.Value = "" Then

cell.Interior.ColorIndex = 4
cell.Value = "Blank Cell"
End If

Next cell
End Sub

Private Sub RemoveExtraSpaces()
Dim cell As range
Dim range As range

Set range = Worksheets("showwcase_sessions").range("A2:N30")

For Each cell In range

cell.Value = Application.WorksheetFunction.Trim(cell.Value)

Next cell
End Sub

Private Sub RemoveDuplicates()
'Select data
Worksheets("showwcase_sessions").range("A2:A30").Select
'Get last row
lastrow = Worksheets("showwcase_sessions").Cells(Rows.Count, 1).End(xlUp).Row
For i = lastrow To 2 Step by - 1
If Worksheets("showwcase_sessions").Cells(i, 1).Value = Worksheets("showwcase_sessions").Cells(i - 1, 1).Value Then
Worksheets("showwcase_sessions").Rows(i).Select
Selection.Delete shift:=xlUp
End If
Next
Worksheets("showwcase_sessions").Cells(1, 1).Select

End Sub
