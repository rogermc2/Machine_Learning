
with Ada.Text_IO; use Ada.Text_IO;

with Examples;
with Type_Tests; use Type_Tests;

procedure Test_Target_Type is
   Routine_Name : constant String := "Test_Target_Type ";
begin
   Put_Line (Routine_Name);
   Examples.Init;

   Binary_Tests;
   Continuous_Tests;
   Multilabel_Indicator_Tests;
   Multiclass_Tests;
   Multiclass_Multioutput_Tests;

end Test_Target_Type;
