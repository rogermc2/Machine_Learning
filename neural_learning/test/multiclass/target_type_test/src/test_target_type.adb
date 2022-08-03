
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Printing;

with Examples; use Examples;
with Multiclass_Utils; use Multiclass_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

procedure Test_Target_Type is
    Routine_Name : constant String := "Test_Target_Type ";
    Target_Type  : Y_Type;
begin
    Put_Line (Routine_Name);
    Examples.Init;
    for index in Binary_Examples.B_Binary.First_Index ..
      Binary_Examples.B_Binary.Last_Index loop
        declare
            B_Array     : constant Binary_Array := Binary_Examples.B_Binary (index);
        begin
            Target_Type := Type_Of_Target (B_Array);
            Assert (Target_Type = Y_Binary,
                    "Type_of_target should be Y_Binary, but got " &
                    Y_Type'Image (Target_Type));
        end;
    end loop;

end Test_Target_Type;
