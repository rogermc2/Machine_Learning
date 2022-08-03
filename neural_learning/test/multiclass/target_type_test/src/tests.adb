
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Multiclass_Utils; use Multiclass_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

with Examples; use Examples;

package body Tests is

   procedure Binary_Tests is
      Routine_Name : constant String := "Tests.Binary_Tests ";
      Target_Type  : Y_Type;
   begin
      Put_Line (Routine_Name);

      for index in Binary_Examples.B_Binary.First_Index ..
        Binary_Examples.B_Binary.Last_Index loop
         declare
            B_Array : constant Binary_Array := Binary_Examples.B_Binary (index);
         begin
            Target_Type := Type_Of_Target (B_Array);
            Assert (Target_Type = Y_Binary, "Type_of_target for binary index" &
                      Integer'Image (index) & " should be Y_Binary, but got " &
                      Y_Type'Image (Target_Type));
         end;
      end loop;

   end Binary_Tests;

   --  -------------------------------------------------------------------------

   procedure Continuous_Tests is
      Routine_Name : constant String := "Tests.Continuous_Tests ";
      Target_Type  : Y_Type;

      procedure Vec_Test (C : Real_Float_Vector) is
      begin
         for index in C'Range loop
            Target_Type := Type_Of_Target (C);
            Assert (Target_Type = Y_Continuous,
                    "Type_of_target for continuous index" &
                      Integer'Image (index) & " should be Y_Continuous, but got " &
                      Y_Type'Image (Target_Type));
         end loop;
      end Vec_Test;

   begin
      Put_Line (Routine_Name);
      Vec_Test (Continuous_Examples.C_Float1);
      Vec_Test (Continuous_Examples.C_Float2);

      Target_Type := Type_Of_Target (Continuous_Examples.C_Float3);
      Assert (Target_Type = Y_Continuous,
              "Type_of_target for continuous matrix should be Y_Continuous" &
                "but got " & Y_Type'Image (Target_Type));

   end Continuous_Tests;

   --  -------------------------------------------------------------------------

end Tests;
