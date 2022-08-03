
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

procedure Test_Target_Type is
   use NL_Types.Float_Package;
   use NL_Types.Float_List_Package;
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name  : constant String := "Test_Target_Type ";
begin
   Put_Line (Routine_Name);
   for row in 1 .. 100 loop
      for col in 1 .. Data.Num_Features loop
         X (row, col) := Features (row, col);
      end loop;
      Y (row, 1) := Data.Target (row);
   end loop;


end Test_Target_Type;
