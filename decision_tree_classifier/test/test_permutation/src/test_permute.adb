
with Classifier_Types; use Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with ML_Types; use ML_Types;
with Printing; use Printing;
with Utilities; use Utilities;

procedure Test_Permute is
   A      : constant Integer_Array (1 .. 3) := (1, 2, 3);
   A_List : constant Value_Data_Lists_2D := To_Integer_Value_List_2D (A);
   P_List : Value_Data_Lists_3D;
   --  Expected: (1, 2, 3), (3, 1, 2), (1, 3, 2),
   --            (2, 1, 3), (2, 3, 1), (3, 2, 1),
begin
   P_List := Permute (A_List);
   for index in P_List.First_Index .. P_List.Last_Index loop
      Print_Value_Data_Lists_2D ("P_List", Transpose (P_List.Element (index)));
   end loop;
end Test_Permute;
