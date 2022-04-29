
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types; use Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with ML_Types; use ML_Types;
with Printing; use Printing;
with Utilities; use Utilities;

procedure Test_Permute is
    A      : constant Integer_Array (1 .. 3) := (1, 2, 3);
    A_List : constant Value_Data_Lists_2D := To_Integer_Value_List_2D (A);
    P_List : Value_Data_Lists_3D;
    aPerm  : Value_Data_Lists_2D;
    Count  : Natural := 0;
    --  Expected: (1, 2, 3), (2, 1, 3), (3, 1, 2),
    --            (1, 3, 2),(2, 3, 1), (3, 2, 1),
    Permutations : Value_Data_Lists_3D;
begin
    P_List := Permute (A_List);
    for index in P_List.First_Index .. P_List.Last_Index loop
        Count := Count + 1;
        Print_Value_Data_Lists_2D ("P_List " & Integer'Image (Count),
                                   Transpose (P_List.Element (index)));
    end loop;

    New_Line;
    for count in  1 .. 10 loop
      aPerm := Permute (A_List);
      Permutations.Append (Transpose (aPerm));
    end loop;
    Print_Value_Data_Lists_3D ("Permutations", Permutations);

end Test_Permute;
