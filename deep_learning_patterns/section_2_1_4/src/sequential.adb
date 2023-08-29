
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Shuffler;

with Structure; use Structure;

procedure Sequential is
   Program_Name : constant String := "Sequential ";

   Level_1      : constant Real_Float_Vector (1 .. 13) :=
   (1.0, 0.0, 0.5, -0.5, 2.3, -5.2, 10.9, -12.0, 4.5, 6.9, -0.1, 7.0, -8.0);
   Level_2      : Real_Float_Vector (1 .. 10);
   Level_3      : Real_Float_Vector (1 .. 10);
   Level_4      : Real_Float_Vector (1 .. 1);
   Connect_12   : Real_Float_Matrix := Connect (Level_1, Level_2);
   Connect_23   : Real_Float_Matrix := Connect (Level_2, Level_3);
   Connect_34   : Real_Float_Matrix := Connect (Level_3, Level_4);
   Levels_List  : Float_Vector_List;
   Connect_List : Float_Matrix_List;

--     Classifier           : Python.Module;

begin
   Levels_List.Append (Level_1);
   Levels_List.Append (Level_2);
   Levels_List.Append (Level_3);
   Levels_List.Append (Level_4);
   Connect_List.Append (Connect_12);
   Connect_List.Append (Connect_23);
   Connect_List.Append (Connect_34);

--     Python.Initialize;
--     Classifier := Python.Import_File ("sequential");
--
--     Python.Call (Classifier, "plot", X);
--
--     Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Sequential;
