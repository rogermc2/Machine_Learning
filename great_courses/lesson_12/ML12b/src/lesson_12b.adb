
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

with Support_12B; use Support_12B;

procedure Lesson_12B is
   Program_Name     : constant String := "Lesson 12B ";
   Vocab_Dictionary : constant Dictionary_List :=
                        Read_Vocabulary ("../../data/vocab2.txt");
   --  Paper_Titles is (dat, labs)
   Paper_Titles    : constant Data_Items :=
                        Get_Data ("../../data/cb.txt", Vocab_Dictionary);
   Rounds           : constant Positive := 1000;
   Rep              : constant Positive := 10;
   F_Rep            : constant := Float (Rep);
   Alphas           : constant Real_Float_Vector (1 .. 14) :=
                        (0.00005, 0.0001, 0.0002, 0.0005, 0.001, 0.002,
                         0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0);
   Classifier       : Python.Module;
   Res              : ML_Types.Integer_List;
   Result           : Real_Float_List;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_12b");
   for alpha in Alphas'Range loop
      Put_Line (Program_Name & "processing alpha = " &
                  Float'Image (Alphas (alpha)));
      Res.Clear;
      for index in 1 .. Rep loop
         Res.Append (Play_Game (Classifier, Rounds, Paper_Titles,
                     Alphas (alpha)));
      end loop;

      Result.Append (Float (ML_Types.Sum (Res)) / F_Rep);
   end loop;

   New_Line;

   Python.Call (Classifier, "plot", Alphas, Result);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_12B;
