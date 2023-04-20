
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

with Support_12A; use Support_12A;

procedure Lesson_12A is
   Program_Name     : constant String := "Lesson 12A ";
   Vocab_Dictionary : constant Dictionary_List :=
                        Read_Vocabulary ("../../data/vocab2.txt");
   CB               : constant Data_Items :=
                        Get_Data ("../../data/cb.txt", Vocab_Dictionary);
--     Features         : constant Integer_Matrix := To_Matrix (CB.Features);
   Rounds           : constant Positive := 2; --  1000;
   Alpha            : Positive := 5;
   Alphas           : ML_Types.Integer_List;
   Result           : ML_Types.Integer_List;
   Classifier       : Python.Module;
   Score            : ML_Types.Integer_List;
begin
   Put_Line (Program_Name);

   Print_Matrix_Dimensions (Program_Name & "CB.Features", CB.Features);
--     Print_Matrix_Dimensions (Program_Name & "Features", Features);
   Print_Matrix_Dimensions (Program_Name & "CB.Labels", CB.Labels);
   Python.Initialize;
--
--     declare
--        Labels : Integer_Matrix (1 .. Integer (CB.Labels.Length),
--                                 1 .. CB.Labels.Element (1)'Length);
--     begin
--        Labels := To_Matrix (CB.Labels);
--        Print_Matrix_Dimensions (Program_Name & "Labels", Labels);
      Classifier := Python.Import_File ("lesson_12a");

      --     while Alpha <= 200 loop
      while Alpha <= 10 loop
         Alpha := Alpha + 5;
         Alphas.Append (Alpha);
         Put_Line (Program_Name & "Alpha" & Integer'Image (Alpha));
         Score := Play_Game (Classifier, Rounds, CB.Features, CB.Labels, Alpha,
                             ProbA_Chooser'Access);
         Result.Append (Score);
      end loop;
--     end;

   Python.Call (Classifier, "plot", Result, Alphas);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

   exception
      when Constraint_Error => Put_Line (Program_Name & "Constraint_Error");
      when others => Put_Line (Program_Name & "exception");

end Lesson_12A;
