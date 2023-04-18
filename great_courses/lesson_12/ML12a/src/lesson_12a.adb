
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Types;
with Python;

with Support_12A; use Support_12A;

procedure Lesson_12A is
   Program_Name     : constant String := "Lesson 12A ";
   Vocab_Dictionary : constant Dictionary_List :=
                        Read_Vocabulary ("../../data/vocab2.txt");
   CB               : constant Data_Lists :=
                        Get_Data ("../../data/cb.txt", Vocab_Dictionary);
--     Features         : Integer_Matrix := To_Integer_Matrix (CB.Features);
--     Labels         : Integer_Matrix := To_Integer_Matrix (CB.Labels);
   Rounds           : constant Positive := 2; --  1000;
   Alpha            : Positive := 5;
   Alphas           : ML_Types.Integer_List;
   Result           : ML_Types.Integer_List;
   Classifier       : Python.Module;
   Score            : ML_Types.Integer_List;
begin
   Put_Line (Program_Name);

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_12a");

--     while Alpha <= 200 loop
   while Alpha <= 10 loop
      Alpha := Alpha + 5;
      Alphas.Append (Alpha);
      Put_Line (Program_Name & "Alpha" & Integer'Image (Alpha));
      Put_Line (Program_Name & "CB.Features length" &
                  Integer'Image (Integer (CB.Features.Length)));
      Put_Line (Program_Name & "CB.Labels length" &
                  Integer'Image (Integer (CB.Labels.Length)));
      Put_Line (Program_Name & "CB.Labels (1) length" &
                  Integer'Image (Integer (CB.Labels.Element (1)'Length)));
      Score := Play_Game (Classifier, Rounds, To_Matrix (CB.Features),
                          To_Matrix (CB.Labels), Alpha, ProbA_Chooser'Access);
      Result.Append (Score);
   end loop;

   Python.Call (Classifier, "plot", Result, Alphas);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_12A;
