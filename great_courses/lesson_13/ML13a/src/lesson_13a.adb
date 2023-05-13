
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Types;
with Python;

with Support_13A; use Support_13A;

procedure Lesson_13A is
   Program_Name     : constant String := "Lesson 13A ";
   Vocab_Dictionary : constant Dictionary_List :=
                        Read_Vocabulary ("../../data/vocab2.txt");
   --  CB_Data is (dat, labs)
   CB_Data          : constant Data_Items :=
                        Get_Data ("../../data/cb.txt", Vocab_Dictionary);
   Rounds           : constant Positive := 1000;
   Classifier       : Python.Module;
   Alpha            : Positive := 5;
   Alphas           : ML_Types.Integer_List;
   Result           : ML_Types.Integer_List;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_12a");
   while Alpha < 200 loop
      Put ("*");
      Alpha := Alpha + 5;
      Alphas.Append (Alpha);
      Result.Append (Play_Game (Classifier, Rounds, CB_Data, Alpha));
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

end Lesson_13A;
