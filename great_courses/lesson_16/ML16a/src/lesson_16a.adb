
with Ada.Exceptions; use Ada.Exceptions;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;
with Python_CLF;

with Python_16A;
with Support_16A; use Support_16A;

procedure Lesson_16A is
   Program_Name         : constant String := "Lesson 16A ";
   Max_Words            : constant Positive := 2000;  -- 20000
   Max_Sequence_Size    : constant Positive := 50;  --  1000
   Embedding_Dimension  : constant Positive := 20;  --  100
   Classifier           : Python.Module;
   Tokenizer            : Python_API.PyObject_Ptr;
   Newsgroups           : Newsgroups_Record;
   Sequences            : ML_Types.Integer_List_2D;
   Word_Index           : Occurrences_Dictionary;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_16a");

   Newsgroups := Load_Newsgroups (Classifier, True);

   Put_Line (Program_Name  & "Newsgroups file read");
   Put_Line (Program_Name  & "Newsgroups.Data length" &
               Integer'Image (Integer (Newsgroups.Data.Length)));
   New_Line;
   Tokenizer := Python.Call (Classifier, "init_tokenizer", Max_Words);
   Put_Line (Program_Name  & "Tokenizer initialised");
   Python_CLF.Call (Classifier, "fit", Tokenizer, Newsgroups.Data);
   Put_Line (Program_Name  & "data fitted");
   Sequences := Python_CLF.Call (Classifier, "get_sequences", Tokenizer,
                                 Newsgroups.Data);
   Put_Line (Program_Name  & "Sequences: " &
               Integer'Image (Integer (Sequences.Length)));
   --     Print_Integer_List_2D (Program_Name & "Sequences", Sequences, 1, 1);
   Word_Index := Python_16A.Call (Classifier, "get_word_index", Tokenizer);
   Put_Line (Program_Name & Integer'Image (Integer (Word_Index.Length)) &
               " unique tokens found.");

   declare
      Embedding_Matrix : Embedding_Matrix_Type := Prepare_Embedding_Matrix
        (Word_Index, Max_Words, Embedding_Dimension);
      Data : Integer_Matrix :=
        Python.Call (Classifier, "get_data", Sequences, Max_Sequence_Size);
      Labels : Integer_Matrix :=
        Python.Call (Classifier, "get_labels", Sequences, Max_Sequence_Size);
   begin
      null;
   end;
   --     Python.Call (Classifier, "plot", Alphas, Result);

   Python_API.Py_DecRef (Tokenizer);
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

end Lesson_16A;
