
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with NL_Types;
with Python;
with Python_API;
with Python_CLF;

with Python_16A;
with Support_16A; use Support_16A;

procedure Lesson_16A is
   Program_Name         : constant String := "Lesson 16A ";
   Emmbeddings_Index    : constant Dictionary :=
     Get_Glove_Data ("../../data/glove.6B/glove.6B.100d.txt");
   Newsgroups_File      : constant String := "Newsgroups.data";
   Max_Words            : constant Positive := 20000;
   Max_Sequence_Size    : constant Positive := 1000;
   Emmbedding_Dimension : constant Positive := 100;
   Classifier           : Python.Module;
   Tokenizer            : Python_API.PyObject_Ptr;
   Newsgroups           : Newsgroups_Record;
   Sequences            : ML_Types.Integer_List_2D;
   Word_Index           : Word_Dictionary;
   Embedding_Vector     : NL_Types.Float_List;
   Num_Words            : Natural;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_16a");

   Newsgroups := Load_Newsgroups (Classifier, Newsgroups_File, True);

   Put_Line (Program_Name  & "Newsgroups file read");
   Put_Line (Program_Name  & "Newsgroups.Data length" &
               Integer'Image (Integer (Newsgroups.Data.Length)));
   New_Line;
   Tokenizer := Python.Call (Classifier, "init_tokenizer", Max_Words);
   Python_CLF.Call (Classifier, "fit", Tokenizer, Newsgroups.Data);
   Sequences := Python_CLF.Call (Classifier, "get_sequences", Tokenizer,
                                 Newsgroups.Data);
   Print_Integer_List_2D (Program_Name & "Sequences", Sequences, 1, 1);
   Word_Index := Python_16A.Call (Classifier, "get_word_index", Tokenizer);
   Put_Line (Program_Name & Integer'Image (Integer (Word_Index.Length)) &
               " unique tokens found.");
   Num_Words := Integer'Min (Max_Words, Integer (Word_Index.Length));

   declare
      use Word_Dictionary_Package;
      aKey       : Unbounded_String;
--        Value      : Integer;
      Word_Count : Natural := 0;
--        Emmbedding_Matrix : array (1 .. Num_Words, 1 .. Emmbedding_Dimension)
      Emmbedding_Matrix : array (1 .. Num_Words) of NL_Types.Float_List;
   begin
      for index in Word_Index.Iterate loop
         Word_Count := Word_Count + 1;
         if Word_Count <= Max_Words then
            aKey := Key (index);
--              Value := Element (index);
            Embedding_Vector := Emmbeddings_Index.Element (aKey);
            Emmbedding_Matrix (Word_Count) := Embedding_Vector;
         end if;
      end loop;
   end;

--     Python.Call (Classifier, "plot", Alphas, Result);

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
