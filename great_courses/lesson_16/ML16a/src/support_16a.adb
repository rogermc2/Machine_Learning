
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Directories; use Ada.Directories;
--  with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Neural_Utilities;
with Python_16A;
with Python_CLF;

package body Support_16A is

   --     Lex_Size    : constant Unbounded_String := To_Unbounded_String ("@size");
   --     Unknown     : constant Unbounded_String := To_Unbounded_String ("@unk");

   --     function Tokenize (aLine : String; Dictionary : Dictionary_List)
   --                        return Integer_Array;
   --     pragma Inline (Tokenize);
   function Process_Glove_Line (File_ID : File_Type) return Coeffs_Record;
   pragma Inline (Process_Glove_Line);

   --  -------------------------------------------------------------------------
   --  Arg_Max returns the index from indices associated with the item in the
   --  Values list with the highest value.
   function Arg_Max (Indices : Integer_Array; Values : Real_Float_Vector)
                     return Integer is
      --        Routine_Name : constant String := "Support_16A.Arg_Max ";
      Best_Index   : Positive;
      pragma Warnings (Off);
      Best         : constant Float := Max (Values, Best_Index);
      pragma Warnings (On);
   begin
      return Indices (Best_Index);

   end Arg_Max;
   pragma Inline (Arg_Max);

   --  -------------------------------------------------------------------------

   --     function Find_Item
   --       (Dictionary : Dictionary_List; Key : Unbounded_String;
   --        Item       : out Dictionary_Record) return Boolean is
   --        use Dictionary_Package;
   --        --        Routine_Name : constant String := "Support_16A.Find_Item ";
   --        Curs  : Cursor := Dictionary.First;
   --        Found : Boolean := False;
   --     begin
   --        while Has_Element (Curs) and not Found loop
   --           Item := Element (Curs);
   --           Found := Item.Key = Key;
   --           Next (Curs);
   --        end loop;
   --
   --        return Found;
   --
   --     end Find_Item;
   --     pragma Inline (Find_Item);

   --  -------------------------------------------------------------------------

   function Get_Glove_Data (File_Name : String) return Coeffs_Dictionary is
      Routine_Name : constant String := "Support_16A.Get_Glove_Data ";
      File_ID      : File_Type;
      Num_Lines    : Natural := 0;
      Row          : Natural := 0;
      Item         : Coeffs_Record;
      Embeddings   : Coeffs_Dictionary;
   begin
      New_Line;
      Put_Line (Routine_Name & "reading " & File_Name);
      Open (File_ID, In_File, File_Name);
      Put_Line (Routine_Name & "processing " & File_Name);
      Put ("*");

      while not End_Of_File (File_ID) loop
         Row := Row + 1;
         Num_Lines := Num_Lines + 1;
         if Num_Lines mod 5000 = 0 then
            Put ("*");
         end if;

         Item := Process_Glove_Line (File_ID);
         Embeddings.Insert (Item.Key, Item.Coeffs);
      end loop;
      New_Line;

      Close (File_ID);

      Put_Line (Routine_Name & "found" &
                  Integer'Image (Integer (Embeddings.Length)) &
                  " word vectors.");
      return Embeddings;

   end Get_Glove_Data;

   --  -------------------------------------------------------------------------

   function Load_Newsgroups (Classifier : Python.Module; File_Name : String;
                             Reload     : Boolean := False) return Newsgroups_Record is
      use Ada.Streams;
      use Stream_IO;
      Routine_Name  : constant String := "Support_16A.Load_Newsgroups ";
      File_ID       : Stream_IO.File_Type;
      aStream       : Stream_Access;
      Data          : Newsgroups_Record;
   begin
      if Exists (File_Name) and not Reload then
         Put_Line (Routine_Name & "restoring from " & File_Name);
         Open (File_ID, In_File, File_Name);
         aStream := Stream (File_ID);
         Newsgroups_Record'Read (aStream, Data);
         Close (File_ID);

      else
         Put_Line (Routine_Name & "reading " & File_Name);
         Data := Python_16A.Call (Classifier, "fetch_newsgroups");
         Save_Data (Data, File_Name);
      end if;

      return Data;

   end Load_Newsgroups;

   --  -------------------------------------------------------------------------

   function Prepare_Embedding_Matrix
     (Embeddings_Index     : Coeffs_Dictionary;
      Word_Index           : Occurrences_Dictionary; Max_Words : Positive;
      Embedding_Dimension  :  Positive) return Embedding_Matrix_Type is
      use Occurrences_Dictionary_Package;
      Num_Words        : constant Positive :=
                           Integer'Min (Max_Words, Integer (Word_Index.Length));
      aKey             : Unbounded_String;
      Word_Count       : Natural := 1;
      Embedding_Vector : NL_Types.Float_List;
      --        Emmbedding_Matrix : array (1 .. Num_Words, 1 .. Emmbedding_Dimension)
      Embedding_Matrix : Embedding_Matrix_Type (1 .. Num_Words);
      Vector_Length    : Positive;
   begin
      --  prepare embedding matrix

      for curs in Word_Index.Iterate loop
         --           Put_Line (Program_Name & "Word_Count" & Integer'Image (Word_Count));
         if Word_Count < Max_Words then
            aKey := Key (curs);
            --              Put_Line (Program_Name & "aKey: " & To_String (aKey));
            if Embeddings_Index.Contains (aKey) then
               Embedding_Vector := Embeddings_Index.Element (aKey);
               if not Embedding_Vector.Is_Empty then
                  Word_Count := Word_Count + 1;
                  Vector_Length := Integer (Embedding_Vector.Length);
                  if Vector_Length > Embedding_Dimension then
                     Embedding_Matrix (Word_Count) :=
                       Slice (Embedding_Vector, 1, Embedding_Dimension);
                  else
                     Embedding_Matrix (Word_Count) := Embedding_Vector;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      return Embedding_Matrix;

   end Prepare_Embedding_Matrix;

   --  -------------------------------------------------------------------------
   --  ProbA_Chooser chooses between B options.
   --  Current_Item is the initial item to consider.
   --  Train_Set represents the results of previous selections.
   --  If alpha selections have not yet made the selection is random.
   --  If alpha selections have been made in the past, fit a clf Naive Bayes
   --  model using the traing data of academic papers by title, trainset and
   --  training labels if the academic papers were interesting, trainlabs.
   --  After fitting the clf model use it to select the item most likely to be
   --  labeled as interesting.
   function ProbA_Chooser
     (Classifier       : Python.Module; Current_Item : Positive;
      Num_Items        : Positive;  --  b
      Labeled_Examples : Data_Items;
      Train_Set        : ML_Types.Integer_List_2D;
      Train_Labels     : ML_Types.Integer_List;
      Alpha            : Integer;  Clf : Python_API.PyObject_Ptr)
      return Integer is
      use System;
      Routine_Name     : constant String := "Support_16.ProbA_Chooser ";
      Train_Set_Length : constant Natural := Integer (Train_Set.Length);
      NIM1             : constant Natural := Num_Items - 1;
      Examples_Batch   : constant Integer_Array_List :=
                           Slice (Labeled_Examples.Features,
                                  Current_Item, Current_Item + NIM1);
      --  Y_Hat predictions
      Y_Hat            : Real_Float_Matrix (1 .. Num_Items, 1 .. 2);
      Indices          : Integer_Array (Y_Hat'Range);
      Y_Hat_2          : Real_Float_Vector (Y_Hat'Range);
      Item             : Integer;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      --  Maximum length of Train_Set is Rounds / B
      if Train_Set_Length = Alpha then
         Python_CLF.Call (Classifier, "fit", Clf, Train_Set, Train_Labels);
      end if;

      if Train_Set_Length < Alpha then
         Item := Maths.Random_Integer (Current_Item, Current_Item + NIM1);
      else  --  Train_Set_Length >= Alpha
         --  predict_proba() returns a Train_Set_Length x two-dimensional array
         --  For binary data, the first column is the probability that the
         --  outcome will be 0 and the second is the probability that the
         --  outcome will be 1, P(0) + P (1) = 1.
         --  The sum of each row of the two columns should equal one.
         Y_Hat := Python_CLF.Call (Classifier, "predict_proba", Clf,
                                   Examples_Batch);
         for index in Indices'Range loop
            Indices (index) := Current_Item + index - 1;
            Y_Hat_2 (index) := Y_Hat (index, 2);
         end loop;
         Item := Arg_Max (Indices, Y_Hat_2);
      end if;

      return Item;

   end ProbA_Chooser;

   --  -------------------------------------------------------------------------

   function Process_Glove_Line (File_ID : File_Type) return Coeffs_Record is
      use Neural_Utilities;
      use ML_Types.String_Package;
      --        Routine_Name  : constant String := "Support_16A.Process_Glove_Line ";
      aLine         : constant String := Get_Line (File_ID);
      Values        : ML_Types.String_List;
      Values_Cursor : Cursor;
      Word          : Unbounded_String;
      Coeffs        : NL_Types.Float_List;
   begin
      Values := Split_String_On_Spaces (aLine);
      Values_Cursor := Values.First;
      Word := Element (Values_Cursor);
      Next (Values_Cursor);

      while Has_Element (Values_Cursor) loop
         Coeffs.Append (Float'Value
                        (To_String (Element ((Values_Cursor)))));
         Next (Values_Cursor);
      end loop;

      return (Word, Coeffs);

   end Process_Glove_Line;

   --  -------------------------------------------------------------------------

   procedure Save_Data (Data : Newsgroups_Record; File_Name : String) is
      use Ada.Streams.Stream_IO;
      Routine_Name : constant String := "Support_16A.Save_Data ";
      File_ID      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream  : Stream_Access;
   begin
      Create (File_ID, Out_File, File_Name);
      Data_Stream := Stream (File_ID);
      Newsgroups_Record'Write (Data_Stream, Data);
      Close (File_ID);
      pragma Unreferenced (File_ID);

      Put_Line (Routine_Name & File_Name & " written");

   end Save_Data;

   --  -------------------------------------------------------------------------

end Support_16A;
