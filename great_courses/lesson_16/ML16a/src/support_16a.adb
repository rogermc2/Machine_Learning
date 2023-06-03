
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
   function Process_Glove_Line (File_ID : File_Type) return Dictionary_Record;
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

   function Get_Glove_Data (File_Name : String) return Dictionary_List is
      Routine_Name : constant String := "Support_16A.Get_Glove_Data ";
      File_ID      : File_Type;
      Num_Lines    : Natural := 0;
      Row          : Natural := 0;
      Embeddings   : Dictionary_List;
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

         Embeddings.Append (Process_Glove_Line (File_ID));
      end loop;
      New_Line;

      Close (File_ID);

      Put_Line (Routine_Name & File_Name & " processed.");
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

   function Process_Glove_Line (File_ID : File_Type) return Dictionary_Record is
      use Neural_Utilities;
      use ML_Types;
      use String_Package;
      --        Routine_Name  : constant String := "Support_16A.Process_Glove_Line ";
      aLine         : constant String := Get_Line (File_ID);
      Values        : ML_Types.String_List;
      Values_Cursor : String_Package.Cursor;
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

   --     function Read_Vocabulary (File_Name : String) return Dictionary_List is
   --        Routine_Name     : constant String := "Support_16A.Read_Vocabulary ";
   --        File_ID          : File_Type;
   --        Lexicon_Size     : Natural := 0;  --  Token
   --        Vocab_Dictionary : Dictionary_List;
   --        Item             : Dictionary_Record;
   --     begin
   --        Item := (Unknown, Lexicon_Size);
   --        Vocab_Dictionary.Append (Item);
   --        Lexicon_Size := Lexicon_Size + 1;
   --
   --        Open (File_ID, In_File, File_Name);
   --
   --        while not End_Of_File (File_ID) loop
   --           declare
   --              aLine : constant Unbounded_String :=
   --                        To_Unbounded_String (Get_Line (File_ID));
   --              Count : constant Positive := Integer'Value (Slice (aLine, 1, 4));
   --              Token : constant Unbounded_String :=
   --                        To_Unbounded_String
   --                          (Slice (aLine, 6, Length (aLine) - 1));
   --           begin
   --              if Count > 1 then
   --                 Item := (Token, Lexicon_Size);
   --                 Vocab_Dictionary.Append (Item);
   --                 Lexicon_Size := Lexicon_Size + 1;
   --              end if;
   --           end;
   --        end loop;
   --
   --        Close (File_ID);
   --
   --        Item :=  (Lex_Size, Lexicon_Size);
   --        Vocab_Dictionary.Append (Item);
   --        Put_Line (Routine_Name & File_Name & " processed.");
   --
   --        return Vocab_Dictionary;
   --
   --     end Read_Vocabulary;

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

   --     function Tokenize (aLine : String; Dictionary : Dictionary_List)
   --                        return Integer_Array is
   --        use Neural_Utilities;
   --        use ML_Types;
   --        use String_Package;
   --        --        Routine_Name : constant String := "Support_16A.Tokenize ";
   --        Words        : ML_Types.String_List;
   --        Word_Cursor  : String_Package.Cursor;
   --        Index        : Natural;
   --        Item         : Dictionary_Record;
   --        Unknown_Item : constant Boolean := Find_Item (Dictionary, Unknown, Item);
   --        Unknown_Val  : constant Integer := Item.Value;
   --        Vec          : Integer_Array (0 .. Positive (Dictionary.Length) - 1) :=
   --                         (others => 0);
   --        Word         : Unbounded_String;
   --     begin
   --        pragma Warnings (Off, Unknown_Item);
   --        Words := Split_String_On_Spaces (aLine);
   --        Word_Cursor := Words.First;
   --        while Has_Element (Word_Cursor) loop
   --           Word := Element (Word_Cursor);
   --           if Find_Item (Dictionary, Word, Item) then
   --              --  the word has a feature so add one to the corresponding feature
   --              Num_Known := Num_Known + 1;
   --              Index := Item.Value;
   --           else
   --              --  add one to the Unknown count
   --              Num_Unknown := Num_Unknown + 1;
   --              Index := Unknown_Val;
   --           end if;
   --
   --           Vec (Index) := Vec (Index) + 1;
   --           Next  (Word_Cursor);
   --        end loop;
   --
   --        return Vec;
   --
   --     end Tokenize;

   --  -------------------------------------------------------------------------

end Support_16A;
